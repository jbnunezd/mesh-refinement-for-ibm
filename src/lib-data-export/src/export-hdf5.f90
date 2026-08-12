!======================================================================================================================!
!
! ARIEN SOLVER
!
! Copyright (c) 2020 by Jonatan Nunez
!
! This program is free software: you can redistribute it and/or modify it under the terms of the GNU 
! General Public License as published by the Free Software Foundation, either version 3 of the License, 
! or (at your option) any later version.
! 
! This program is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even 
! the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.
! See the GNU General Public License for more details.
! 
! You should have received a copy of the GNU General Public License along with this program.
! If not, see <https://www.gnu.org/licenses/>.
!
!======================================================================================================================!
!
!======================================================================================================================!
#include "main.h"
!======================================================================================================================!
!
!======================================================================================================================!
MODULE MOD_DataExport_HDF5
!----------------------------------------------------------------------------------------------------------------------!
USE MOD_GLOBAL_vars
!----------------------------------------------------------------------------------------------------------------------!
IMPLICIT NONE
!----------------------------------------------------------------------------------------------------------------------!
PRIVATE
!----------------------------------------------------------------------------------------------------------------------!
INTEGER,PARAMETER :: ELEMTYPE_EDGE2  = 1
INTEGER,PARAMETER :: ELEMTYPE_TRI3   = 2
INTEGER,PARAMETER :: ELEMTYPE_QUAD4  = 3
INTEGER,PARAMETER :: ELEMTYPE_TETRA4 = 4
INTEGER,PARAMETER :: ELEMTYPE_HEXA8  = 5
INTEGER,PARAMETER :: ELEMTYPE_PRISM6 = 6
INTEGER,PARAMETER :: ELEMTYPE_PYRA5  = 7
!----------------------------------------------------------------------------------------------------------------------!
INTERFACE DataExport_HDF5_CODA
  MODULE PROCEDURE DataExport_HDF5_CODA
END INTERFACE
!----------------------------------------------------------------------------------------------------------------------!
PUBLIC :: DataExport_HDF5_CODA
!----------------------------------------------------------------------------------------------------------------------!
CHARACTER(LEN=256) :: ModuleName = "DataExport_HDF5"
!----------------------------------------------------------------------------------------------------------------------!
!
!
!
!======================================================================================================================!
CONTAINS
!======================================================================================================================!
!
!
!
!======================================================================================================================!
SUBROUTINE DataExport_HDF5_CODA(&
  FileName,&
  ProjectName,&
  ProgramName,&
  FileVersion,&
  nDims,&
  NGeo,&
  OutputTime,&
  VarNames,&
  ElementsToElementType,&
  ElementsToNodes,&
  NodesCoordinates,&
  BCFacesToNodes,&
  BCFacesToElementType,&
  BCFacesToMark,&
  BoundaryMark,&
  BoundaryName,&
  MasterSlavesToNodes_Tri4Tri,&   
  MasterSlavesToNodes_Quad2Quad,& 
  MasterSlavesToNodes_Quad4Quad)
!----------------------------------------------------------------------------------------------------------------------!
USE HDF5
USE MOD_HDF5_Tools
!----------------------------------------------------------------------------------------------------------------------!
IMPLICIT NONE
!----------------------------------------------------------------------------------------------------------------------!
! FORMAL ARGUMENTS
!----------------------------------------------------------------------------------------------------------------------!
CHARACTER(LEN=*),INTENT(IN)   :: FileName
CHARACTER(LEN=*),INTENT(IN)   :: ProjectName
CHARACTER(LEN=*),INTENT(IN)   :: ProgramName
CHARACTER(LEN=*),INTENT(IN)   :: FileVersion
INTEGER,INTENT(IN)            :: nDims
INTEGER,INTENT(IN)            :: NGeo
REAL,INTENT(IN)               :: OutputTime
CHARACTER(LEN=*),INTENT(IN)   :: VarNames(:)
INTEGER,INTENT(IN)            :: ElementsToElementType(:)
INTEGER,INTENT(INOUT)         :: ElementsToNodes(:,:)
REAL,INTENT(IN)               :: NodesCoordinates(:,:)
INTEGER,INTENT(INOUT)         :: BCFacesToNodes(:,:)
INTEGER,INTENT(IN)            :: BCFacesToElementType(:)
INTEGER,INTENT(IN)            :: BCFacesToMark(:)
INTEGER,INTENT(IN)            :: BoundaryMark(:)
CHARACTER(LEN=256),INTENT(IN) :: BoundaryName(:)
INTEGER,INTENT(INOUT),ALLOCATABLE :: MasterSlavesToNodes_Tri4Tri(:,:)
INTEGER,INTENT(INOUT),ALLOCATABLE :: MasterSlavesToNodes_Quad2Quad(:,:)
INTEGER,INTENT(INOUT),ALLOCATABLE :: MasterSlavesToNodes_Quad4Quad(:,:)
!----------------------------------------------------------------------------------------------------------------------!
! LOCAL VARIABLES
!----------------------------------------------------------------------------------------------------------------------!
INTEGER :: nTabIn
INTEGER :: iBC
INTEGER :: ElemID
INTEGER :: FaceID
INTEGER :: nElems
INTEGER :: nNodes
INTEGER :: iBCFace
INTEGER :: iElem
INTEGER :: nBCFaces
INTEGER :: nElemNodes
INTEGER :: nBCFacesNodes
INTEGER :: nTri4Tri
INTEGER :: nQuad2Quad
INTEGER :: nQuad4Quad
INTEGER :: iMappingCellType2Index
INTEGER :: nMappingCellType2Index
CHARACTER(LEN=256) :: iBCStr
!----------------------------------------------------------------------------------------------------------------------!
INTEGER :: ElemType
INTEGER :: nElems_TETRA4
INTEGER :: nElems_HEXA8
INTEGER :: nElems_PRISM6
INTEGER :: nElems_PYRA5
!----------------------------------------------------------------------------------------------------------------------!
INTEGER,ALLOCATABLE :: BCFacesToMarkByType(:)
INTEGER,ALLOCATABLE :: BCFacesToNodesByType(:,:)
INTEGER,ALLOCATABLE :: ElementsToNodesByType(:,:)
!----------------------------------------------------------------------------------------------------------------------!
INTEGER :: nBCFaces_TRI3
INTEGER :: nBCFaces_QUAD4
!----------------------------------------------------------------------------------------------------------------------!
INTEGER(HID_T)      :: file_id
INTEGER(HID_T)      :: mygroup_id
!----------------------------------------------------------------------------------------------------------------------!
INTEGER             :: CODA_NumberOfCellTypes(1:1)
INTEGER             :: CODA_NumberOfCells(1:1)
INTEGER             :: CODA_NumberOfVariables(1:1)
INTEGER             :: CODA_SpansAllCells(1:1)
INTEGER,ALLOCATABLE :: CODA_MappingCellType2Index(:)
INTEGER             :: CODA_Version(1:1)
CHARACTER(LEN=256)  :: CODA_MeshImportFormat
CHARACTER(LEN=256)  :: CODA_FullFileName
CHARACTER(LEN=256)  :: CODA_FileExtension
CHARACTER(LEN=256)  :: CODA_Variable0
CHARACTER(LEN=256)  :: CODA_Variable1
CHARACTER(LEN=256)  :: CODA_Variable2
CHARACTER(LEN=256)  :: CODA_CellType
CHARACTER(LEN=256)  :: CODA_EmptyString
CHARACTER(LEN=256)  :: CODA_NotInitialized
CHARACTER(LEN=256),ALLOCATABLE :: CODA_CADGroupID(:)
!----------------------------------------------------------------------------------------------------------------------!
CHARACTER(LEN=256) :: StrL
CHARACTER(LEN=256) :: StrR
!----------------------------------------------------------------------------------------------------------------------!
CHARACTER(LEN=256) :: ErrorMessage
CHARACTER(LEN=256) :: FunctionName = "DataExport_HDF5_CODA"
!----------------------------------------------------------------------------------------------------------------------!

IF (nDims .NE. 3) THEN
  ErrorMessage = "nDims .NE. 3"
  CALL PrintError(__STAMP__,ErrorMessage,ModuleName,FunctionName)
END IF

IF (NGeo .GT. 1) THEN
  ErrorMessage = "NGeo .GT. 1"
  CALL PrintError(__STAMP__,ErrorMessage,ModuleName,FunctionName)
END IF

nElems   = SIZE(ElementsToNodes,2)
nNodes   = SIZE(NodesCoordinates,2)
nBCFaces = SIZE(BCFacesToNodes,2)

nTri4Tri   = 0
nQuad2Quad = 0
nQuad4Quad = 0

IF (ALLOCATED(MasterSlavesToNodes_Tri4Tri) .EQV. .TRUE.) THEN
  nTri4Tri = SIZE(MasterSlavesToNodes_Tri4Tri,2)
END IF
IF (ALLOCATED(MasterSlavesToNodes_Quad2Quad) .EQV. .TRUE.) THEN
  nQuad2Quad = SIZE(MasterSlavesToNodes_Quad2Quad,2)
END IF
IF (ALLOCATED(MasterSlavesToNodes_Quad4Quad) .EQV. .TRUE.) THEN
  nQuad4Quad = SIZE(MasterSlavesToNodes_Quad4Quad,2)
END IF

ElementsToNodes  = ElementsToNodes-1
BCFacesToNodes   = BCFacesToNodes-1

IF (ALLOCATED(MasterSlavesToNodes_Tri4Tri) .EQV. .TRUE.) THEN
  MasterSlavesToNodes_Tri4Tri = MasterSlavesToNodes_Tri4Tri-1
END IF
IF (ALLOCATED(MasterSlavesToNodes_Quad2Quad) .EQV. .TRUE.) THEN
  MasterSlavesToNodes_Quad2Quad = MasterSlavesToNodes_Quad2Quad-1
END IF
IF (ALLOCATED(MasterSlavesToNodes_Quad4Quad) .EQV. .TRUE.) THEN
  MasterSlavesToNodes_Quad4Quad = MasterSlavesToNodes_Quad4Quad-1
END IF

nBCFaces_TRI3  = 0
nBCFaces_QUAD4 = 0

DO iBCFace=1,SIZE(BCFacesToElementType,1)
  ElemType = BCFacesToElementType(iBCFace)
  SELECT CASE(ElemType)
    CASE(ELEMTYPE_TRI3)
      nBCFaces_TRI3 = nBCFaces_TRI3+1
    CASE(ELEMTYPE_QUAD4)
      nBCFaces_QUAD4 = nBCFaces_QUAD4+1
  END SELECT
END DO

nElems_TETRA4 = 0
nElems_HEXA8  = 0
nElems_PRISM6 = 0
nElems_PYRA5  = 0

DO iElem=1,SIZE(ElementsToElementType,1)
  ElemType = ElementsToElementType(iElem)
  SELECT CASE(ElemType)
    CASE(ELEMTYPE_TETRA4)
      nElems_TETRA4 = nElems_TETRA4+1
    CASE(ELEMTYPE_HEXA8)
      nElems_HEXA8 = nElems_HEXA8+1
    CASE(ELEMTYPE_PRISM6)
      nElems_PRISM6 = nElems_PRISM6+1
    CASE(ELEMTYPE_PYRA5)
      nElems_PYRA5 = nElems_PYRA5+1
  END SELECT
END DO

! Information for CODA files
CODA_FileExtension    = ".h5"
CODA_FullFileName     = TRIM(FileName)//TRIM(CODA_FileExtension)
CODA_EmptyString      = "<no information>"
CODA_NotInitialized   = "<not initialized>"
CODA_MeshImportFormat = "HDF5"
CODA_Version          = 1

nTabIn = 4
StrL = "Writing Mesh"
StrR = TRIM(CODA_FullFileName)
CALL PrintAnalyze(StrL,StrR,nTabIn=nTabIn)

CALL HDF5_OpenFile(file_id,CODA_FullFileName,status="NEW",action="WRITE")

CALL HDF5_CreateGroup(file_id,"FS:Mesh")
CALL HDF5_CreateGroup(file_id,"FS:Mesh/UnstructuredCells")
CALL HDF5_CreateGroup(file_id,"FS:Mesh/UnstructuredCells/Datasets")
CALL HDF5_CreateGroup(file_id,"FS:Mesh/UnstructuredCells/Datasets/Coordinates")
CALL HDF5_CreateGroup(file_id,"FS:Mesh/UnstructuredCells/Datasets/Coordinates/CellType0")
CALL HDF5_CreateGroup(file_id,"FS:Mesh/UnstructuredCells/Datasets/Coordinates/Variable0")
CALL HDF5_CreateGroup(file_id,"FS:Mesh/UnstructuredCells/Datasets/Coordinates/Variable0/DataSpecification")
CALL HDF5_CreateGroup(file_id,"FS:Mesh/UnstructuredCells/Datasets/Coordinates/Variable1")
CALL HDF5_CreateGroup(file_id,"FS:Mesh/UnstructuredCells/Datasets/Coordinates/Variable1/DataSpecification")
CALL HDF5_CreateGroup(file_id,"FS:Mesh/UnstructuredCells/Datasets/Coordinates/Variable2")
CALL HDF5_CreateGroup(file_id,"FS:Mesh/UnstructuredCells/Datasets/Coordinates/Variable2/DataSpecification")
CALL HDF5_CreateGroup(file_id,"FS:Mesh/UnstructuredCells/MappingCellType2Index")
CALL HDF5_CreateGroup(file_id,"FS:Mesh/UnstructuredCells/Node")

IF (nBCFaces_TRI3 .GT. 0) THEN
  CALL HDF5_CreateGroup(file_id,"FS:Mesh/UnstructuredCells/Tri3")
  CALL HDF5_CreateGroup(file_id,"FS:Mesh/UnstructuredCells/Tri3/CellAttributes")
END IF

IF (nBCFaces_QUAD4 .GT. 0) THEN
  CALL HDF5_CreateGroup(file_id,"FS:Mesh/UnstructuredCells/Quad4")
  CALL HDF5_CreateGroup(file_id,"FS:Mesh/UnstructuredCells/Quad4/CellAttributes")
END IF

IF (nElems_TETRA4 .GT. 0) THEN
  CALL HDF5_CreateGroup(file_id,"FS:Mesh/UnstructuredCells/Tetra4")
END IF

IF (nElems_HEXA8 .GT. 0) THEN
  CALL HDF5_CreateGroup(file_id,"FS:Mesh/UnstructuredCells/Hexa8")
END IF

IF (nElems_PRISM6 .GT. 0) THEN
  CALL HDF5_CreateGroup(file_id,"FS:Mesh/UnstructuredCells/Prism6")
END IF

IF (nElems_PYRA5 .GT. 0) THEN
  CALL HDF5_CreateGroup(file_id,"FS:Mesh/UnstructuredCells/Pyra5")
END IF

IF (nTri4Tri .GT. 0) THEN
  CALL HDF5_CreateGroup(file_id,"FS:Mesh/UnstructuredCells/Tri4Tri")
END IF

IF (nQuad2Quad .GT. 0) THEN
  CALL HDF5_CreateGroup(file_id,"FS:Mesh/UnstructuredCells/Quad2Quad")
END IF

IF (nQuad4Quad .GT. 0) THEN
  CALL HDF5_CreateGroup(file_id,"FS:Mesh/UnstructuredCells/Quad4Quad")
END IF

CALL HDF5_CreateGroup(file_id,"FS:Mesh/UnstructuredCells/NamesOfCellAttributeValues")
CALL HDF5_CreateGroup(file_id,"FS:Mesh/UnstructuredCells/NamesOfCellAttributeValues/CADGroupID")

CALL HDF5_OpenGroup(file_id,"FS:Mesh",mygroup_id)
CALL HDF5_WriteAttribute(mygroup_id,"","MeshFilename",CODA_FullFileName)
CALL HDF5_WriteAttribute(mygroup_id,"","MeshImportFormat",CODA_MeshImportFormat)
CALL HDF5_WriteAttribute(mygroup_id,"","Version",CODA_Version)
CALL HDF5_CloseGroup(mygroup_id)

! Information for CODA files
CODA_NumberOfCellTypes = 1
CODA_NumberOfCells     = nNodes
CODA_NumberOfVariables = 3
CODA_SpansAllCells     = 1

CALL HDF5_OpenGroup(file_id,"FS:Mesh/UnstructuredCells/Datasets/Coordinates",mygroup_id)
CALL HDF5_WriteAttribute(mygroup_id,"","NumberOfCellTypes",CODA_NumberOfCellTypes)
CALL HDF5_WriteAttribute(mygroup_id,"","NumberOfCells",CODA_NumberOfCells)
CALL HDF5_WriteAttribute(mygroup_id,"","NumberOfVariables",CODA_NumberOfVariables)
CALL HDF5_WriteAttribute(mygroup_id,"","SpansAllCells",CODA_SpansAllCells)
CALL HDF5_WriteDataSet(mygroup_id,"Values",NodesCoordinates)
CALL HDF5_CloseGroup(mygroup_id)

! Information for CODA files
CODA_CellType  = "Node"
CODA_Variable0 = VarNames(1)
CODA_Variable1 = VarNames(2)
CODA_Variable2 = VarNames(3)

CALL HDF5_OpenGroup(file_id,"FS:Mesh/UnstructuredCells/Datasets/Coordinates/CellType0",mygroup_id)
CALL HDF5_WriteAttribute(mygroup_id,"","Name",CODA_CellType)
CALL HDF5_CloseGroup(mygroup_id)

CALL HDF5_OpenGroup(file_id,"FS:Mesh/UnstructuredCells/Datasets/Coordinates/Variable0",mygroup_id)
CALL HDF5_WriteAttribute(mygroup_id,"","Name",CODA_Variable0)
CALL HDF5_CloseGroup(mygroup_id)

CALL HDF5_OpenGroup(file_id,"FS:Mesh/UnstructuredCells/Datasets/Coordinates/Variable0/DataSpecification",mygroup_id)
CALL HDF5_WriteAttribute(mygroup_id,"","InfoString",CODA_EmptyString)
CALL HDF5_WriteAttribute(mygroup_id,"","Type",CODA_NotInitialized)
CALL HDF5_CloseGroup(mygroup_id)

CALL HDF5_OpenGroup(file_id,"FS:Mesh/UnstructuredCells/Datasets/Coordinates/Variable1",mygroup_id)
CALL HDF5_WriteAttribute(mygroup_id,"","Name",CODA_Variable1)
CALL HDF5_CloseGroup(mygroup_id)

CALL HDF5_OpenGroup(file_id,"FS:Mesh/UnstructuredCells/Datasets/Coordinates/Variable1/DataSpecification",mygroup_id)
CALL HDF5_WriteAttribute(mygroup_id,"","InfoString",CODA_EmptyString)
CALL HDF5_WriteAttribute(mygroup_id,"","Type",CODA_NotInitialized)
CALL HDF5_CloseGroup(mygroup_id)

CALL HDF5_OpenGroup(file_id,"FS:Mesh/UnstructuredCells/Datasets/Coordinates/Variable2",mygroup_id)
CALL HDF5_WriteAttribute(mygroup_id,"","Name",CODA_Variable2)
CALL HDF5_CloseGroup(mygroup_id)

CALL HDF5_OpenGroup(file_id,"FS:Mesh/UnstructuredCells/Datasets/Coordinates/Variable2/DataSpecification",mygroup_id)
CALL HDF5_WriteAttribute(mygroup_id,"","InfoString",CODA_EmptyString)
CALL HDF5_WriteAttribute(mygroup_id,"","Type",CODA_NotInitialized)
CALL HDF5_CloseGroup(mygroup_id)

! Information for CODA files

nMappingCellType2Index = 1
IF (nBCFaces_TRI3 .GT. 0) THEN
  nMappingCellType2Index = nMappingCellType2Index+1
END IF

IF (nBCFaces_QUAD4 .GT. 0) THEN
  nMappingCellType2Index = nMappingCellType2Index+1
END IF

IF (nElems_TETRA4 .GT. 0) THEN
  nMappingCellType2Index = nMappingCellType2Index+1
END IF

IF (nElems_HEXA8 .GT. 0) THEN
  nMappingCellType2Index = nMappingCellType2Index+1
END IF

IF (nElems_PRISM6 .GT. 0) THEN
  nMappingCellType2Index = nMappingCellType2Index+1
END IF

IF (nElems_PYRA5 .GT. 0) THEN
  nMappingCellType2Index = nMappingCellType2Index+1
END IF

IF (nTri4Tri .GT. 0) THEN
  nMappingCellType2Index = nMappingCellType2Index+1
END IF

IF (nQuad2Quad .GT. 0) THEN
  nMappingCellType2Index = nMappingCellType2Index+1
END IF

IF (nQuad4Quad .GT. 0) THEN
  nMappingCellType2Index = nMappingCellType2Index+1
END IF

IF (ALLOCATED(CODA_MappingCellType2Index)) THEN
  DEALLOCATE(CODA_MappingCellType2Index)
END IF
ALLOCATE(CODA_MappingCellType2Index(1:nMappingCellType2Index))

DO iMappingCellType2Index=1,nMappingCellType2Index
  CODA_MappingCellType2Index(iMappingCellType2Index) = iMappingCellType2Index-1
END DO

CALL HDF5_OpenGroup(file_id,"FS:Mesh/UnstructuredCells/MappingCellType2Index",mygroup_id)

iMappingCellType2Index = 1
CALL HDF5_WriteAttribute(mygroup_id,"","Node",(/CODA_MappingCellType2Index(iMappingCellType2Index)/))

IF (nBCFaces_TRI3 .GT. 0) THEN
  iMappingCellType2Index = iMappingCellType2Index+1
  CALL HDF5_WriteAttribute(mygroup_id,"","Tri3",(/CODA_MappingCellType2Index(iMappingCellType2Index)/))
END IF

IF (nBCFaces_QUAD4 .GT. 0) THEN
  iMappingCellType2Index = iMappingCellType2Index+1
  CALL HDF5_WriteAttribute(mygroup_id,"","Quad4",(/CODA_MappingCellType2Index(iMappingCellType2Index)/))
END IF

IF (nElems_TETRA4 .GT. 0) THEN
  iMappingCellType2Index = iMappingCellType2Index+1
  CALL HDF5_WriteAttribute(mygroup_id,"","Tetra4",(/CODA_MappingCellType2Index(iMappingCellType2Index)/))
END IF

IF (nElems_HEXA8 .GT. 0) THEN
  iMappingCellType2Index = iMappingCellType2Index+1
  CALL HDF5_WriteAttribute(mygroup_id,"","Hexa8",(/CODA_MappingCellType2Index(iMappingCellType2Index)/))
END IF

IF (nElems_PRISM6 .GT. 0) THEN
  iMappingCellType2Index = iMappingCellType2Index+1
  CALL HDF5_WriteAttribute(mygroup_id,"","Prism6",(/CODA_MappingCellType2Index(iMappingCellType2Index)/))
END IF

IF (nElems_PYRA5 .GT. 0) THEN
  iMappingCellType2Index = iMappingCellType2Index+1
  CALL HDF5_WriteAttribute(mygroup_id,"","Pyra5",(/CODA_MappingCellType2Index(iMappingCellType2Index)/))
END IF

IF (nTri4Tri .GT. 0) THEN
  iMappingCellType2Index = iMappingCellType2Index+1
  CALL HDF5_WriteAttribute(mygroup_id,"","Tri4Tri",(/CODA_MappingCellType2Index(iMappingCellType2Index)/))
END IF

IF (nQuad2Quad .GT. 0) THEN
  iMappingCellType2Index = iMappingCellType2Index+1
  CALL HDF5_WriteAttribute(mygroup_id,"","Quad2Quad",(/CODA_MappingCellType2Index(iMappingCellType2Index)/))
END IF

IF (nQuad4Quad .GT. 0) THEN
  iMappingCellType2Index = iMappingCellType2Index+1
  CALL HDF5_WriteAttribute(mygroup_id,"","Quad4Quad",(/CODA_MappingCellType2Index(iMappingCellType2Index)/))
END IF

CALL HDF5_CloseGroup(mygroup_id)

! Information for CODA files
IF (ALLOCATED(CODA_CADGroupID)) THEN
  DEALLOCATE(CODA_CADGroupID)
END IF
ALLOCATE(CODA_CADGroupID(1:SIZE(BoundaryMark)))

! WARNING
! ! ! DO iBC=1,SIZE(BoundaryMark)
! ! !   WRITE(CODA_CADGroupID(iBC),"(I0)") BoundaryMark(iBC)
! ! ! END DO
DO iBC=1,SIZE(BoundaryMark)
  CODA_CADGroupID(iBC) = TRIM(BoundaryName(iBC))
! ! !   WRITE(CODA_CADGroupID(iBC),"(I0)") BoundaryMark(iBC)
END DO
! WARNING

CALL HDF5_OpenGroup(file_id,"FS:Mesh/UnstructuredCells/NamesOfCellAttributeValues/CADGroupID",mygroup_id)
DO iBC=1,SIZE(BoundaryMark)
  WRITE(iBCStr,"(I0)") iBC
  CALL HDF5_WriteAttribute(mygroup_id,"",TRIM(iBCStr),CODA_CADGroupID(iBC))
END DO
CALL HDF5_CloseGroup(mygroup_id)

! Information for CODA files
CODA_NumberOfCells = nNodes

CALL HDF5_OpenGroup(file_id,"FS:Mesh/UnstructuredCells/Node",mygroup_id)
CALL HDF5_WriteAttribute(mygroup_id,"","NumberOfCells",CODA_NumberOfCells)
CALL HDF5_CloseGroup(mygroup_id)

! Information for CODA files
IF (nBCFaces_TRI3 .GT. 0) THEN
  nBCFacesNodes = 3
  IF (ALLOCATED(BCFacesToNodesByType)) THEN
    DEALLOCATE(BCFacesToNodesByType)
  END IF
  ALLOCATE(BCFacesToNodesByType(1:nBCFacesNodes,1:nBCFaces_TRI3))
  IF (ALLOCATED(BCFacesToMarkByType)) THEN
    DEALLOCATE(BCFacesToMarkByType)
  END IF
  ALLOCATE(BCFacesToMarkByType(1:nBCFaces_TRI3))
  
  FaceID = 0
  DO iBCFace=1,SIZE(BCFacesToNodes,2)
    IF (BCFacesToElementType(iBCFace) .EQ. ELEMTYPE_TRI3) THEN
      FaceID = FaceID+1
      BCFacesToNodesByType(1:nBCFacesNodes,FaceID) = BCFacesToNodes(1:nBCFacesNodes,iBCFace)
      BCFacesToMarkByType(FaceID) = BCFacesToMark(iBCFace)
    END IF
  END DO

  CODA_NumberOfCells = nBCFaces_TRI3
  CALL HDF5_OpenGroup(file_id,"FS:Mesh/UnstructuredCells/Tri3",mygroup_id)
  CALL HDF5_WriteAttribute(mygroup_id,"","NumberOfCells",CODA_NumberOfCells)
  CALL HDF5_WriteDataSet(mygroup_id,"Cell2Node",BCFacesToNodesByType)
  CALL HDF5_CloseGroup(mygroup_id)

  CALL HDF5_OpenGroup(file_id,"FS:Mesh/UnstructuredCells/Tri3/CellAttributes",mygroup_id)
  CALL HDF5_WriteDataSet(mygroup_id,"CADGroupID",BCFacesToMarkByType)
  CALL HDF5_CloseGroup(mygroup_id)
END IF

IF (nBCFaces_QUAD4 .GT. 0) THEN
  nBCFacesNodes = 4
  IF (ALLOCATED(BCFacesToNodesByType)) THEN
    DEALLOCATE(BCFacesToNodesByType)
  END IF
  ALLOCATE(BCFacesToNodesByType(1:nBCFacesNodes,1:nBCFaces_QUAD4))
  IF (ALLOCATED(BCFacesToMarkByType)) THEN
    DEALLOCATE(BCFacesToMarkByType)
  END IF
  ALLOCATE(BCFacesToMarkByType(1:nBCFaces_QUAD4))
  
  FaceID = 0
  DO iBCFace=1,SIZE(BCFacesToNodes,2)
    IF (BCFacesToElementType(iBCFace) .EQ. ELEMTYPE_QUAD4) THEN
      FaceID = FaceID+1
      BCFacesToNodesByType(1:nBCFacesNodes,FaceID) = BCFacesToNodes(1:nBCFacesNodes,iBCFace)
      BCFacesToMarkByType(FaceID) = BCFacesToMark(iBCFace)
    END IF
  END DO
  
  CODA_NumberOfCells = nBCFaces_QUAD4
  CALL HDF5_OpenGroup(file_id,"FS:Mesh/UnstructuredCells/Quad4",mygroup_id)
  CALL HDF5_WriteAttribute(mygroup_id,"","NumberOfCells",CODA_NumberOfCells)
  CALL HDF5_WriteDataSet(mygroup_id,"Cell2Node",BCFacesToNodesByType)
  CALL HDF5_CloseGroup(mygroup_id)

  CALL HDF5_OpenGroup(file_id,"FS:Mesh/UnstructuredCells/Quad4/CellAttributes",mygroup_id)
  CALL HDF5_WriteDataSet(mygroup_id,"CADGroupID",BCFacesToMarkByType)
  CALL HDF5_CloseGroup(mygroup_id)
END IF

! Information for CODA files
IF (nElems_TETRA4 .GT. 0) THEN
  nElemNodes = 4
  IF (ALLOCATED(ElementsToNodesByType)) THEN
    DEALLOCATE(ElementsToNodesByType)
  END IF
  ALLOCATE(ElementsToNodesByType(1:nElemNodes,1:nElems_TETRA4))
  
  ElemID = 0
  DO iElem=1,SIZE(ElementsToNodes,2)
    IF (ElementsToElementType(iElem) .EQ. ELEMTYPE_TETRA4) THEN
      ElemID = ElemID+1
      ElementsToNodesByType(1:nElemNodes,ElemID) = ElementsToNodes(1:nElemNodes,iElem)
    END IF
  END DO

  CODA_NumberOfCells = nElems_TETRA4
  CALL HDF5_OpenGroup(file_id,"FS:Mesh/UnstructuredCells/Tetra4",mygroup_id)
  CALL HDF5_WriteAttribute(mygroup_id,"","NumberOfCells",CODA_NumberOfCells)
  CALL HDF5_WriteDataSet(mygroup_id,"Cell2Node",ElementsToNodesByType)
  CALL HDF5_CloseGroup(mygroup_id)
END IF

IF (nElems_HEXA8 .GT. 0) THEN
  nElemNodes = 8
  IF (ALLOCATED(ElementsToNodesByType)) THEN
    DEALLOCATE(ElementsToNodesByType)
  END IF
  ALLOCATE(ElementsToNodesByType(1:nElemNodes,1:nElems_HEXA8))
  
  ElemID = 0
  DO iElem=1,SIZE(ElementsToNodes,2)
    IF (ElementsToElementType(iElem) .EQ. ELEMTYPE_HEXA8) THEN
      ElemID = ElemID+1
      ElementsToNodesByType(1:nElemNodes,ElemID) = ElementsToNodes(1:nElemNodes,iElem)
    END IF
  END DO

  CODA_NumberOfCells = nElems_HEXA8
  CALL HDF5_OpenGroup(file_id,"FS:Mesh/UnstructuredCells/Hexa8",mygroup_id)
  CALL HDF5_WriteAttribute(mygroup_id,"","NumberOfCells",CODA_NumberOfCells)
  CALL HDF5_WriteDataSet(mygroup_id,"Cell2Node",ElementsToNodesByType)
  CALL HDF5_CloseGroup(mygroup_id)
END IF

IF (nElems_PRISM6 .GT. 0) THEN
  nElemNodes = 6
  IF (ALLOCATED(ElementsToNodesByType)) THEN
    DEALLOCATE(ElementsToNodesByType)
  END IF
  ALLOCATE(ElementsToNodesByType(1:nElemNodes,1:nElems_PRISM6))
  
  ElemID = 0
  DO iElem=1,SIZE(ElementsToNodes,2)
    IF (ElementsToElementType(iElem) .EQ. ELEMTYPE_PRISM6) THEN
      ElemID = ElemID+1
      ElementsToNodesByType(1:nElemNodes,ElemID) = ElementsToNodes(1:nElemNodes,iElem)
    END IF
  END DO

  CODA_NumberOfCells = nElems_PRISM6
  CALL HDF5_OpenGroup(file_id,"FS:Mesh/UnstructuredCells/Prism6",mygroup_id)
  CALL HDF5_WriteAttribute(mygroup_id,"","NumberOfCells",CODA_NumberOfCells)
  CALL HDF5_WriteDataSet(mygroup_id,"Cell2Node",ElementsToNodesByType)
  CALL HDF5_CloseGroup(mygroup_id)
END IF

IF (nElems_PYRA5 .GT. 0) THEN
  nElemNodes = 5
  IF (ALLOCATED(ElementsToNodesByType)) THEN
    DEALLOCATE(ElementsToNodesByType)
  END IF
  ALLOCATE(ElementsToNodesByType(1:nElemNodes,1:nElems_PYRA5))
  
  ElemID = 0
  DO iElem=1,SIZE(ElementsToNodes,2)
    IF (ElementsToElementType(iElem) .EQ. ELEMTYPE_PYRA5) THEN
      ElemID = ElemID+1
      ElementsToNodesByType(1:nElemNodes,ElemID) = ElementsToNodes(1:nElemNodes,iElem)
    END IF
  END DO

  CODA_NumberOfCells = nElems_PYRA5
  CALL HDF5_OpenGroup(file_id,"FS:Mesh/UnstructuredCells/Pyra5",mygroup_id)
  CALL HDF5_WriteAttribute(mygroup_id,"","NumberOfCells",CODA_NumberOfCells)
  CALL HDF5_WriteDataSet(mygroup_id,"Cell2Node",ElementsToNodesByType)
  CALL HDF5_CloseGroup(mygroup_id)
END IF

IF (nTri4Tri .GT. 0) THEN
  CODA_NumberOfCells = nTri4Tri
  CALL HDF5_OpenGroup(file_id,"FS:Mesh/UnstructuredCells/Tri4Tri",mygroup_id)
  CALL HDF5_WriteAttribute(mygroup_id,"","NumberOfCells",CODA_NumberOfCells)
  CALL HDF5_WriteDataSet(mygroup_id,"Cell2Node",MasterSlavesToNodes_Tri4Tri)
  CALL HDF5_CloseGroup(mygroup_id)
END IF

IF (nQuad2Quad .GT. 0) THEN
  CODA_NumberOfCells = nQuad2Quad
  CALL HDF5_OpenGroup(file_id,"FS:Mesh/UnstructuredCells/Quad2Quad",mygroup_id)
  CALL HDF5_WriteAttribute(mygroup_id,"","NumberOfCells",CODA_NumberOfCells)
  CALL HDF5_WriteDataSet(mygroup_id,"Cell2Node",MasterSlavesToNodes_Quad2Quad)
  CALL HDF5_CloseGroup(mygroup_id)
END IF

IF (nQuad4Quad .GT. 0) THEN
  CODA_NumberOfCells = nQuad4Quad
  CALL HDF5_OpenGroup(file_id,"FS:Mesh/UnstructuredCells/Quad4Quad",mygroup_id)
  CALL HDF5_WriteAttribute(mygroup_id,"","NumberOfCells",CODA_NumberOfCells)
  CALL HDF5_WriteDataSet(mygroup_id,"Cell2Node",MasterSlavesToNodes_Quad4Quad)
  CALL HDF5_CloseGroup(mygroup_id)
END IF

CALL HDF5_CloseFile(file_id)

ElementsToNodes  = ElementsToNodes+1
BCFacesToNodes   = BCFacesToNodes+1

IF (nTri4Tri .GT. 0) THEN
  MasterSlavesToNodes_Tri4Tri = MasterSlavesToNodes_Tri4Tri+1
END IF

IF (nQuad2Quad .GT. 0) THEN
  MasterSlavesToNodes_Quad2Quad = MasterSlavesToNodes_Quad2Quad+1
END IF

IF (nQuad4Quad .GT. 0) THEN
  MasterSlavesToNodes_Quad4Quad = MasterSlavesToNodes_Quad4Quad+1
END IF

!----------------------------------------------------------------------------------------------------------------------!
END SUBROUTINE DataExport_HDF5_CODA
!======================================================================================================================!
!
!
!----------------------------------------------------------------------------------------------------------------------!
END MODULE MOD_DataExport_HDF5
!======================================================================================================================!
