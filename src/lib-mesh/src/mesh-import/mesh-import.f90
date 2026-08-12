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
MODULE MOD_MeshImport
!----------------------------------------------------------------------------------------------------------------------!
USE MOD_GLOBAL_vars
!----------------------------------------------------------------------------------------------------------------------!
IMPLICIT NONE
!----------------------------------------------------------------------------------------------------------------------!
PRIVATE
!----------------------------------------------------------------------------------------------------------------------!
INTERFACE InitializeMeshImport
  MODULE PROCEDURE InitializeMeshImport
END INTERFACE

INTERFACE MeshImport
  MODULE PROCEDURE MeshImport
END INTERFACE
!----------------------------------------------------------------------------------------------------------------------!
PUBLIC :: InitializeMeshImport
PUBLIC :: MeshImport
!----------------------------------------------------------------------------------------------------------------------!
CHARACTER(LEN=256) :: ModuleName = "MeshImport"
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
SUBROUTINE InitializeMeshImport()
!----------------------------------------------------------------------------------------------------------------------!
USE MOD_ConfigFilesTools
!----------------------------------------------------------------------------------------------------------------------!
USE MOD_MeshMain_vars,ONLY: PP_NGeo
USE MOD_MeshMain_vars,ONLY: MeshData_BoundaryName
USE MOD_MeshMain_vars,ONLY: MeshData_BoundaryMark
!----------------------------------------------------------------------------------------------------------------------!
USE MOD_MeshImport_vars,ONLY: RotateMesh
USE MOD_MeshImport_vars,ONLY: RotationAngle3D
USE MOD_MeshImport_vars,ONLY: RotationMatrix3D
!----------------------------------------------------------------------------------------------------------------------!
USE MOD_MeshImport_vars,ONLY: MeshImport_GMSH
USE MOD_MeshImport_vars,ONLY: ParametersMeshImport
USE MOD_MeshImport_vars,ONLY: InitializeMeshImportIsDone
!----------------------------------------------------------------------------------------------------------------------!
USE MOD_DataImport_GMSH,ONLY: DataImport_GMSH
!----------------------------------------------------------------------------------------------------------------------!
IMPLICIT NONE
!----------------------------------------------------------------------------------------------------------------------!
! FORMAL ARGUMENTS
!----------------------------------------------------------------------------------------------------------------------!
! LOCAL VARIABLES
!----------------------------------------------------------------------------------------------------------------------!
INTEGER :: ii
INTEGER :: nBoundaries
!----------------------------------------------------------------------------------------------------------------------!
CHARACTER(LEN=256) :: Header
CHARACTER(LEN=256) :: ErrorMessage
CHARACTER(LEN=256) :: FunctionName = "InitializeMeshImport"
!----------------------------------------------------------------------------------------------------------------------!

IF (InitializeMeshImportIsDone) THEN
  SWRITE(UNIT_SCREEN,*) "InitializeMesh not ready to be called or already called."
  RETURN
END IF

Header = "INITIALIZING READ-IN MESH MODULE..."
CALL PrintHeader(Header)

PP_NGeo = GetInteger('NGeo','1')

ParametersMeshImport%InputMeshFolder         = GetString('InputMeshFolder')
ParametersMeshImport%InputMeshFile           = GetString('InputMeshFile')
ParametersMeshImport%InputMeshFileFormat     = GetString('InputMeshFileFormat')
ParametersMeshImport%DebugMeshImport         = GetLogical('DebugMeshImport','.FALSE.')
ParametersMeshImport%RotateMesh              = GetLogical('RotateMesh','.FALSE.')
ParametersMeshImport%RotationAngle3D         = GetRealArray('RotationAngle3D',3,'0.0,0.0,0.0')

! Boundary Conditions
nBoundaries = CountStrings('BoundaryMark',0)
IF (nBoundaries .GT. 0) THEN
  ALLOCATE(ParametersMeshImport%BoundaryName(1:nBoundaries))
  ALLOCATE(ParametersMeshImport%BoundaryMark(1:nBoundaries))
  DO ii=1,nBoundaries
    ParametersMeshImport%BoundaryName(ii) = GetString('BoundaryName')
    ParametersMeshImport%BoundaryMark(ii) = GetInteger('BoundaryMark')
  END DO
ELSE
  ErrorMessage = "Wrong number of BoundaryMark and BoundaryName"
  CALL PrintError(__STAMP__,ErrorMessage,ModuleName,FunctionName)
END IF

ALLOCATE(MeshData_BoundaryName(1:nBoundaries))
ALLOCATE(MeshData_BoundaryMark(1:nBoundaries))

MeshData_BoundaryName = ParametersMeshImport%BoundaryName
MeshData_BoundaryMark = ParametersMeshImport%BoundaryMark

SELECT CASE(PP_nDims)
  CASE(2)
    MeshImport_GMSH => DataImport_GMSH
  CASE(3)
    MeshImport_GMSH => DataImport_GMSH
  CASE DEFAULT
  ErrorMessage = "Wrong value of Mesh Dimension"
  CALL PrintError(__STAMP__,ErrorMessage,ModuleName,FunctionName)
END SELECT

RotateMesh      = ParametersMeshImport%RotateMesh
RotationAngle3D = ParametersMeshImport%RotationAngle3D

ALLOCATE(RotationMatrix3D(1:3,1:3,1:3))

RotationAngle3D(1:3) = ACOS(-1.0)*RotationAngle3D/180.0
RotationMatrix3D = 0.0

RotationMatrix3D(1,1,1) = 1.0
RotationMatrix3D(2,2,1) = COS(RotationAngle3D(1))
RotationMatrix3D(2,3,1) =-SIN(RotationAngle3D(1))
RotationMatrix3D(3,2,1) = SIN(RotationAngle3D(1))
RotationMatrix3D(3,3,1) = COS(RotationAngle3D(1))

RotationMatrix3D(2,2,2) = 1.0
RotationMatrix3D(1,1,2) = COS(RotationAngle3D(2))
RotationMatrix3D(1,3,2) = SIN(RotationAngle3D(2))
RotationMatrix3D(3,1,2) =-SIN(RotationAngle3D(2))
RotationMatrix3D(3,3,2) = COS(RotationAngle3D(2))

RotationMatrix3D(3,3,3) = 1.0
RotationMatrix3D(1,1,3) = COS(RotationAngle3D(3))
RotationMatrix3D(1,2,3) =-SIN(RotationAngle3D(3))
RotationMatrix3D(2,1,3) = SIN(RotationAngle3D(3))
RotationMatrix3D(2,2,3) = COS(RotationAngle3D(3))

InitializeMeshImportIsDone = .TRUE.

!----------------------------------------------------------------------------------------------------------------------!
END SUBROUTINE InitializeMeshImport
!======================================================================================================================!
!
!
!
!======================================================================================================================!
SUBROUTINE MeshImport()
!----------------------------------------------------------------------------------------------------------------------!
USE MOD_DataStructures,ONLY: LowerCase
!----------------------------------------------------------------------------------------------------------------------!
USE MOD_MeshImport_vars,ONLY: RotateMesh
USE MOD_MeshImport_vars,ONLY: RotationMatrix3D
!----------------------------------------------------------------------------------------------------------------------!
USE MOD_MeshImport_vars,ONLY: MeshImport_GMSH
USE MOD_MeshImport_vars,ONLY: ParametersMeshImport
!----------------------------------------------------------------------------------------------------------------------!
USE MOD_MeshMain_vars,ONLY: MeshInfo
USE MOD_MeshMain_vars,ONLY: ExtrusionDirection
USE MOD_MeshMain_vars,ONLY: MeshData_ElementsToElementType
USE MOD_MeshMain_vars,ONLY: MeshData_ElementsToNodes
USE MOD_MeshMain_vars,ONLY: MeshData_NodesCoordinates
USE MOD_MeshMain_vars,ONLY: MeshData_BCFacesToMark
USE MOD_MeshMain_vars,ONLY: MeshData_BCFacesToNodes
USE MOD_MeshMain_vars,ONLY: MeshData_BCFacesToElementType
USE MOD_MeshMain_vars,ONLY: MeshData_BoundaryName
USE MOD_MeshMain_vars,ONLY: MeshData_BoundaryMark
USE MOD_MeshMain_vars,ONLY: MeshData_ElementsToNodesCoordinates
!----------------------------------------------------------------------------------------------------------------------!
USE MOD_Mesh_CGNS_Definitions,ONLY: ELEMTYPE_EDGE2
USE MOD_Mesh_CGNS_Definitions,ONLY: ELEMTYPE_TRI3
USE MOD_Mesh_CGNS_Definitions,ONLY: ELEMTYPE_QUAD4
USE MOD_Mesh_CGNS_Definitions,ONLY: ELEMTYPE_TETRA4
USE MOD_Mesh_CGNS_Definitions,ONLY: ELEMTYPE_HEXA8
USE MOD_Mesh_CGNS_Definitions,ONLY: ELEMTYPE_PRISM6
USE MOD_Mesh_CGNS_Definitions,ONLY: ELEMTYPE_PYRA5
USE MOD_Mesh_CGNS_Definitions,ONLY: CGNS_ElemToNumberOfNodes
USE MOD_Mesh_CGNS_Definitions,ONLY: CGNS_ElemToNumberOfFaces
USE MOD_Mesh_CGNS_Definitions,ONLY: CGNS_FaceToNodes_EDGE2
USE MOD_Mesh_CGNS_Definitions,ONLY: CGNS_FaceToNodes_TRI3
USE MOD_Mesh_CGNS_Definitions,ONLY: CGNS_FaceToNodes_QUAD4
USE MOD_Mesh_CGNS_Definitions,ONLY: CGNS_FaceToNodes_TETRA4
USE MOD_Mesh_CGNS_Definitions,ONLY: CGNS_FaceToNodes_HEXA8
USE MOD_Mesh_CGNS_Definitions,ONLY: CGNS_FaceToNodes_PRISM6
USE MOD_Mesh_CGNS_Definitions,ONLY: CGNS_FaceToNodes_PYRAM5
USE MOD_Mesh_CGNS_Definitions,ONLY: CGNS_NodesPerFace_EDGE2
USE MOD_Mesh_CGNS_Definitions,ONLY: CGNS_NodesPerFace_TRI3
USE MOD_Mesh_CGNS_Definitions,ONLY: CGNS_NodesPerFace_QUAD4
USE MOD_Mesh_CGNS_Definitions,ONLY: CGNS_NodesPerFace_TETRA4
USE MOD_Mesh_CGNS_Definitions,ONLY: CGNS_NodesPerFace_HEXA8
USE MOD_Mesh_CGNS_Definitions,ONLY: CGNS_NodesPerFace_PRISM6
USE MOD_Mesh_CGNS_Definitions,ONLY: CGNS_NodesPerFace_PYRAM5
USE MOD_Mesh_CGNS_Definitions,ONLY: GetnElemNodes
USE MOD_Mesh_CGNS_Definitions,ONLY: GetnElemFaces
USE MOD_Mesh_CGNS_Definitions,ONLY: GetnNodesOnFace
USE MOD_Mesh_CGNS_Definitions,ONLY: GetNodesOnFace
!----------------------------------------------------------------------------------------------------------------------!
USE MOD_Mesh_CGNS_Definitions
!----------------------------------------------------------------------------------------------------------------------!
IMPLICIT NONE
!----------------------------------------------------------------------------------------------------------------------!
! FORMAL ARGUMENTS
!----------------------------------------------------------------------------------------------------------------------!
! LOCAL VARIABLES
!----------------------------------------------------------------------------------------------------------------------!
INTEGER :: nElemNodes
INTEGER :: nMaxElemNodes
INTEGER :: ElemType
INTEGER :: NodeID
INTEGER :: iNode
INTEGER :: iElem
INTEGER :: iVertex
INTEGER :: nNodes
INTEGER :: nElems
INTEGER :: nDataVars
INTEGER :: nOutputVars
INTEGER :: nBCFaces
!----------------------------------------------------------------------------------------------------------------------!
INTEGER :: nElems_EDGE
INTEGER :: nElems_TRI
INTEGER :: nElems_QUAD
INTEGER :: nElems_TETRA
INTEGER :: nElems_HEXA
INTEGER :: nElems_PRISM
INTEGER :: nElems_PYRA
!----------------------------------------------------------------------------------------------------------------------!
CHARACTER(LEN=256),ALLOCATABLE :: CoordNames(:)
CHARACTER(LEN=256),ALLOCATABLE :: DataNames(:)
CHARACTER(LEN=256),ALLOCATABLE :: OutputVars(:)
!----------------------------------------------------------------------------------------------------------------------!
REAL               :: CalcTimeIni
REAL               :: CalcTimeEnd
CHARACTER(LEN=256) :: ElapsedTime
!----------------------------------------------------------------------------------------------------------------------!
CHARACTER(LEN=256) :: Header
!----------------------------------------------------------------------------------------------------------------------!
CHARACTER(LEN=256) :: InputMeshFile
CHARACTER(LEN=256) :: InputMeshFolder
CHARACTER(LEN=256) :: InputMeshFileFormat
CHARACTER(LEN=256) :: ErrorMessage
CHARACTER(LEN=256) :: FunctionName = "MeshImport"
!----------------------------------------------------------------------------------------------------------------------!

InputMeshFolder     = TRIM(ParametersMeshImport%InputMeshFolder)
InputMeshFile       = TRIM(ParametersMeshImport%InputMeshFile)
InputMeshFile       = TRIM(InputMeshFolder)//"/"//TRIM(InputMeshFile)
InputMeshFileFormat = TRIM(ParametersMeshImport%InputMeshFileFormat)

!------------------------------------------------------------!
! IMPORTING MESH
!------------------------------------------------------------!
SWRITE(UNIT_SCREEN,*)
Header = "IMPORTING MESH..."
CALL PrintMessage(Header)

CALL PrintAnalyze("MeshFile",TRIM(ParametersMeshImport%InputMeshFile))

CalcTimeIni = RunningTime()

SELECT CASE(LowerCase(InputMeshFileFormat))
  CASE("gmsh")
    CALL MeshImport_GMSH(&
      InputFile          = InputMeshFile,&
      ElementsType       = MeshData_ElementsToElementType,&
      ElementsToNodes    = MeshData_ElementsToNodes,&
      NodesCoordinates   = MeshData_NodesCoordinates,&
      BCFacesToMark      = MeshData_BCFacesToMark,&
      BCFacesToNodes     = MeshData_BCFacesToNodes,&
      BCFacesElementType = MeshData_BCFacesToElementType,&
      BoundaryMark       = MeshData_BoundaryMark,&
      BoundaryName       = MeshData_BoundaryName,&
      Debug              = ParametersMeshImport%DebugMeshImport)
  CASE DEFAULT
  ErrorMessage = "Unknown mesh format"
  CALL PrintError(__STAMP__,ErrorMessage,ModuleName,FunctionName)
END SELECT

nNodes   = SIZE(MeshData_NodesCoordinates,2)
nElems   = SIZE(MeshData_ElementsToNodes,2)
nBCFaces = SIZE(MeshData_BCFacesToNodes,2)

IF (RotateMesh .EQV. .TRUE.) THEN
  CALL PrintAnalyze("Rotating the Mesh","OK")
  DO iNode=1,nNodes
    CALL RotateMesh3D(RotationMatrix3D,MeshData_NodesCoordinates(1:3,iNode))
  END DO
END IF

IF ((ExtrusionDirection .EQ. 'y') .AND. (LowerCase(InputMeshFileFormat) .EQ. "gmsh")) THEN
  CALL GMSH_FixElementsOrientation(MeshData_NodesCoordinates,MeshData_ElementsToElementType,MeshData_ElementsToNodes)
  CALL PrintAnalyze("Elements Orientation","Fixed")
END IF

!------------------------------------------------------------!
! PRINTING NUMBER OF ELEMENTS BY TYPE
!------------------------------------------------------------!
nElems_EDGE  = 0
nElems_TRI   = 0
nElems_QUAD  = 0
nElems_TETRA = 0
nElems_HEXA  = 0
nElems_PRISM = 0
nElems_PYRA  = 0

DO iElem=1,SIZE(MeshData_ElementsToElementType,1)
  ElemType = MeshData_ElementsToElementType(iElem)
  SELECT CASE(ElemType)
    CASE(ELEMTYPE_EDGE2)
      nElems_EDGE = nElems_EDGE+1
    CASE(ELEMTYPE_TRI3)
      nElems_TRI = nElems_TRI+1
    CASE(ELEMTYPE_QUAD4)
      nElems_QUAD = nElems_QUAD+1
    CASE(ELEMTYPE_TETRA4)
      nElems_TETRA = nElems_TETRA+1
    CASE(ELEMTYPE_HEXA8)
      nElems_HEXA = nElems_HEXA+1
    CASE(ELEMTYPE_PRISM6)
      nElems_PRISM = nElems_PRISM+1
    CASE(ELEMTYPE_PYRA5)
      nElems_PYRA = nElems_PYRA+1
  END SELECT
END DO

IF (nElems_EDGE .GT. 0) THEN
  CALL PrintAnalyze("nElems (EDGE2)",FormatNumber(nElems_EDGE))
END IF
IF (nElems_TRI .GT. 0) THEN
  CALL PrintAnalyze("nElems (TRI3)",FormatNumber(nElems_TRI))
END IF
IF (nElems_QUAD .GT. 0) THEN
  CALL PrintAnalyze("nElems (QUAD4)",FormatNumber(nElems_QUAD))
END IF
IF (nElems_TETRA .GT. 0) THEN
  CALL PrintAnalyze("nElems (TETRA4)",FormatNumber(nElems_TETRA))
END IF
IF (nElems_HEXA .GT. 0) THEN
  CALL PrintAnalyze("nElems (HEXA8)",FormatNumber(nElems_HEXA))
END IF
IF (nElems_PRISM .GT. 0) THEN
  CALL PrintAnalyze("nElems (PRISM6)",FormatNumber(nElems_PRISM))
END IF
IF (nElems_PYRA .GT. 0) THEN
  CALL PrintAnalyze("nElems (PYRA5)",FormatNumber(nElems_PYRA))
END IF

CALL PrintAnalyze("nElems (TOTAL)",FormatNumber(nElems))

!------------------------------------------------------------!
! PRINTING NUMBER OF BCFACES BY TYPE
!------------------------------------------------------------!
nElems_EDGE  = 0
nElems_TRI   = 0
nElems_QUAD  = 0

DO iElem=1,SIZE(MeshData_BCFacesToElementType,1)
  ElemType = MeshData_BCFacesToElementType(iElem)
  SELECT CASE(ElemType)
    CASE(ELEMTYPE_EDGE2)
      nElems_EDGE = nElems_EDGE+1
    CASE(ELEMTYPE_TRI3)
      nElems_TRI = nElems_TRI+1
    CASE(ELEMTYPE_QUAD4)
      nElems_QUAD = nElems_QUAD+1
  END SELECT
END DO

IF (nElems_EDGE .GT. 0) THEN
  CALL PrintAnalyze("nBCFaces (EDGE2)",FormatNumber(nElems_EDGE))
END IF
IF (nElems_TRI .GT. 0) THEN
  CALL PrintAnalyze("nBCFaces (TRI3)",FormatNumber(nElems_TRI))
END IF
IF (nElems_QUAD .GT. 0) THEN
  CALL PrintAnalyze("nBCFaces (QUAD4)",FormatNumber(nElems_QUAD))
END IF

CALL PrintAnalyze("nBCFaces (TOTAL)",FormatNumber(nBCFaces))
CALL PrintAnalyze("nNodes",FormatNumber(nNodes))

! WARNING
! WARNING
! WARNING

CalcTimeEnd = RunningTime()
CALL ComputeRuntime(CalcTimeEnd-CalcTimeIni,ElapsedTime)
CALL PrintAnalyze("Elapsed Time",ElapsedTime,nTabIn=2)

! Exporting ElementsToNodesList to array
! For PP_nDims=2, MAX(nElemNodes)=MAX({3,4})=4
! For PP_nDims=3, MAX(nElemNodes)=MAX({4,5,6,8})=8

! Exporting BCFacesToNodesList to array
! For PP_nDims=2, MAX(nBCFacesNodes)=MAX({2})=2
! For PP_nDims=3, MAX(nBCFacesNodes)=MAX({3,4})=4

SELECT CASE(PP_nDims)
  CASE(2)
    nMaxElemNodes = 4
  CASE(3)
    nMaxElemNodes = 8
  CASE DEFAULT
  ErrorMessage = "Only implemented for 2D and 3D"
  CALL PrintError(__STAMP__,ErrorMessage,ModuleName,FunctionName)
END SELECT

IF (ALLOCATED(CoordNames) .EQV. .TRUE.) THEN
  DEALLOCATE(CoordNames)
END IF
ALLOCATE(CoordNames(1:PP_nDims))

SELECT CASE(PP_nDims)
  CASE(2)
    CoordNames(1) = "CoordinateX"
    CoordNames(2) = "CoordinateY"
  CASE(3)
    CoordNames(1) = "CoordinateX"
    CoordNames(2) = "CoordinateY"
    CoordNames(3) = "CoordinateZ"
  CASE DEFAULT
  ErrorMessage = "Only implemented for 2D and 3D"
  CALL PrintError(__STAMP__,ErrorMessage,ModuleName,FunctionName)
END SELECT

nDataVars   = 2
nOutputVars = PP_nDims+nDataVars
IF (ALLOCATED(DataNames) .EQV. .TRUE.) THEN
  DEALLOCATE(DataNames)
END IF
ALLOCATE(DataNames(1:nDataVars))
DataNames(1)  = "Level"
DataNames(2)  = "Flag"

IF (ALLOCATED(OutputVars) .EQV. .TRUE.) THEN
  DEALLOCATE(OutputVars)
END IF
ALLOCATE(OutputVars(1:nOutputVars))

OutputVars(1:PP_nDims)             = CoordNames(1:PP_nDims)
OutputVars(PP_nDims+1:nOutputVars) = DataNames(1:nDataVars)

IF (ALLOCATED(MeshData_ElementsToNodesCoordinates) .EQV. .TRUE.) THEN
  DEALLOCATE(MeshData_ElementsToNodesCoordinates)
END IF
ALLOCATE(MeshData_ElementsToNodesCoordinates(1:PP_nDims,1:nMaxElemNodes,1:nElems))
MeshData_ElementsToNodesCoordinates = 0.0
DO iElem=1,nElems
  ElemType = MeshData_ElementsToElementType(iElem)
  CALL GetnElemNodes(ElemType,nElemNodes)
  DO iVertex=1,nElemNodes
    NodeID = MeshData_ElementsToNodes(iVertex,iElem)
    MeshData_ElementsToNodesCoordinates(1:PP_nDims,iVertex,iElem) = MeshData_NodesCoordinates(1:PP_nDims,NodeID)
  END DO
END DO

IF (ALLOCATED(MeshInfo%CoordNames) .EQV. .TRUE.) THEN
  DEALLOCATE(MeshInfo%CoordNames)
END IF
ALLOCATE(MeshInfo%CoordNames(1:PP_nDims))

IF (ALLOCATED(MeshInfo%DataNames) .EQV. .TRUE.) THEN
  DEALLOCATE(MeshInfo%DataNames)
END IF
ALLOCATE(MeshInfo%DataNames(1:nDataVars))

IF (ALLOCATED(MeshInfo%OutputVars) .EQV. .TRUE.) THEN
  DEALLOCATE(MeshInfo%OutputVars)
END IF
ALLOCATE(MeshInfo%OutputVars(1:nOutputVars))

MeshInfo%nDims        = PP_nDims
MeshInfo%NGeo         = 1
MeshInfo%nElems       = nElems
MeshInfo%nNodes       = nNodes
MeshInfo%nBCFaces     = nBCFaces
MeshInfo%nOutVars     = PP_nDims
MeshInfo%MaxRefLevel  = 0
MeshInfo%DataNames    = DataNames
MeshInfo%CoordNames   = CoordNames
MeshInfo%OutputVars   = OutputVars
MeshInfo%FileVersion  = TRIM(FileVersion)
MeshInfo%ProgramName  = TRIM(ProgramName)
MeshInfo%ProjectName  = TRIM(ProjectName)
MeshInfo%BaseFileName = TRIM(ProjectName)//"_MESH"

!----------------------------------------------------------------------------------------------------------------------!
END SUBROUTINE MeshImport
!======================================================================================================================!
!
!
!
!======================================================================================================================!
SUBROUTINE GMSH_FixElementsOrientation(NodesCoordinates,ElementsType,ElementsToNodes,Debug)
!----------------------------------------------------------------------------------------------------------------------!
USE MOD_NumericsTools,ONLY: NORMALIZE
USE MOD_NumericsTools,ONLY: DOTPRODUCT
USE MOD_NumericsTools,ONLY: CROSSPRODUCT
!----------------------------------------------------------------------------------------------------------------------!
USE MOD_Mesh_CGNS_Definitions,ONLY: ELEMTYPE_HEXA8
USE MOD_Mesh_CGNS_Definitions,ONLY: ELEMTYPE_PRISM6
USE MOD_Mesh_CGNS_Definitions,ONLY: GetnElemNodes
USE MOD_Mesh_CGNS_Definitions,ONLY: GetnElemFaces
USE MOD_Mesh_CGNS_Definitions,ONLY: GetnNodesOnFace
USE MOD_Mesh_CGNS_Definitions,ONLY: GetNodesOnFace
!----------------------------------------------------------------------------------------------------------------------!
IMPLICIT NONE
!----------------------------------------------------------------------------------------------------------------------!
! FORMAL ARGUMENTS
!----------------------------------------------------------------------------------------------------------------------!
REAL,INTENT(IN)             :: NodesCoordinates(:,:)
INTEGER,INTENT(IN)          :: ElementsType(:)
INTEGER,INTENT(INOUT)       :: ElementsToNodes(:,:)
LOGICAL,INTENT(IN),OPTIONAL :: Debug
!----------------------------------------------------------------------------------------------------------------------!
! LOCAL VARIABLES
!----------------------------------------------------------------------------------------------------------------------!
REAL    :: UnitVector(1:3,1:3)
REAL    :: ElemAxis(1:3,1:3)
REAL    :: FaceNormalVector(1:3)
REAL    :: test
INTEGER :: iDir
INTEGER :: nElems
INTEGER :: ElemID
INTEGER :: nFixed
INTEGER :: SortedNodeIDs(1:8)
INTEGER :: ElemNodeIDSorting(1:8,1:8,1:3)
LOGICAL :: DebugMode
!----------------------------------------------------------------------------------------------------------------------!
CHARACTER(LEN=256) :: ErrorMessage
CHARACTER(LEN=256) :: FunctionName = "GMSH_FixElementsOrientation"
!----------------------------------------------------------------------------------------------------------------------!

IF (PRESENT(Debug)) THEN
  DebugMode = Debug
ELSE
  DebugMode = .FALSE.
END IF
IF (PP_nDims .NE. 3) THEN
  RETURN
END IF

! WARNING
! ! ! RETURN
! ! ! DebugMode = .TRUE.
! WARNING

! WARNING WARNING WARNING WARNING WARNING WARNING WARNING
! Z-ORIENTATION VS Y-ORIENTATION
! WARNING WARNING WARNING WARNING WARNING WARNING WARNING

! ! ! ! ElemNodeIDSorting(NodeIDs,NodeOrigin,Orientation)
! ! ! ElemNodeIDSorting(1:8,1,1) = (/1,2,3,4,5,6,7,8/) ! (x,y,z) -> (+x,+y,+z) +Y
! ! ! ElemNodeIDSorting(1:8,1,2) = (/1,5,6,2,4,8,7,3/) ! (x,y,z) -> (+z,+x,+y) +X
! ! ! ElemNodeIDSorting(1:8,1,3) = (/1,4,8,5,2,3,7,6/) ! (x,y,z) -> (+y,+z,+x) +Z
! ! !
! ! ! ElemNodeIDSorting(1:8,2,1) = (/4,1,2,3,7,8,5,6/) ! (x,y,z) -> (-y,+x,+z)
! ! ! ElemNodeIDSorting(1:8,2,2) = (/2,1,5,6,3,4,8,7/) ! (x,y,z) -> (-x,+z,+y)
! ! ! ElemNodeIDSorting(1:8,2,3) = (/5,1,4,8,6,2,3,7/) ! (x,y,z) -> (-z,+y,+x)
! ! !
! ! ! ElemNodeIDSorting(1:8,3,1) = (/3,4,1,2,7,8,5,6/) ! (x,y,z) -> (-x,-y,+z) -Y
! ! ! ElemNodeIDSorting(1:8,3,2) = (/8,5,1,4,7,6,2,3/) ! (x,y,z) -> (-y,-z,+x) -Z
! ! ! ElemNodeIDSorting(1:8,3,3) = (/6,2,1,5,7,3,4,8/) ! (x,y,z) -> (-z,-x,+y) -X
! ! !
! ! ! ElemNodeIDSorting(1:8,4,1) = (/2,3,4,1,6,7,8,5/) ! (x,y,z) -> (+y,-x,+z)++++++++
! ! ! ElemNodeIDSorting(1:8,4,2) = (/5,6,2,1,8,7,3,4/) ! (x,y,z) -> (+x,-z,+y)++++++++
! ! ! ElemNodeIDSorting(1:8,4,3) = (/4,8,5,1,3,7,6,2/) ! (x,y,z) -> (+z,-y,+x)++++++++
! ! !
! ! ! ElemNodeIDSorting(1:8,5,1) = (/5,8,7,6,1,4,3,2/) ! (x,y,z) -> (+y,+x,-z)
! ! ! ElemNodeIDSorting(1:8,5,2) = (/2,6,7,3,1,5,8,4/) ! (x,y,z) -> (+z,+y,-x)
! ! ! ElemNodeIDSorting(1:8,5,3) = (/4,3,7,8,1,2,6,5/) ! (x,y,z) -> (+x,+z,-y)
! ! !
! ! ! ElemNodeIDSorting(1:8,6,1) = (/6,5,8,7,2,1,4,3/) ! (x,y,z) -> (-x,+y,-z)
! ! ! ElemNodeIDSorting(1:8,6,2) = (/3,2,6,7,4,1,5,8/) ! (x,y,z) -> (-y,+z,-x)
! ! ! ElemNodeIDSorting(1:8,6,3) = (/8,4,3,7,5,1,2,6/) ! (x,y,z) -> (-z,+x,-y)
! ! !
! ! ! ElemNodeIDSorting(1:8,7,1) = (/7,6,5,8,3,2,1,4/) ! (x,y,z) -> (-y,-x,-z)***
! ! ! ElemNodeIDSorting(1:8,7,2) = (/7,3,2,6,8,4,1,5/) ! (x,y,z) -> (-z,-y,-x)***
! ! ! ElemNodeIDSorting(1:8,7,3) = (/7,8,4,3,6,5,1,2/) ! (x,y,z) -> (-x,-z,-y)***
! ! !
! ! ! ElemNodeIDSorting(1:8,8,1) = (/8,7,6,5,4,3,2,1/) ! (x,y,z) -> (+x,-y,-z)
! ! ! ElemNodeIDSorting(1:8,8,2) = (/6,7,3,2,5,8,4,1/) ! (x,y,z) -> (+y,-z,-x)
! ! ! ElemNodeIDSorting(1:8,8,3) = (/3,7,8,4,2,6,5,1/) ! (x,y,z) -> (+z,-x,-y)

! ElemNodeIDSorting(NodeIDs,NodeOrigin,Orientation)
UnitVector(1:3,1) = (/1.0,0.0,0.0/)
UnitVector(1:3,2) = (/0.0,1.0,0.0/)
UnitVector(1:3,3) = (/0.0,0.0,1.0/)

ElemNodeIDSorting(1:8,1,1) = (/1,5,6,2,4,8,7,3/) ! (x,y,z) -> (+z,+x,+y) +X
ElemNodeIDSorting(1:8,1,2) = (/1,2,3,4,5,6,7,8/) ! (x,y,z) -> (+x,+y,+z) +Y
ElemNodeIDSorting(1:8,1,3) = (/1,4,8,5,2,3,7,6/) ! (x,y,z) -> (+y,+z,+x) +Z

ElemNodeIDSorting(1:8,7,1) = (/7,6,5,8,3,2,1,4/) ! (x,y,z) -> (-y,-x,-z) -X
ElemNodeIDSorting(1:8,7,2) = (/7,3,2,6,8,4,1,5/) ! (x,y,z) -> (-z,-y,-x) -Y
ElemNodeIDSorting(1:8,7,3) = (/7,8,4,3,6,5,1,2/) ! (x,y,z) -> (-x,-z,-y) -Z

DO ElemID=1,nElems
  IF ((ElementsType(ElemID) .NE. ELEMTYPE_HEXA8) .OR. (ElementsType(ElemID) .NE. ELEMTYPE_PRISM6)) THEN
    ErrorMessage = "This function works only for full hexahedral/prismatic meshes"
    CALL PrintError(__STAMP__,ErrorMessage,ModuleName,FunctionName)
  END IF
END DO

! IF (ANY(ElementsType .NE. ELEMTYPE_HEXA8) .OR. ANY(ElementsType .NE. ELEMTYPE_PRISM6)) THEN
! END IF

nElems = SIZE(ElementsType)

nFixed = 0

DO ElemID=1,nElems
  IF (ElementsType(ElemID) .EQ. ELEMTYPE_HEXA8) THEN
    ElemAxis(1:3,1) = NodesCoordinates(1:3,ElementsToNodes(2,ElemID))-NodesCoordinates(1:3,ElementsToNodes(1,ElemID))
    ElemAxis(1:3,2) = NodesCoordinates(1:3,ElementsToNodes(4,ElemID))-NodesCoordinates(1:3,ElementsToNodes(1,ElemID))
    ElemAxis(1:3,3) = NodesCoordinates(1:3,ElementsToNodes(5,ElemID))-NodesCoordinates(1:3,ElementsToNodes(1,ElemID))

    ElemAxis(1:3,1) = NORMALIZE(ElemAxis(1:3,1))
    ElemAxis(1:3,2) = NORMALIZE(ElemAxis(1:3,2))
    ElemAxis(1:3,3) = NORMALIZE(ElemAxis(1:3,3))

    FaceNormalVector(1:3) = ElemAxis(1:3,3)

    DO iDir=1,PP_nDims
      test = DOTPRODUCT(FaceNormalVector(1:3),UnitVector(1:3,iDir))
      IF (ABS(test) .GT. 0.95) THEN
        EXIT
      END IF
    END DO

    SELECT CASE(iDir)
      CASE(1)
        IF (test .GT. 0.0) THEN
          nFixed = nFixed+1
          SortedNodeIDs(1:8) = ElementsToNodes(ElemNodeIDSorting(1:8,1,1),ElemID)
        ELSEIF (test .LT. 0.0) THEN
          nFixed = nFixed+1
          SortedNodeIDs(1:8) = ElementsToNodes(ElemNodeIDSorting(1:8,7,1),ElemID)
        END IF
      CASE(2)
        IF (test .GT. 0.0) THEN
          nFixed = nFixed+1
          SortedNodeIDs(1:8) = ElementsToNodes(ElemNodeIDSorting(1:8,1,2),ElemID)
        ELSEIF (test .LT. 0.0) THEN
          nFixed = nFixed+1
          SortedNodeIDs(1:8) = ElementsToNodes(ElemNodeIDSorting(1:8,7,2),ElemID)
        END IF
      CASE(3)
        IF (test .GT. 0.0) THEN
          nFixed = nFixed+1
          SortedNodeIDs(1:8) = ElementsToNodes(ElemNodeIDSorting(1:8,1,3),ElemID)
        ELSEIF (test .LT. 0.0) THEN
          nFixed = nFixed+1
          SortedNodeIDs(1:8) = ElementsToNodes(ElemNodeIDSorting(1:8,7,3),ElemID)
        END IF
    END SELECT
    ElementsToNodes(1:8,ElemID) = SortedNodeIDs(1:8)
  END IF
END DO

!----------------------------------------------------------------------------------------------------------------------!
END SUBROUTINE GMSH_FixElementsOrientation
!======================================================================================================================!
!
!
!
!======================================================================================================================!
SUBROUTINE RotateMesh3D(RotationMatrix,x)
!----------------------------------------------------------------------------------------------------------------------!
IMPLICIT NONE
!----------------------------------------------------------------------------------------------------------------------!
! FORMAL ARGUMENTS
!----------------------------------------------------------------------------------------------------------------------!
REAL,INTENT(IN)    :: RotationMatrix(1:3,1:3,1:3)
REAL,INTENT(INOUT) :: x(1:3)
!----------------------------------------------------------------------------------------------------------------------!
! LOCAL VARIABLES
!----------------------------------------------------------------------------------------------------------------------!

x(1:3) = MATMUL(RotationMatrix(1:3,1:3,1),x(1:3))
x(1:3) = MATMUL(RotationMatrix(1:3,1:3,2),x(1:3))
x(1:3) = MATMUL(RotationMatrix(1:3,1:3,3),x(1:3))

!----------------------------------------------------------------------------------------------------------------------!
END SUBROUTINE RotateMesh3D
!======================================================================================================================!
!
!
!
!----------------------------------------------------------------------------------------------------------------------!
END MODULE MOD_MeshImport
!======================================================================================================================!
