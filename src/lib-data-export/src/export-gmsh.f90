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
MODULE MOD_DataExport_GMSH
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
INTERFACE DataExport_GMSH
  MODULE PROCEDURE DataExport_GMSH
END INTERFACE
!----------------------------------------------------------------------------------------------------------------------!
PUBLIC :: DataExport_GMSH
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
SUBROUTINE DataExport_GMSH(&
  FileName,&
  ProjectName,&
  ProgramName,&
  FileVersion,&
  nDims,&
  ElementsType,&
  ElementsToNodes,&
  NodesCoordinates,&
  BCFacesToNodes,&
  BCFacesToMark,&
  BCFacesElementType,&
  BoundaryMark,&
  BoundaryName)
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
INTEGER,INTENT(IN)            :: ElementsType(:)
INTEGER,INTENT(IN)            :: ElementsToNodes(:,:)
REAL,INTENT(IN)               :: NodesCoordinates(:,:)
INTEGER,INTENT(IN)            :: BCFacesToNodes(:,:)
INTEGER,INTENT(IN)            :: BCFacesToMark(:)
INTEGER,INTENT(IN)            :: BCFacesElementType(:)
INTEGER,INTENT(IN)            :: BoundaryMark(:)
CHARACTER(LEN=256),INTENT(IN) :: BoundaryName(:)
!----------------------------------------------------------------------------------------------------------------------!
! LOCAL VARIABLES
!----------------------------------------------------------------------------------------------------------------------!
INTEGER :: UNIT_FILE
INTEGER :: nTabIn
INTEGER :: ii
INTEGER :: Tag1
INTEGER :: Tag2
INTEGER :: nTags
INTEGER :: iElem
INTEGER :: iNode
INTEGER :: nNodes
INTEGER :: nElems
INTEGER :: ElemID
INTEGER :: LastElemID
INTEGER :: nElemNodes
INTEGER :: ElemType
INTEGER :: FaceID
INTEGER :: iBCFace
INTEGER :: nBCFaces
INTEGER :: nBoundaries
INTEGER :: nBCFacesNodes
INTEGER :: PhysicalDomain
INTEGER :: PhysicalDimension
INTEGER :: nPhysicalEntities
!----------------------------------------------------------------------------------------------------------------------!
INTEGER,ALLOCATABLE :: MaskElemType(:)
!----------------------------------------------------------------------------------------------------------------------!
CHARACTER(LEN=256) :: FormatString
CHARACTER(LEN=256) :: FullFileName
CHARACTER(LEN=256) :: FileExtension
!----------------------------------------------------------------------------------------------------------------------!
CHARACTER(LEN=256) :: StrL
CHARACTER(LEN=256) :: StrR
!----------------------------------------------------------------------------------------------------------------------!

FileExtension = ".msh"
FullFileName  = TRIM(FileName)//TRIM(FileExtension)

nElems      = SIZE(ElementsToNodes,2)
nNodes      = SIZE(NodesCoordinates,2)
nBCFaces    = SIZE(BCFacesToNodes,2)
nBoundaries = SIZE(BoundaryName,1)

nPhysicalEntities = nBoundaries+1

nTabIn = 4
StrL = "Writing MESH"
StrR = TRIM(FullFileName)
CALL PrintAnalyze(StrL,StrR,nTabIn=nTabIn)

OPEN(NEWUNIT=UNIT_FILE,FILE=TRIM(FullFileName),STATUS="REPLACE")

!--------------------------------------------------!
! SECTION: $MeshFormat
!--------------------------------------------------!
WRITE(UNIT_FILE,"(A)") "$MeshFormat"
WRITE(UNIT_FILE,"(A)") "2.2 0 8"
WRITE(UNIT_FILE,"(A)") "$EndMeshFormat"

!--------------------------------------------------!
! SECTION: $PhysicalNames
!--------------------------------------------------!
WRITE(UNIT_FILE,"(A)") "$PhysicalNames"
WRITE(UNIT_FILE,"(I0)") nPhysicalEntities

SELECT CASE(nDims)
  CASE(2)
    PhysicalDimension = 1
    DO ii=1,nBoundaries
      WRITE(UNIT_FILE,"(I0,1X,I0,1X,A1,A,A1)") PhysicalDimension, BoundaryMark(ii), '"', TRIM(BoundaryName(ii)), '"'
    END DO
    PhysicalDomain = BoundaryMark(nBoundaries)+1
    WRITE(UNIT_FILE,"(I0,1X,I0,1X,A1,A,A1)") PhysicalDimension+1, PhysicalDomain, '"', "PhysicalDomain", '"'
  CASE(3)
    PhysicalDimension = 2
    DO ii=1,nBoundaries
      WRITE(UNIT_FILE,"(I0,1X,I0,1X,A1,A,A1)") PhysicalDimension, BoundaryMark(ii), '"', TRIM(BoundaryName(ii)), '"'
    END DO
    PhysicalDomain = BoundaryMark(nBoundaries)+1
    WRITE(UNIT_FILE,"(I0,1X,I0,1X,A1,A,A1)") PhysicalDimension+1, PhysicalDomain, '"', "PhysicalDomain", '"'
END SELECT
WRITE(UNIT_FILE,"(A)") "$EndPhysicalNames"

!--------------------------------------------------!
! SECTION: $Nodes
!--------------------------------------------------!
WRITE(UNIT_FILE,"(A)") "$Nodes"
WRITE(UNIT_FILE,"(I0)") nNodes

WRITE(FormatString,'(A)') "(I0,SP,3(1X,ES20.13E2))"
DO iNode=1,nNodes
  WRITE(UNIT_FILE,FormatString) iNode, NodesCoordinates(1:3,iNode)
END DO

WRITE(UNIT_FILE,"(A)") "$EndNodes"

!--------------------------------------------------!
! SECTION: $Elements
!--------------------------------------------------!
WRITE(UNIT_FILE,"(A)") "$Elements"
WRITE(UNIT_FILE,"(I0)") nElems+nBCFaces

nTags = 2
!--------------------------------------------------!
! Tag1: Physical Entity
! Tag2: Geometrical Entity
!--------------------------------------------------!

LastElemID = 0
SELECT CASE(nDims)
  CASE(2)
    !--------------------------------------------------!
    ! ElemType = ELEMTYPE_EDGE2
    !--------------------------------------------------!
    IF (ANY(BCFacesElementType(:) .EQ. ELEMTYPE_EDGE2)) THEN
      ElemType  = ELEMTYPE_EDGE2
      nBCFacesNodes = 2

      nBCFaces = 0
      DO iBCFace=1,SIZE(BCFacesToNodes,2)
        IF (BCFacesElementType(iBCFace) .EQ. ElemType) THEN
          nBCFaces = nBCFaces+1
        END IF
      END DO

      ! Subarray Mask of ElemType
      IF (ALLOCATED(MaskElemType)) THEN
        DEALLOCATE(MaskElemType)
      END IF
      ALLOCATE(MaskElemType(1:nBCFaces))
      FaceID = 0
      DO iBCFace=1,SIZE(BCFacesToNodes,2)
        IF (BCFacesElementType(iBCFace) .EQ. ElemType) THEN
          FaceID = FaceID+1
          MaskElemType(FaceID) = iBCFace
        END IF
      END DO
      
      ! Writing Elements-to-Nodes data
      WRITE(FormatString,'(A,I0,A)') "(", nBCFacesNodes+5, "(I0,1X))"
      FaceID = LastElemID
      DO iBCFace=1,nBCFaces
        FaceID = FaceID+1
        Tag1 = BCFacesToMark(MaskElemType(iBCFace))
        Tag2 = 1
        WRITE(UNIT_FILE,FormatString) &
          FaceID, ElemType, nTags, Tag1, Tag2, BCFacesToNodes(1:nBCFacesNodes,MaskElemType(iBCFace))
      END DO
      LastElemID = FaceID
    END IF
  CASE(3)
    !--------------------------------------------------!
    ! ElemType = ELEMTYPE_TRI3
    !--------------------------------------------------!
    IF (ANY(BCFacesElementType(:) .EQ. ELEMTYPE_TRI3)) THEN
      ElemType  = ELEMTYPE_TRI3
      nBCFacesNodes = 3

      nBCFaces = 0
      DO iBCFace=1,SIZE(BCFacesToNodes,2)
        IF (BCFacesElementType(iBCFace) .EQ. ElemType) THEN
          nBCFaces = nBCFaces+1
        END IF
      END DO

      ! Subarray Mask of ElemType
      IF (ALLOCATED(MaskElemType)) THEN
        DEALLOCATE(MaskElemType)
      END IF
      ALLOCATE(MaskElemType(1:nBCFaces))
      FaceID = 0
      DO iBCFace=1,SIZE(BCFacesToNodes,2)
        IF (BCFacesElementType(iBCFace) .EQ. ElemType) THEN
          FaceID = FaceID+1
          MaskElemType(FaceID) = iBCFace
        END IF
      END DO
      
      ! Writing Elements-to-Nodes data
      WRITE(FormatString,'(A,I0,A)') "(", nBCFacesNodes+5, "(I0,1X))"
      FaceID = LastElemID
      DO iBCFace=1,nBCFaces
        FaceID = FaceID+1
        Tag1 = BCFacesToMark(MaskElemType(iBCFace))
        Tag2 = 1
        WRITE(UNIT_FILE,FormatString) &
          FaceID, ElemType, nTags, Tag1, Tag2, BCFacesToNodes(1:nBCFacesNodes,MaskElemType(iBCFace))
      END DO
      LastElemID = FaceID
    END IF

    !--------------------------------------------------!
    ! ElemType = ELEMTYPE_QUAD4
    !--------------------------------------------------!
    IF (ANY(BCFacesElementType(:) .EQ. ELEMTYPE_QUAD4)) THEN
      ElemType  = ELEMTYPE_QUAD4
      nBCFacesNodes = 4

      nBCFaces = 0
      DO iBCFace=1,SIZE(BCFacesToNodes,2)
        IF (BCFacesElementType(iBCFace) .EQ. ElemType) THEN
          nBCFaces = nBCFaces+1
        END IF
      END DO

      ! Subarray Mask of ElemType
      IF (ALLOCATED(MaskElemType)) THEN
        DEALLOCATE(MaskElemType)
      END IF
      ALLOCATE(MaskElemType(1:nBCFaces))
      FaceID = 0
      DO iBCFace=1,SIZE(BCFacesToNodes,2)
        IF (BCFacesElementType(iBCFace) .EQ. ElemType) THEN
          FaceID = FaceID+1
          MaskElemType(FaceID) = iBCFace
        END IF
      END DO
      
      ! Writing Elements-to-Nodes data
      WRITE(FormatString,'(A,I0,A)') "(", nBCFacesNodes+5, "(I0,1X))"
      FaceID = LastElemID
      DO iBCFace=1,nBCFaces
        FaceID = FaceID+1
        Tag1 = BCFacesToMark(MaskElemType(iBCFace))
        Tag2 = 1
        WRITE(UNIT_FILE,FormatString) &
          FaceID, ElemType, nTags, Tag1, Tag2, BCFacesToNodes(1:nBCFacesNodes,MaskElemType(iBCFace))
      END DO
      LastElemID = FaceID
    END IF
END SELECT

! ! ! LastElemID = 0

!--------------------------------------------------!
! ElemType = ELEMTYPE_EDGE2
!--------------------------------------------------!
IF (ANY(ElementsType(:) .EQ. ELEMTYPE_EDGE2)) THEN
  ElemType  = ELEMTYPE_EDGE2
  nElemNodes = 2

  nElems = 0
  DO iElem=1,SIZE(ElementsToNodes,2)
    IF (ElementsType(iElem) .EQ. ElemType) THEN
      nElems = nElems+1
    END IF
  END DO

  ! Subarray Mask of ElemType
  IF (ALLOCATED(MaskElemType)) THEN
    DEALLOCATE(MaskElemType)
  END IF
  ALLOCATE(MaskElemType(1:nElems))
  ElemID = 0
  DO iElem=1,SIZE(ElementsToNodes,2)
    IF (ElementsType(iElem) .EQ. ElemType) THEN
      ElemID = ElemID+1
      MaskElemType(ElemID) = iElem
    END IF
  END DO
  
  ! Writing Elements-to-Nodes data
  WRITE(FormatString,'(A,I0,A)') "(", nElemNodes+5, "(I0,1X))"
  ElemID = LastElemID
  DO iElem=1,nElems
    ElemID = ElemID+1
    Tag1 = PhysicalDomain
    Tag2 = 2
    WRITE(UNIT_FILE,FormatString) &
      ElemID, ElemType, nTags, Tag1, Tag2, ElementsToNodes(1:nElemNodes,MaskElemType(iElem))
  END DO
  LastElemID = ElemID
  
END IF

!--------------------------------------------------!
! ElemType = ELEMTYPE_TRI3
!--------------------------------------------------!
IF (ANY(ElementsType(:) .EQ. ELEMTYPE_TRI3)) THEN
  ElemType  = ELEMTYPE_TRI3
  nElemNodes = 3

  nElems = 0
  DO iElem=1,SIZE(ElementsToNodes,2)
    IF (ElementsType(iElem) .EQ. ElemType) THEN
      nElems = nElems+1
    END IF
  END DO

  ! Subarray Mask of ElemType
  IF (ALLOCATED(MaskElemType)) THEN
    DEALLOCATE(MaskElemType)
  END IF
  ALLOCATE(MaskElemType(1:nElems))
  ElemID = 0
  DO iElem=1,SIZE(ElementsToNodes,2)
    IF (ElementsType(iElem) .EQ. ElemType) THEN
      ElemID = ElemID+1
      MaskElemType(ElemID) = iElem
    END IF
  END DO
  
  ! Writing Elements-to-Nodes data
  WRITE(FormatString,'(A,I0,A)') "(", nElemNodes+5, "(I0,1X))"
  ElemID = LastElemID
  DO iElem=1,nElems
    ElemID = ElemID+1
    Tag1 = PhysicalDomain
    Tag2 = 2
    WRITE(UNIT_FILE,FormatString) &
      ElemID, ElemType, nTags, Tag1, Tag2, ElementsToNodes(1:nElemNodes,MaskElemType(iElem))
  END DO
  LastElemID = ElemID
  
END IF

!--------------------------------------------------!
! ElemType = ELEMTYPE_QUAD4
!--------------------------------------------------!
IF (ANY(ElementsType(:) .EQ. ELEMTYPE_QUAD4)) THEN
  ElemType  = ELEMTYPE_QUAD4
  nElemNodes = 4

  nElems = 0
  DO iElem=1,SIZE(ElementsToNodes,2)
    IF (ElementsType(iElem) .EQ. ElemType) THEN
      nElems = nElems+1
    END IF
  END DO

  ! Subarray Mask of ElemType
  IF (ALLOCATED(MaskElemType)) THEN
    DEALLOCATE(MaskElemType)
  END IF
  ALLOCATE(MaskElemType(1:nElems))
  ElemID = 0
  DO iElem=1,SIZE(ElementsToNodes,2)
    IF (ElementsType(iElem) .EQ. ElemType) THEN
      ElemID = ElemID+1
      MaskElemType(ElemID) = iElem
    END IF
  END DO  
  
  ! Writing Elements-to-Nodes data
  WRITE(FormatString,'(A,I0,A)') "(", nElemNodes+5, "(I0,1X))"
  ElemID = LastElemID
  DO iElem=1,nElems
    ElemID = ElemID+1
    Tag1 = PhysicalDomain
    Tag2 = 2
    WRITE(UNIT_FILE,FormatString) &
      ElemID, ElemType, nTags, Tag1, Tag2, ElementsToNodes(1:nElemNodes,MaskElemType(iElem))
  END DO
  LastElemID = ElemID
  
END IF

!--------------------------------------------------!
! ElemType = ELEMTYPE_TETRA4
!--------------------------------------------------!
IF (ANY(ElementsType(:) .EQ. ELEMTYPE_TETRA4)) THEN
  ElemType  = ELEMTYPE_TETRA4
  nElemNodes = 4

  nElems = 0
  DO iElem=1,SIZE(ElementsToNodes,2)
    IF (ElementsType(iElem) .EQ. ElemType) THEN
      nElems = nElems+1
    END IF
  END DO

  ! Subarray Mask of ElemType
  IF (ALLOCATED(MaskElemType)) THEN
    DEALLOCATE(MaskElemType)
  END IF
  ALLOCATE(MaskElemType(1:nElems))
  ElemID = 0
  DO iElem=1,SIZE(ElementsToNodes,2)
    IF (ElementsType(iElem) .EQ. ElemType) THEN
      ElemID = ElemID+1
      MaskElemType(ElemID) = iElem
    END IF
  END DO

  ! Writing Elements-to-Nodes data
  WRITE(FormatString,'(A,I0,A)') "(", nElemNodes+5, "(I0,1X))"
  ElemID = LastElemID
  DO iElem=1,nElems
    ElemID = ElemID+1
    Tag1 = PhysicalDomain
    Tag2 = 2
    WRITE(UNIT_FILE,FormatString) &
      ElemID, ElemType, nTags, Tag1, Tag2, ElementsToNodes(1:nElemNodes,MaskElemType(iElem))
  END DO
  LastElemID = ElemID
  
END IF

!--------------------------------------------------!
! ElemType = ELEMTYPE_HEXA8
!--------------------------------------------------!
IF (ANY(ElementsType(:) .EQ. ELEMTYPE_HEXA8)) THEN
  ElemType  = ELEMTYPE_HEXA8
  nElemNodes = 8

  nElems = 0
  DO iElem=1,SIZE(ElementsToNodes,2)
    IF (ElementsType(iElem) .EQ. ElemType) THEN
      nElems = nElems+1
    END IF
  END DO

  ! Subarray Mask of ElemType
  IF (ALLOCATED(MaskElemType)) THEN
    DEALLOCATE(MaskElemType)
  END IF
  ALLOCATE(MaskElemType(1:nElems))
  ElemID = 0
  DO iElem=1,SIZE(ElementsToNodes,2)
    IF (ElementsType(iElem) .EQ. ElemType) THEN
      ElemID = ElemID+1
      MaskElemType(ElemID) = iElem
    END IF
  END DO
  
  ! Writing Elements-to-Nodes data
  WRITE(FormatString,'(A,I0,A)') "(", nElemNodes+5, "(I0,1X))"
  ElemID = LastElemID
  DO iElem=1,nElems
    ElemID = ElemID+1
    Tag1 = PhysicalDomain
    Tag2 = 2
    WRITE(UNIT_FILE,FormatString) &
      ElemID, ElemType, nTags, Tag1, Tag2, ElementsToNodes(1:nElemNodes,MaskElemType(iElem))
  END DO
  LastElemID = ElemID
  
END IF

!--------------------------------------------------!
! ElemType = ELEMTYPE_PRISM6
!--------------------------------------------------!
IF (ANY(ElementsType(:) .EQ. ELEMTYPE_PRISM6)) THEN
  ElemType  = ELEMTYPE_PRISM6
  nElemNodes = 6

  nElems = 0
  DO iElem=1,SIZE(ElementsToNodes,2)
    IF (ElementsType(iElem) .EQ. ElemType) THEN
      nElems = nElems+1
    END IF
  END DO

  ! Subarray Mask of ElemType
  IF (ALLOCATED(MaskElemType)) THEN
    DEALLOCATE(MaskElemType)
  END IF
  ALLOCATE(MaskElemType(1:nElems))
  ElemID = 0
  DO iElem=1,SIZE(ElementsToNodes,2)
    IF (ElementsType(iElem) .EQ. ElemType) THEN
      ElemID = ElemID+1
      MaskElemType(ElemID) = iElem
    END IF
  END DO
  
  ! Writing Elements-to-Nodes data
  WRITE(FormatString,'(A,I0,A)') "(", nElemNodes+5, "(I0,1X))"
  ElemID = LastElemID
  DO iElem=1,nElems
    ElemID = ElemID+1
    Tag1 = PhysicalDomain
    Tag2 = 2
    WRITE(UNIT_FILE,FormatString) &
      ElemID, ElemType, nTags, Tag1, Tag2, ElementsToNodes(1:nElemNodes,MaskElemType(iElem))
  END DO
  LastElemID = ElemID
  
END IF

!--------------------------------------------------!
! ElemType = ELEMTYPE_PYRA5
!--------------------------------------------------!
IF (ANY(ElementsType(:) .EQ. ELEMTYPE_PYRA5)) THEN
  ElemType  = ELEMTYPE_PYRA5
  nElemNodes = 5

  nElems = 0
  DO iElem=1,SIZE(ElementsToNodes,2)
    IF (ElementsType(iElem) .EQ. ElemType) THEN
      nElems = nElems+1
    END IF
  END DO

  ! Subarray Mask of ElemType
  IF (ALLOCATED(MaskElemType)) THEN
    DEALLOCATE(MaskElemType)
  END IF
  ALLOCATE(MaskElemType(1:nElems))
  ElemID = 0
  DO iElem=1,SIZE(ElementsToNodes,2)
    IF (ElementsType(iElem) .EQ. ElemType) THEN
      ElemID = ElemID+1
      MaskElemType(ElemID) = iElem
    END IF
  END DO
  
  ! Writing Elements-to-Nodes data
  WRITE(FormatString,'(A,I0,A)') "(", nElemNodes+5, "(I0,1X))"
  ElemID = LastElemID
  DO iElem=1,nElems
    ElemID = ElemID+1
    Tag1 = PhysicalDomain
    Tag2 = 2
    WRITE(UNIT_FILE,FormatString) &
      ElemID, ElemType, nTags, Tag1, Tag2, ElementsToNodes(1:nElemNodes,MaskElemType(iElem))
  END DO
  LastElemID = ElemID
  
END IF

WRITE(UNIT_FILE,"(A)") "$EndElements"

IF (ALLOCATED(MaskElemType)) THEN
  DEALLOCATE(MaskElemType)
END IF

CLOSE(UNIT_FILE)

!----------------------------------------------------------------------------------------------------------------------!
END SUBROUTINE DataExport_GMSH
!======================================================================================================================!
!
!
!
!======================================================================================================================!
END MODULE MOD_DataExport_GMSH
!======================================================================================================================!
