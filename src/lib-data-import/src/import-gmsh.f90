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
MODULE MOD_DataImport_GMSH
!----------------------------------------------------------------------------------------------------------------------!
USE MOD_GLOBAL_vars
!----------------------------------------------------------------------------------------------------------------------!
IMPLICIT NONE
!----------------------------------------------------------------------------------------------------------------------!
PRIVATE
!----------------------------------------------------------------------------------------------------------------------!
INTERFACE DataImport_GMSH
  MODULE PROCEDURE DataImport_GMSH
END INTERFACE
!----------------------------------------------------------------------------------------------------------------------!
PUBLIC :: DataImport_GMSH
!----------------------------------------------------------------------------------------------------------------------!
! ElemType={1}={edge}-{2-nodes}
! ElemType={2}={triangle}-{3-nodes}
! ElemType={3}={quadrangle}-{4-nodes}
! ElemType={4}={tetrahedron}-{4-nodes}
! ElemType={5}={hexahedron}-{8-nodes}
! ElemType={6}={prism}-{6-nodes}
! ElemType={7}={pyramid}-{7-nodes}
!----------------------------------------------------------------------------------------------------------------------!
INTEGER :: GMSH_ElemType(1:7,1:3) = RESHAPE(&
  (/1,2,1,&
    2,3,3,&
    3,4,4,&
    4,4,4,&
    5,8,6,&
    6,6,5,&
    7,5,5/),&
  (/7,3/),ORDER=[2,1])
!----------------------------------------------------------------------------------------------------------------------!
CHARACTER(LEN=256) :: ModuleName = "DataImport_GMSH"
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
SUBROUTINE DataImport_GMSH(&
  InputFile,&
  ElementsType,&
  ElementsToNodes,&
  NodesCoordinates,&
  BCFacesToMark,&
  BCFacesToNodes,&
  BCFacesElementType,&
  BoundaryMark,&
  BoundaryName,&
  Debug)
!----------------------------------------------------------------------------------------------------------------------!
USE MOD_DataStructures,ONLY: SplitString
!----------------------------------------------------------------------------------------------------------------------!
USE MOD_DataStructures,ONLY: tArrayREAL
USE MOD_DataStructures,ONLY: tArrayINTEGER
USE MOD_DataStructures,ONLY: tLinkedList
USE MOD_DataStructures,ONLY: tLinkedListNode
USE MOD_DataStructures,ONLY: CreateLinkedListNode
USE MOD_DataStructures,ONLY: AddLinkedListNode
USE MOD_DataStructures,ONLY: GetLinkedListNode
USE MOD_DataStructures,ONLY: PrintLinkedList
USE MOD_DataStructures,ONLY: DestructLinkedList
USE MOD_DataStructures,ONLY: CountLinkedListNodes
!----------------------------------------------------------------------------------------------------------------------!
USE MOD_DataStructures,ONLY: tBucket
USE MOD_DataStructures,ONLY: CreateBucket
USE MOD_DataStructures,ONLY: AddDataToBucket
USE MOD_DataStructures,ONLY: AddBucketIDToHashTable
USE MOD_DataStructures,ONLY: PrintHashTable
USE MOD_DataStructures,ONLY: CreateHashTable
!----------------------------------------------------------------------------------------------------------------------!
USE MOD_DataStructures,ONLY: GetBucketID
USE MOD_DataStructures,ONLY: CheckBucketIDIsInHashTable
!----------------------------------------------------------------------------------------------------------------------!
IMPLICIT NONE
!----------------------------------------------------------------------------------------------------------------------!
! FORMAL ARGUMENTS
!----------------------------------------------------------------------------------------------------------------------!
CHARACTER(LEN=*),INTENT(IN)               :: InputFile
INTEGER,ALLOCATABLE,INTENT(OUT)           :: ElementsType(:)
INTEGER,ALLOCATABLE,INTENT(OUT)           :: ElementsToNodes(:,:)
REAL,ALLOCATABLE,INTENT(OUT)              :: NodesCoordinates(:,:)
INTEGER,ALLOCATABLE,INTENT(OUT)           :: BCFacesToMark(:)
INTEGER,ALLOCATABLE,INTENT(OUT)           :: BCFacesToNodes(:,:)
INTEGER,ALLOCATABLE,INTENT(OUT)           :: BCFacesElementType(:)
INTEGER,ALLOCATABLE,INTENT(OUT)           :: BoundaryMark(:)
CHARACTER(LEN=256),ALLOCATABLE,INTENT(IN) :: BoundaryName(:)
LOGICAL,INTENT(IN),OPTIONAL               :: Debug
!----------------------------------------------------------------------------------------------------------------------!
! LOCAL VARIABLES
!----------------------------------------------------------------------------------------------------------------------!
LOGICAL :: DebugMeshImport = .FALSE.
!----------------------------------------------------------------------------------------------------------------------!
INTEGER :: iExt
INTEGER :: iLine
INTEGER :: UNIT_FILE
INTEGER :: STAT_FILE
!----------------------------------------------------------------------------------------------------------------------!
INTEGER :: iElem
INTEGER :: iBCFace
INTEGER :: NodeID
INTEGER :: ElemID
INTEGER :: nNodes
INTEGER :: nElems
INTEGER :: nBCFaces
!----------------------------------------------------------------------------------------------------------------------!
INTEGER :: ii
INTEGER :: nTags
INTEGER :: ElemType
INTEGER :: PhysicalDimension
INTEGER :: PhysicalEntityNumber
INTEGER :: GeometricalEntityNumber
INTEGER :: nBoundaries
INTEGER :: nPhysicalEntities
INTEGER :: nElemNodes
INTEGER :: nBCFacesNodes
INTEGER :: IndexBoundaryMark
LOGICAL :: Flag
REAL    :: Coords(1:3)
!----------------------------------------------------------------------------------------------------------------------!
INTEGER,ALLOCATABLE :: NodeIDs(:)
CHARACTER(LEN=256)  :: PhysicalEntityName
!----------------------------------------------------------------------------------------------------------------------!
TYPE(tBucket),ALLOCATABLE :: PhysicalEntityNumberToIndexMap(:)
!----------------------------------------------------------------------------------------------------------------------!
TYPE(tLinkedList)             :: NodesToCoordinatesList
TYPE(tLinkedList)             :: ElementsToNodesList
TYPE(tLinkedList)             :: BCFacesToNodesList
TYPE(tArrayREAL)              :: aNodeData
TYPE(tArrayINTEGER)           :: aElemData
TYPE(tArrayINTEGER)           :: aBCFaceData
TYPE(tLinkedListNode),POINTER :: aNodeToCoordinates
TYPE(tLinkedListNode),POINTER :: aElementToNodes
TYPE(tLinkedListNode),POINTER :: aBCFaceToNodes
!----------------------------------------------------------------------------------------------------------------------!
CHARACTER(LEN=256) :: TextLine
CHARACTER(LEN=256) :: ErrorMessage
CHARACTER(LEN=256) :: FunctionName = "DataImport_GMSH"
!----------------------------------------------------------------------------------------------------------------------!
CHARACTER(LEN=:),ALLOCATABLE :: StrL
CHARACTER(LEN=:),ALLOCATABLE :: StrR
!----------------------------------------------------------------------------------------------------------------------!

! Mesh File
iExt = INDEX(InputFile,'.',BACK=.TRUE.)

! Check if input file is a parameter file
IF(InputFile(iExt+1:iExt+3) .NE. 'msh') THEN
  ErrorMessage = "No GMSH file provided"
  CALL PrintError(__STAMP__,ErrorMessage,ModuleName,FunctionName)
END IF

! Opening GMSH input file
STAT_FILE = 0
OPEN(&
  NEWUNIT = UNIT_FILE,   &
  FILE    = InputFile,   &
  STATUS  = "OLD",       &
  ACTION  = "READ",      &
  ACCESS  = "SEQUENTIAL",&
  IOSTAT  = STAT_FILE)
IF (STAT_FILE .NE. 0) THEN
  ErrorMessage = "Error opening GMSH file"
  CALL PrintError(__STAMP__,ErrorMessage,ModuleName,FunctionName)
END IF

STAT_FILE = 0
DO
  READ(UNIT_FILE,'(A)',IOSTAT=STAT_FILE) TextLine
  IF (STAT_FILE .NE. 0) THEN
    EXIT
  END IF
  !--------------------------------------------------!
  ! READING: $PhysicalNames
  !--------------------------------------------------!
  IF (TRIM(TextLine) .EQ. "$PhysicalNames") THEN
    nBoundaries = SIZE(BoundaryName)

    ! Creating PhysicalEntityNumberToIndexMap
    CALL CreateHashTable(PhysicalEntityNumberToIndexMap,nBoundaries,1)

    IF (ALLOCATED(BoundaryMark) .EQV. .TRUE.) THEN
      DEALLOCATE(BoundaryMark)
    END IF
    ALLOCATE(BoundaryMark(1:nBoundaries))
    READ(UNIT_FILE,'(A)',IOSTAT=STAT_FILE) TextLine
    IF (STAT_FILE .NE. 0) THEN
      EXIT
    END IF
    READ(TextLine,*) nPhysicalEntities
    DO iLine=1,nPhysicalEntities
      READ(UNIT_FILE,'(A)',IOSTAT=STAT_FILE) TextLine
      IF (STAT_FILE .NE. 0) THEN
        EXIT
      END IF
      ! Physical Dimension
      TextLine = ADJUSTL(TextLine)
      CALL SplitString(TRIM(TextLine)," ",StrL,StrR)
      READ(StrL,*) PhysicalDimension
      ! Physical Entity Number
      StrR = ADJUSTL(StrR)
      CALL SplitString(TRIM(StrR)," ",StrL,StrR)
      READ(StrL,*) PhysicalEntityNumber
      ! Physical Entity Name
      READ(StrR,*) PhysicalEntityName
      
      SELECT CASE(PP_nDims)
        CASE(2)
          IF (PhysicalDimension .EQ. 1) THEN
            Flag = .FALSE.
            DO ii=1,nBoundaries
              IF (TRIM(PhysicalEntityName) .EQ. TRIM(BoundaryName(ii))) THEN
                Flag = .TRUE.
                BoundaryMark(ii) = ii
                CALL AddBucketIDToHashTable(PhysicalEntityNumberToIndexMap,ii,(/PhysicalEntityNumber/))
              END IF
            END DO
            IF (Flag .EQV. .FALSE.) THEN
              ErrorMessage = "GMSH Boundary Condition not found in parameter file"
              CALL PrintError(__STAMP__,ErrorMessage,ModuleName,FunctionName)
            END IF
          END IF
        CASE(3)
          IF (PhysicalDimension .EQ. 2) THEN
            Flag = .FALSE.
            DO ii=1,nBoundaries
              IF (TRIM(PhysicalEntityName) .EQ. TRIM(BoundaryName(ii))) THEN
                Flag = .TRUE.
                BoundaryMark(ii) = ii
                CALL AddBucketIDToHashTable(PhysicalEntityNumberToIndexMap,ii,(/PhysicalEntityNumber/))
              END IF
            END DO
            IF (Flag .EQV. .FALSE.) THEN
              ErrorMessage = "GMSH Boundary Condition not found in parameter file"
              CALL PrintError(__STAMP__,ErrorMessage,ModuleName,FunctionName)
            END IF
          END IF
        CASE DEFAULT
          ErrorMessage = "MeshDimension not implemented"
          CALL PrintError(__STAMP__,ErrorMessage,ModuleName,FunctionName)
      END SELECT
      
    END DO
    EXIT
  END IF
END DO

STAT_FILE = 0
DO
  READ(UNIT_FILE,'(A)',IOSTAT=STAT_FILE) TextLine
  IF (STAT_FILE .NE. 0) THEN
    EXIT
  END IF
  !--------------------------------------------------!
  ! READING: $Nodes
  !--------------------------------------------------!
  IF (TRIM(TextLine) .EQ. "$Nodes") THEN
    READ(UNIT_FILE,'(A)',IOSTAT=STAT_FILE) TextLine
    IF (STAT_FILE .NE. 0) THEN
      EXIT
    END IF
    READ(TextLine,*) nNodes
    DO iLine=1,nNodes
      READ(UNIT_FILE,'(A)',IOSTAT=STAT_FILE) TextLine
      IF (STAT_FILE .NE. 0) THEN
        EXIT
      END IF
      TextLine = ADJUSTL(TextLine)
      CALL SplitString(TRIM(TextLine)," ",StrL,StrR)
      READ(StrL,*) NodeID
      READ(StrR,*) Coords
      ! Allocating linked list bucket
      IF (ALLOCATED(aNodeData%Data)) THEN
        DEALLOCATE(aNodeData%Data)
      END IF
      ALLOCATE(aNodeData%Data(1:3))
      aNodeData%Data(1:3) = Coords(1:3)
      CALL CreateLinkedListNode(aNodeToCoordinates,NodeID,aNodeData)
      CALL AddLinkedListNode(aNodeToCoordinates,NodesToCoordinatesList)
    END DO
    EXIT
  END IF
END DO

STAT_FILE = 0
DO
  READ(UNIT_FILE,'(A)',IOSTAT=STAT_FILE) TextLine
  IF (STAT_FILE .NE. 0) THEN
    EXIT
  END IF
  !--------------------------------------------------!
  ! READING: $Elements
  !--------------------------------------------------!
  IF (TRIM(TextLine) .EQ. "$Elements") THEN
    READ(UNIT_FILE,'(A)',IOSTAT=STAT_FILE) TextLine
    IF (STAT_FILE .NE. 0) THEN
      EXIT
    END IF
    READ(TextLine,*) nElems
    DO iLine=1,nElems
      READ(UNIT_FILE,'(A)',IOSTAT=STAT_FILE) TextLine
      IF (STAT_FILE .NE. 0) THEN
        EXIT
      END IF
      ! 1: ElemID
      TextLine = ADJUSTL(TextLine)
      CALL SplitString(TRIM(TextLine)," ",StrL,StrR)
      READ(StrL,*) ElemID
      ! 2: ElemType
      ! ElemType={1,2,3,4,5,6,7}={line,triangle,quadrangle,tetrahedron,hexahedron,prism,pyramid}
      StrR = ADJUSTL(StrR)
      CALL SplitString(TRIM(StrR)," ",StrL,StrR)
      READ(StrL,*) ElemType
      IF (CheckElemTypeExists(ElemType) .EQV. .FALSE.)  THEN
        ErrorMessage = "Element Type not implemented "
        CALL PrintError(__STAMP__,ErrorMessage,ModuleName,FunctionName)
      END IF
      nElemNodes = nElemNodes_GMSH(ElemType)
      ! 3: nTags
      StrR = ADJUSTL(StrR)
      CALL SplitString(TRIM(StrR)," ",StrL,StrR)
      READ(StrL,*) nTags
      IF (nTags .GT. 2) THEN
        ErrorMessage = "Only 2 Tags are admitted"
        CALL PrintError(__STAMP__,ErrorMessage,ModuleName,FunctionName)
      END IF
      ! 4: Tag1: Physical Entity Number
      StrR = ADJUSTL(StrR)
      CALL SplitString(TRIM(StrR)," ",StrL,StrR)
      READ(StrL,*) PhysicalEntityNumber
      ! 5: Tag2: Geometrical Entity Number
      StrR = ADJUSTL(StrR)
      CALL SplitString(TRIM(StrR)," ",StrL,StrR)
      READ(StrL,*) GeometricalEntityNumber
      ! 6+: NodeID(1:nElemNodes)
      IF (ALLOCATED(NodeIDs)) THEN
        DEALLOCATE(NodeIDs)
      END IF
      ALLOCATE(NodeIDs(1:nElemNodes))
      READ(StrR,*) NodeIDs(1:nElemNodes)
      SELECT CASE(PP_nDims)
        CASE(2)
          SELECT CASE (ElemType)
            CASE(1) ! ElemType={1}={line}
              ! Allocating linked list bucket
              IF (ALLOCATED(aBCFaceData%Data)) THEN
                DEALLOCATE(aBCFaceData%Data)
              END IF
              ALLOCATE(aBCFaceData%Data(1:nElemNodes))      
              aBCFaceData%Data(1:nElemNodes) = NodeIDs(1:nElemNodes)
              IF (CheckBucketIDIsInHashTable(PhysicalEntityNumberToIndexMap,(/PhysicalEntityNumber/)) .EQV. .TRUE.) THEN
                IndexBoundaryMark = GetBucketID(PhysicalEntityNumberToIndexMap,(/PhysicalEntityNumber/))
              END IF
              CALL CreateLinkedListNode(aBCFaceToNodes,IndexBoundaryMark,aBCFaceData)
              CALL AddLinkedListNode(aBCFaceToNodes,BCFacesToNodesList)
            CASE(2,3) ! ElemType={2,3}={triangle,quadrangle}
              ! Allocating linked list bucket
              IF (ALLOCATED(aElemData%Data)) THEN
                DEALLOCATE(aElemData%Data)
              END IF
              ALLOCATE(aElemData%Data(1:nElemNodes))
              aElemData%Data(1:nElemNodes) = NodeIDs(1:nElemNodes)
              CALL CreateLinkedListNode(aElementToNodes,ElemType,aElemData)
              CALL AddLinkedListNode(aElementToNodes,ElementsToNodesList)
            CASE DEFAULT
              ! ElemType={1,2,3}={line,triangle,quadrangle}
              ErrorMessage = "Element Type not implemented "
              CALL PrintError(__STAMP__,ErrorMessage,ModuleName,FunctionName)
          END SELECT
        CASE(3)
          SELECT CASE (ElemType)
            CASE(2,3) ! ElemType={2,3}={triangle,quadrangle}
              ! Allocating linked list bucket
              IF (ALLOCATED(aBCFaceData%Data)) THEN
                DEALLOCATE(aBCFaceData%Data)
              END IF
              ALLOCATE(aBCFaceData%Data(1:nElemNodes))
              aBCFaceData%Data(1:nElemNodes) = NodeIDs(1:nElemNodes)
              IF (CheckBucketIDIsInHashTable(PhysicalEntityNumberToIndexMap,(/PhysicalEntityNumber/)) .EQV. .TRUE.) THEN
                IndexBoundaryMark = GetBucketID(PhysicalEntityNumberToIndexMap,(/PhysicalEntityNumber/))
              END IF
              CALL CreateLinkedListNode(aBCFaceToNodes,IndexBoundaryMark,aBCFaceData)
              CALL AddLinkedListNode(aBCFaceToNodes,BCFacesToNodesList)
            CASE(4,5,6,7) ! ElemType={4,5,6,7}={tetrahedron,hexahedron,prism,pyramid}
              ! Allocating linked list bucket
              IF (ALLOCATED(aElemData%Data)) THEN
                DEALLOCATE(aElemData%Data)
              END IF
              ALLOCATE(aElemData%Data(1:nElemNodes))
              aElemData%Data(1:nElemNodes) = NodeIDs(1:nElemNodes)
              CALL CreateLinkedListNode(aElementToNodes,ElemType,aElemData)
              CALL AddLinkedListNode(aElementToNodes,ElementsToNodesList)
            CASE DEFAULT
              ! ElemType={2,3,4,5,6,7}={triangle,quadrangle,tetrahedron,hexahedron,prism,pyramid}
              ErrorMessage = "Element Type not implemented "
              CALL PrintError(__STAMP__,ErrorMessage,ModuleName,FunctionName)
          END SELECT
        CASE DEFAULT
          ErrorMessage = "MeshDimension not implemented"
          CALL PrintError(__STAMP__,ErrorMessage,ModuleName,FunctionName)
      END SELECT
    END DO
    EXIT
  END IF
END DO

IF (PRESENT(Debug)) THEN
  DebugMeshImport = Debug
END IF

IF (DebugMeshImport .EQV. .TRUE.) THEN
  WRITE(UNIT_SCREEN,*)
  WRITE(UNIT_SCREEN,*)
  WRITE(UNIT_SCREEN,*) "======================================================="
  WRITE(UNIT_SCREEN,*) "Printing Nodes-Coordinates List"
  WRITE(UNIT_SCREEN,*) "======================================================="
  CALL PrintLinkedList(NodesToCoordinatesList)
  WRITE(UNIT_SCREEN,*)
  WRITE(UNIT_SCREEN,*)
  WRITE(UNIT_SCREEN,*) "======================================================="
  WRITE(UNIT_SCREEN,*) "Printing Elements-Nodes List"
  WRITE(UNIT_SCREEN,*) "======================================================="
  CALL PrintLinkedList(ElementsToNodesList)
  WRITE(UNIT_SCREEN,*)
  WRITE(UNIT_SCREEN,*)
  WRITE(UNIT_SCREEN,*) "======================================================="
  WRITE(UNIT_SCREEN,*) "Printing BCFaces-Nodes List"
  WRITE(UNIT_SCREEN,*) "======================================================="
  CALL PrintLinkedList(BCFacesToNodesList)
END IF

! Exporting NodesToCoordinatesList to array
CALL CountLinkedListNodes(NodesToCoordinatesList,nNodes)

IF (ALLOCATED(NodesCoordinates) .EQV. .TRUE.) THEN
  DEALLOCATE(NodesCoordinates)
END IF
ALLOCATE(NodesCoordinates(1:3,1:nNodes))

aNodeToCoordinates => NodesToCoordinatesList%FirstLinkedListNode
DO WHILE(ASSOCIATED(aNodeToCoordinates))
  CALL GetLinkedListNode(aNodeToCoordinates,NodeID,aNodeData)
  NodesCoordinates(1:3,NodeID) = aNodeData%Data(1:3)
  aNodeToCoordinates => aNodeToCoordinates%NextLinkedListNode
END DO

! Exporting ElementsToNodesList to array
! For PP_nDims=2, MAX(nElemNodes)=MAX({3,4})=4
! For PP_nDims=3, MAX(nElemNodes)=MAX({4,5,6,8})=8
SELECT CASE(PP_nDims)
  CASE(2)
    nElemNodes = 4
  CASE(3)
    nElemNodes = 8
  CASE DEFAULT
    ErrorMessage = "MeshDimension not implemented"
    CALL PrintError(__STAMP__,ErrorMessage,ModuleName,FunctionName)
END SELECT
CALL CountLinkedListNodes(ElementsToNodesList,nElems)
IF (ALLOCATED(ElementsToNodes) .EQV. .TRUE.) THEN
  DEALLOCATE(ElementsToNodes)
END IF
IF (ALLOCATED(ElementsType) .EQV. .TRUE.) THEN
  DEALLOCATE(ElementsType)
END IF
ALLOCATE(ElementsToNodes(1:nElemNodes,1:nElems))
ALLOCATE(ElementsType(1:nElems))

ElementsToNodes = -1

iElem = 0
aElementToNodes => ElementsToNodesList%FirstLinkedListNode
DO WHILE(ASSOCIATED(aElementToNodes))
  iElem = iElem+1
  CALL GetLinkedListNode(aElementToNodes,ElemType,aElemData)
  nElemNodes = SIZE(aElemData%Data)
  ElementsType(iElem) = ElemType
  ElementsToNodes(1:nElemNodes,iElem) = aElemData%Data(1:nElemNodes)
  aElementToNodes => aElementToNodes%NextLinkedListNode
END DO

! Exporting BCFacesToNodesList to array
! For PP_nDims=2, MAX(nBCFacesNodes)=MAX({2})=2
! For PP_nDims=3, MAX(nBCFacesNodes)=MAX({3,4})=4
SELECT CASE(PP_nDims)
  CASE(2)
    nBCFacesNodes = 2
  CASE(3)
    nBCFacesNodes = 4
  CASE DEFAULT
    ErrorMessage = "MeshDimension not implemented"
    CALL PrintError(__STAMP__,ErrorMessage,ModuleName,FunctionName)
END SELECT
CALL CountLinkedListNodes(BCFacesToNodesList,nBCFaces)
IF (ALLOCATED(BCFacesToNodes) .EQV. .TRUE.) THEN
  DEALLOCATE(BCFacesToNodes)
END IF
IF (ALLOCATED(BCFacesToMark) .EQV. .TRUE.) THEN
  DEALLOCATE(BCFacesToMark)
END IF
IF (ALLOCATED(BCFacesElementType) .EQV. .TRUE.) THEN
  DEALLOCATE(BCFacesElementType)
END IF
ALLOCATE(BCFacesToNodes(1:nBCFacesNodes,1:nBCFaces))
ALLOCATE(BCFacesToMark(1:nBCFaces))
ALLOCATE(BCFacesElementType(1:nBCFaces))

BCFacesToNodes = -1

iBCFace = 0
aBCFaceToNodes => BCFacesToNodesList%FirstLinkedListNode
DO WHILE(ASSOCIATED(aBCFaceToNodes))
  iBCFace = iBCFace+1
  CALL GetLinkedListNode(aBCFaceToNodes,PhysicalEntityNumber,aBCFaceData)
  nBCFacesNodes = SIZE(aBCFaceData%Data)
  SELECT CASE(PP_nDims)
    CASE(2)
      BCFacesElementType(iBCFace) = 1
    CASE(3)
      SELECT CASE(nBCFacesNodes)
        CASE(3)
          BCFacesElementType(iBCFace) = 2
        CASE(4)
          BCFacesElementType(iBCFace) = 3
      END SELECT
    CASE DEFAULT
      ErrorMessage = "MeshDimension not implemented"
      CALL PrintError(__STAMP__,ErrorMessage,ModuleName,FunctionName)
  END SELECT
  BCFacesToMark(iBCFace) = PhysicalEntityNumber
  BCFacesToNodes(1:nBCFacesNodes,iBCFace) = aBCFaceData%Data(1:nBCFacesNodes)
  aBCFaceToNodes => aBCFaceToNodes%NextLinkedListNode
END DO

CLOSE(UNIT_FILE)

!----------------------------------------------------------------------------------------------------------------------!
END SUBROUTINE DataImport_GMSH
!======================================================================================================================!
!
!
!
!======================================================================================================================!
FUNCTION nElemNodes_GMSH(ElemType) RESULT(nElemNodes)
!----------------------------------------------------------------------------------------------------------------------!
IMPLICIT NONE
!----------------------------------------------------------------------------------------------------------------------!
! FORMAL ARGUMENTS
!----------------------------------------------------------------------------------------------------------------------!
INTEGER,INTENT(IN)  :: ElemType
!----------------------------------------------------------------------------------------------------------------------!
INTEGER :: nElemNodes
!----------------------------------------------------------------------------------------------------------------------!
! LOCAL VARIABLES
!----------------------------------------------------------------------------------------------------------------------!
INTEGER :: idx(1)
INTEGER :: MASK_INDEX(1:7)
LOGICAL :: MASK_ElemType(1:7,1:3) = .FALSE.
!----------------------------------------------------------------------------------------------------------------------!

! In the first column we stored ElemType
MASK_ElemType(1:7,1) = .TRUE.

! Find the indices where vector GMSH_ElemType(:,1) entries are equal to ElemType
MASK_INDEX = FINDLOC(GMSH_ElemType,VALUE=ElemType,MASK=MASK_ElemType,DIM=2)

! nElemNodes
idx = PACK(GMSH_ElemType(:,2),MASK_INDEX .NE. 0)
nElemNodes = idx(1)

!----------------------------------------------------------------------------------------------------------------------!
END FUNCTION nElemNodes_GMSH
!======================================================================================================================!
!
!
!
!======================================================================================================================!
FUNCTION nElemFaces_GMSH(ElemType) RESULT(nElemFaces)
!----------------------------------------------------------------------------------------------------------------------!
IMPLICIT NONE
!----------------------------------------------------------------------------------------------------------------------!
! FORMAL ARGUMENTS
!----------------------------------------------------------------------------------------------------------------------!
INTEGER,INTENT(IN)  :: ElemType
!----------------------------------------------------------------------------------------------------------------------!
INTEGER :: nElemFaces
!----------------------------------------------------------------------------------------------------------------------!
! LOCAL VARIABLES
!----------------------------------------------------------------------------------------------------------------------!
INTEGER :: idx(1)
INTEGER :: MASK_INDEX(1:7)
LOGICAL :: MASK_ElemType(1:7,1:3) = .FALSE.
!----------------------------------------------------------------------------------------------------------------------!

! In the first column we stored ElemType
MASK_ElemType(1:7,1) = .TRUE.

! Find the indices where vector GMSH_ElemType(:,1) entries are equal to ElemType
MASK_INDEX = FINDLOC(GMSH_ElemType,VALUE=ElemType,MASK=MASK_ElemType,DIM=2)

! nElemFaces
idx = PACK(GMSH_ElemType(:,3),MASK_INDEX .NE. 0)
nElemFaces = idx(1)

!----------------------------------------------------------------------------------------------------------------------!
END FUNCTION nElemFaces_GMSH
!======================================================================================================================!
!
!
!
!======================================================================================================================!
FUNCTION CheckElemTypeExists(ElemType) RESULT (Flag)
!----------------------------------------------------------------------------------------------------------------------!
IMPLICIT NONE
!----------------------------------------------------------------------------------------------------------------------!
! FORMAL ARGUMENTS
!----------------------------------------------------------------------------------------------------------------------!
INTEGER,INTENT(IN) :: ElemType
!----------------------------------------------------------------------------------------------------------------------!
LOGICAL            :: Flag
!----------------------------------------------------------------------------------------------------------------------!
! LOCAL VARIABLES
!----------------------------------------------------------------------------------------------------------------------!
LOGICAL :: MASK_ElemType(1:7,1:3) = .FALSE.
!----------------------------------------------------------------------------------------------------------------------!

! In the first column we stored ElemType
MASK_ElemType(1:7,1) = .TRUE.

Flag = (.NOT. ALL(FINDLOC(GMSH_ElemType,VALUE=ElemType,MASK=MASK_ElemType,DIM=1) .EQ. 0))

!----------------------------------------------------------------------------------------------------------------------!
END FUNCTION CheckElemTypeExists
!======================================================================================================================!
!
!
!
!----------------------------------------------------------------------------------------------------------------------!
END MODULE MOD_DataImport_GMSH
!======================================================================================================================!
