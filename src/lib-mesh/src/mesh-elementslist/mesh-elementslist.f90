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
MODULE MOD_MeshElementsList
!----------------------------------------------------------------------------------------------------------------------!
USE MOD_GLOBAL_vars
!----------------------------------------------------------------------------------------------------------------------!
IMPLICIT NONE
!----------------------------------------------------------------------------------------------------------------------!
PRIVATE
!----------------------------------------------------------------------------------------------------------------------!
INTERFACE InitializeMeshElementsList
  MODULE PROCEDURE InitializeMeshElementsList
END INTERFACE

INTERFACE CreateElementsList
  MODULE PROCEDURE CreateElementsList
END INTERFACE

INTERFACE SetUniqueElemID
  MODULE PROCEDURE SetUniqueElemID
END INTERFACE

INTERFACE SetUniqueNodes
  MODULE PROCEDURE SetUniqueNodes
END INTERFACE

INTERFACE SetUniqueSideID
  MODULE PROCEDURE SetUniqueSideID
END INTERFACE
!----------------------------------------------------------------------------------------------------------------------!
PUBLIC :: InitializeMeshElementsList
PUBLIC :: CreateElementsList
PUBLIC :: SetUniqueElemID
PUBLIC :: SetUniqueSideID
PUBLIC :: SetUniqueNodes
!----------------------------------------------------------------------------------------------------------------------!
CHARACTER(LEN=256) :: ModuleName = "MeshElementsList"
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
SUBROUTINE InitializeMeshElementsList()
!----------------------------------------------------------------------------------------------------------------------!
USE MOD_ConfigFilesTools
!----------------------------------------------------------------------------------------------------------------------!
USE MOD_MeshElementsList_vars,ONLY: ParametersMeshElementsList
USE MOD_MeshElementsList_vars,ONLY: InitializeMeshElementsListIsDone
!----------------------------------------------------------------------------------------------------------------------!
IMPLICIT NONE
!----------------------------------------------------------------------------------------------------------------------!
! FORMAL ARGUMENTS
!----------------------------------------------------------------------------------------------------------------------!
! LOCAL VARIABLES
!----------------------------------------------------------------------------------------------------------------------!
CHARACTER(LEN=256) :: Header
!----------------------------------------------------------------------------------------------------------------------!

IF (InitializeMeshElementsListIsDone) THEN
  SWRITE(UNIT_SCREEN,*) "InitializeMeshElementsList not ready to be called or already called."
  RETURN
END IF

Header = "INITIALIZING MESH ELEMENT LIST MODULE..."
CALL PrintHeader(Header)

ParametersMeshElementsList%DebugMeshElementsList = GetLogical('DebugMeshElementsList','.FALSE.')

InitializeMeshElementsListIsDone = .TRUE.

!----------------------------------------------------------------------------------------------------------------------!
END SUBROUTINE InitializeMeshElementsList
!======================================================================================================================!
!
!
!
!======================================================================================================================!
SUBROUTINE CreateElementsList(Debug)
!----------------------------------------------------------------------------------------------------------------------!
USE MOD_DataStructures,ONLY: LowerCase
!----------------------------------------------------------------------------------------------------------------------!
USE MOD_MeshMain_vars,ONLY: MeshConstructionMethod
!----------------------------------------------------------------------------------------------------------------------!
IMPLICIT NONE
!----------------------------------------------------------------------------------------------------------------------!
! FORMAL ARGUMENTS
!----------------------------------------------------------------------------------------------------------------------!
LOGICAL,INTENT(IN),OPTIONAL :: Debug
!----------------------------------------------------------------------------------------------------------------------!
! LOCAL VARIABLES
!----------------------------------------------------------------------------------------------------------------------!
LOGICAL :: DebugMode
!----------------------------------------------------------------------------------------------------------------------!
CHARACTER(LEN=256) :: ErrorMessage
CHARACTER(LEN=256) :: FunctionName = "CreateElementsList"
!----------------------------------------------------------------------------------------------------------------------!

IF (PRESENT(Debug) .EQV. .TRUE.) THEN
  DebugMode = Debug
ELSE 
  DebugMode = .FALSE.
END IF

SELECT CASE(LowerCase(MeshConstructionMethod))
  CASE('mesh-import')
    CALL CreateElementsListFromMeshImport(Debug=DebugMode)
  CASE('mesh-builtin')
    SELECT CASE(PP_nDims)
      CASE(2)
        CALL CreateElementsListFromMeshBuiltIn2D(Debug=DebugMode)
      CASE(3)
        CALL CreateElementsListFromMeshBuiltIn3D(Debug=DebugMode)
    END SELECT
  CASE DEFAULT
    ErrorMessage = "Mesh Creation Procedure is Unknown"
    CALL PrintError(__STAMP__,ErrorMessage,ModuleName,FunctionName)
END SELECT

! ! ! ! WARNING
! ! ! ! WARNING
! ! ! ! WARNING
! ! ! 
! ! ! CALL ConvertHEXA2PRISM()
! ! ! 
! ! ! CALL SetUniqueNodes(Debug=DebugMode)
! ! ! CALL SetUniqueElemID(Debug=DebugMode)
! ! ! CALL SetUniqueSideID(Debug=DebugMode)
! ! ! 
! ! ! ! WARNING
! ! ! ! WARNING
! ! ! ! WARNING

!----------------------------------------------------------------------------------------------------------------------!
END SUBROUTINE CreateElementsList
!======================================================================================================================!
!
!
!
!======================================================================================================================!
SUBROUTINE CreateElementsListFromMeshBuiltIn2D(Debug)
!----------------------------------------------------------------------------------------------------------------------!
USE MOD_DataStructures,ONLY: LowerCase
!----------------------------------------------------------------------------------------------------------------------!
USE MOD_MeshBuiltIn_vars,ONLY: nBoxElems2D
!----------------------------------------------------------------------------------------------------------------------!
USE MOD_MeshMain_vars,ONLY: PP_NGeo
USE MOD_MeshMain_vars,ONLY: tElem
USE MOD_MeshMain_vars,ONLY: tSide
USE MOD_MeshMain_vars,ONLY: tNodePtr
USE MOD_MeshMain_vars,ONLY: ElemList
USE MOD_MeshMain_vars,ONLY: MeshInfo
USE MOD_MeshMain_vars,ONLY: MeshData_BoundaryMark
USE MOD_MeshMain_vars,ONLY: MeshData_ElementsToElementType
USE MOD_MeshMain_vars,ONLY: MeshData_ElementsToNodesCoordinates2D
!----------------------------------------------------------------------------------------------------------------------!
USE MOD_MeshMainMethods,ONLY: CreateBC
USE MOD_MeshMainMethods,ONLY: CreateElem
USE MOD_MeshMainMethods,ONLY: CreateNode
USE MOD_MeshMainMethods,ONLY: AddDataToNode
USE MOD_MeshMainMethods,ONLY: AddElemToList
USE MOD_MeshMainMethods,ONLY: CreateElem_CGNS
USE MOD_MeshMainMethods,ONLY: PrintElemList
!----------------------------------------------------------------------------------------------------------------------!
USE MOD_Mesh_CGNS_Definitions
!----------------------------------------------------------------------------------------------------------------------!
IMPLICIT NONE
!----------------------------------------------------------------------------------------------------------------------!
! FORMAL ARGUMENTS
!----------------------------------------------------------------------------------------------------------------------!
LOGICAL,INTENT(IN),OPTIONAL :: Debug
!----------------------------------------------------------------------------------------------------------------------!
! LOCAL VARIABLES
!----------------------------------------------------------------------------------------------------------------------!
INTEGER :: i, j
INTEGER :: ii, jj
INTEGER :: iNode
INTEGER :: nTabIn
INTEGER :: iSide
INTEGER :: ElemID
INTEGER :: NodeID
INTEGER :: nElems
INTEGER :: nNodes
INTEGER :: iVertex
INTEGER :: ElemType
INTEGER :: nElemNodes
INTEGER :: nElemFaces
LOGICAL :: DebugMode
LOGICAL :: IsOnBoundary
REAL    :: Coords(1:3)
!----------------------------------------------------------------------------------------------------------------------!
INTEGER :: VertexMap(1:4) = (/1,2,4,3/)
!----------------------------------------------------------------------------------------------------------------------!
TYPE(tElem),POINTER        :: aElem
TYPE(tSide),POINTER        :: aSide
TYPE(tNodePtr),ALLOCATABLE :: MeshNodes(:)
TYPE(tNodePtr),ALLOCATABLE :: ElemNodes(:)
!----------------------------------------------------------------------------------------------------------------------!
REAL                :: CalcTimeIni
REAL                :: CalcTimeEnd
CHARACTER(LEN=256)  :: Header
CHARACTER(LEN=256)  :: ElapsedTime
!----------------------------------------------------------------------------------------------------------------------!

nTabIn = 2
Header = "CREATING ELEMENTS LIST"
CALL PrintMessage(Header,nTabIn=nTabIn)

CalcTimeIni = RunningTime()

nElems = MeshInfo%nElems
nNodes = MeshInfo%nNodes

IF (PRESENT(Debug) .EQV. .TRUE.) THEN
  DebugMode = Debug
ELSE 
  DebugMode = .FALSE.
END IF

IF (ALLOCATED(MeshNodes)) THEN
  DEALLOCATE(MeshNodes)
END IF
ALLOCATE(MeshNodes(1:nNodes))

! Create MeshNodes pointers
NodeID = 0
ElemID = 0
DO jj=1,nBoxElems2D(2)
  DO ii=1,nBoxElems2D(1)
    ElemID = ElemID+1
    DO j=0,PP_NGeo
      DO i=0,PP_NGeo
        ! Create Pointers to Mesh Nodes
        NodeID = NodeID+1
        Coords(1:3) = 0.0
        Coords(1:2) = MeshData_ElementsToNodesCoordinates2D(1:2,i,j,ElemID)
        CALL CreateNode(MeshNodes(NodeID)%Node)
        CALL AddDataToNode(MeshNodes(NodeID)%Node,NodeID,Coords(1:3))

        ! Flag Boundary Sides (CGNS convention)
        MeshNodes(NodeID)%Node%BCFlag = 0
        ! CGNS Face 1: Y-
        IF ((jj .EQ. 1) .AND. (j .EQ. 0)) THEN
          MeshNodes(NodeID)%Node%BCFlag = MeshNodes(NodeID)%Node%BCFlag+1
        END IF
        ! CGNS Face 2: X+
        IF ((ii .EQ. nBoxElems2D(1)) .AND. (i .EQ. PP_NGeo)) THEN
          MeshNodes(NodeID)%Node%BCFlag = MeshNodes(NodeID)%Node%BCFlag+20
        END IF
        ! CGNS Face 3: Y+
        IF ((jj .EQ. nBoxElems2D(2)) .AND. (j .EQ. PP_NGeo)) THEN
          MeshNodes(NodeID)%Node%BCFlag = MeshNodes(NodeID)%Node%BCFlag+300
        END IF
        ! CGNS Face 4: X-
        IF ((ii .EQ. 1) .AND. (i .EQ. 0)) THEN
          MeshNodes(NodeID)%Node%BCFlag = MeshNodes(NodeID)%Node%BCFlag+4000
        END IF
      END DO
    END DO
  END DO
END DO

!--------------------------------------------------!
! VertexMap
!--------------------------------------------------!
! This array allows us to traverse the nodes
! created by a tensor-product of nodes, but
! following the CGNS ordering convention
!--------------------------------------------------!

! Create Elements and Sides
ElemID = 0
NodeID = 0
DO jj=1,nBoxElems2D(2)
  DO ii=1,nBoxElems2D(1)
    ElemID = ElemID+1
    ElemType = MeshData_ElementsToElementType(ElemID)
    CALL GetnElemNodes(ElemType,nElemNodes)
    IF (ALLOCATED(ElemNodes)) THEN
      DEALLOCATE(ElemNodes)
    END IF
    ALLOCATE(ElemNodes(1:nElemNodes))
    DO iVertex=1,nElemNodes
      NodeID = NodeID+1
      ElemNodes(VertexMap(iVertex))%Node => MeshNodes(NodeID)%Node
    END DO
    CALL CreateElem_CGNS(aElem,ElemType,PP_NGeo,ElemNodes,ElemID)
    CALL AddElemToList(aElem,ElemList)
  END DO
END DO

! Add Boundary Condition Information to Sides
aElem => ElemList%FirstElem
DO WHILE(ASSOCIATED(aElem))
  aSide => aElem%FirstSide
  ElemType = aElem%ElemType
  DO WHILE(ASSOCIATED(aSide))
    CALL GetnElemFaces(ElemType,nElemFaces)
    DO iSide=1,nElemFaces
      IsOnBoundary = .TRUE.
      DO iNode=1,aSide%nNodes
        IF ((MOD(aSide%Nodes(iNode)%Node%BCFlag,10**iSide)/(10**(iSide-1))) .NE. iSide) THEN
          IsOnBoundary = .FALSE.
          EXIT
        END IF
      END DO
      IF (IsOnBoundary .EQV. .TRUE.) THEN
        CALL CreateBC(aSide%BC)
        aSide%BC%BCMark = MeshData_BoundaryMark(iSide)
      END IF
    END DO
    aSide => aSide%NextElemSide
  END DO
  aElem => aElem%NextElem
END DO

IF (ALLOCATED(MeshNodes)) THEN
  DEALLOCATE(MeshNodes)
END IF
IF (ALLOCATED(ElemNodes)) THEN
  DEALLOCATE(ElemNodes)
END IF
IF (ALLOCATED(MeshData_ElementsToNodesCoordinates2D)) THEN
  DEALLOCATE(MeshData_ElementsToNodesCoordinates2D)
END IF

CALL SetUniqueNodes(Debug=DebugMode)
CALL SetUniqueElemID(Debug=DebugMode)
CALL SetUniqueSideID(Debug=DebugMode)

IF (DebugMode .EQV. .TRUE.) THEN
  WRITE(UNIT_SCREEN,*)
  WRITE(UNIT_SCREEN,*)
  WRITE(UNIT_SCREEN,*) "======================================================="
  WRITE(UNIT_SCREEN,*) "Printing Elements-Nodes List and Local Faces-Nodes List"
  WRITE(UNIT_SCREEN,*) "======================================================="
  CALL PrintElemList()
END IF

CalcTimeEnd = RunningTime()
CALL ComputeRuntime(CalcTimeEnd-CalcTimeIni,ElapsedTime)
Header = "Elapsed Time"
CALL PrintAnalyze(Header,ElapsedTime,nTabIn=4)

!----------------------------------------------------------------------------------------------------------------------!
END SUBROUTINE CreateElementsListFromMeshBuiltIn2D
!======================================================================================================================!
!
!
!
!======================================================================================================================!
SUBROUTINE CreateElementsListFromMeshBuiltIn3D(Debug)
!----------------------------------------------------------------------------------------------------------------------!
USE MOD_DataStructures,ONLY: LowerCase
!----------------------------------------------------------------------------------------------------------------------!
USE MOD_MeshBuiltIn_vars,ONLY: nBoxElems3D
!----------------------------------------------------------------------------------------------------------------------!
USE MOD_MeshMain_vars,ONLY: PP_NGeo
USE MOD_MeshMain_vars,ONLY: tElem
USE MOD_MeshMain_vars,ONLY: tSide
USE MOD_MeshMain_vars,ONLY: tNodePtr
USE MOD_MeshMain_vars,ONLY: ElemList
USE MOD_MeshMain_vars,ONLY: MeshInfo
USE MOD_MeshMain_vars,ONLY: ExtrusionDirection
USE MOD_MeshMain_vars,ONLY: ElemNodeIDSortingForExtrusion
USE MOD_MeshMain_vars,ONLY: MeshData_BoundaryMark
USE MOD_MeshMain_vars,ONLY: MeshData_ElementsToElementType
USE MOD_MeshMain_vars,ONLY: MeshData_ElementsToNodesCoordinates3D
!----------------------------------------------------------------------------------------------------------------------!
USE MOD_MeshMainMethods,ONLY: CreateBC
USE MOD_MeshMainMethods,ONLY: CreateElem
USE MOD_MeshMainMethods,ONLY: CreateNode
USE MOD_MeshMainMethods,ONLY: AddDataToNode
USE MOD_MeshMainMethods,ONLY: AddElemToList
USE MOD_MeshMainMethods,ONLY: CreateElem_CGNS
USE MOD_MeshMainMethods,ONLY: PrintElemList
!----------------------------------------------------------------------------------------------------------------------!
USE MOD_Mesh_CGNS_Definitions
!----------------------------------------------------------------------------------------------------------------------!
IMPLICIT NONE
!----------------------------------------------------------------------------------------------------------------------!
! FORMAL ARGUMENTS
!----------------------------------------------------------------------------------------------------------------------!
LOGICAL,INTENT(IN),OPTIONAL :: Debug
!----------------------------------------------------------------------------------------------------------------------!
! LOCAL VARIABLES
!----------------------------------------------------------------------------------------------------------------------!
INTEGER :: i, j, k
INTEGER :: ii, jj, kk
INTEGER :: nTabIn
INTEGER :: iNode
INTEGER :: iSide
INTEGER :: ElemID
INTEGER :: NodeID
INTEGER :: nElems
INTEGER :: nNodes
INTEGER :: iVertex
INTEGER :: ElemType
INTEGER :: nElemNodes
INTEGER :: nElemFaces
LOGICAL :: IsOnBoundary
LOGICAL :: DebugMode
REAL    :: Coords(1:3)
!----------------------------------------------------------------------------------------------------------------------!
INTEGER :: VertexMap(1:8) = (/1,2,4,3,5,6,8,7/)
!----------------------------------------------------------------------------------------------------------------------!
TYPE(tElem),POINTER        :: aElem
TYPE(tSide),POINTER        :: aSide
TYPE(tNodePtr),ALLOCATABLE :: MeshNodes(:)
TYPE(tNodePtr),ALLOCATABLE :: ElemNodes(:)
!----------------------------------------------------------------------------------------------------------------------!
REAL                :: CalcTimeIni
REAL                :: CalcTimeEnd
CHARACTER(LEN=256)  :: Header
CHARACTER(LEN=256)  :: ElapsedTime
!----------------------------------------------------------------------------------------------------------------------!

nTabIn = 2
Header = "CREATING ELEMENTS LIST"
CALL PrintMessage(Header,nTabIn=nTabIn)

CalcTimeIni = RunningTime()

nElems = MeshInfo%nElems
nNodes = MeshInfo%nNodes

IF (PRESENT(Debug) .EQV. .TRUE.) THEN
  DebugMode = Debug
ELSE 
  DebugMode = .FALSE.
END IF

IF (ALLOCATED(MeshNodes)) THEN
  DEALLOCATE(MeshNodes)
END IF
ALLOCATE(MeshNodes(1:nNodes))

! Create MeshNodes pointers
ElemID = 0
NodeID = 0
DO kk=1,nBoxElems3D(3)
  DO jj=1,nBoxElems3D(2)
    DO ii=1,nBoxElems3D(1)
      ElemID = ElemID+1
      DO k=0,PP_NGeo
        DO j=0,PP_NGeo
          DO i=0,PP_NGeo
            ! Create Pointers to Mesh Nodes
            NodeID = NodeID+1
            Coords(1:3) = MeshData_ElementsToNodesCoordinates3D(1:3,i,j,k,ElemID)
            CALL CreateNode(MeshNodes(NodeID)%Node)
            CALL AddDataToNode(MeshNodes(NodeID)%Node,NodeID,Coords(1:3))

            ! Flag Boundary Sides (CGNS convention)
            MeshNodes(NodeID)%Node%BCFlag = 0
            ! CGNS Face 1: Z-
            IF ((kk .EQ. 1) .AND. (k .EQ. 0)) THEN
              MeshNodes(NodeID)%Node%BCFlag = MeshNodes(NodeID)%Node%BCFlag+1
            END IF
            ! CGNS Face 2: Y-
            IF ((jj .EQ. 1) .AND. (j .EQ. 0)) THEN
              MeshNodes(NodeID)%Node%BCFlag = MeshNodes(NodeID)%Node%BCFlag+20
            END IF
            ! CGNS Face 3: X+
            IF ((ii .EQ. nBoxElems3D(1)) .AND. (i .EQ. PP_NGeo)) THEN
              MeshNodes(NodeID)%Node%BCFlag = MeshNodes(NodeID)%Node%BCFlag+300
            END IF
            ! CGNS Face 4: Y+
            IF ((jj .EQ. nBoxElems3D(2)) .AND. (j .EQ. PP_NGeo)) THEN
              MeshNodes(NodeID)%Node%BCFlag = MeshNodes(NodeID)%Node%BCFlag+4000
            END IF
            ! CGNS Face 5: X-
            IF ((ii .EQ. 1) .AND. (i .EQ. 0)) THEN
              MeshNodes(NodeID)%Node%BCFlag = MeshNodes(NodeID)%Node%BCFlag+50000
            END IF
            ! CGNS Face 6: Z+
            IF ((kk .EQ. nBoxElems3D(3)) .AND. (k .EQ. PP_NGeo)) THEN
              MeshNodes(NodeID)%Node%BCFlag = MeshNodes(NodeID)%Node%BCFlag+600000
            END IF
          END DO
        END DO
      END DO
    END DO
  END DO
END DO

!--------------------------------------------------!
! VertexMap
!--------------------------------------------------!
! This array allows us to traverse the nodes
! created by a tensor-product of nodes, but
! following the CGNS ordering convention
!--------------------------------------------------!

! Create Elements and Sides
ElemID = 0
NodeID = 0
DO kk=1,nBoxElems3D(3)
  DO jj=1,nBoxElems3D(2)
    DO ii=1,nBoxElems3D(1)
      ElemID = ElemID+1
      ElemType = MeshData_ElementsToElementType(ElemID)
      CALL GetnElemNodes(ElemType,nElemNodes)
      IF (ALLOCATED(ElemNodes)) THEN
        DEALLOCATE(ElemNodes)
      END IF
      ALLOCATE(ElemNodes(1:nElemNodes))
      DO iVertex=1,nElemNodes
        NodeID = NodeID+1
        ! Elements are constructed following CGNS nodes numbering.
        ! This numbering enables to refine anisotropicaly only in z-direction,
        ! like an extrusion in z-direction.
        ! Therefore, the numbering has to be changed such that the anisotropic refining
        ! takes place in the y-direction
        IF (LowerCase(ExtrusionDirection) .EQ. 'y') THEN
          ElemNodes(ElemNodeIDSortingForExtrusion(VertexMap(iVertex)))%Node => MeshNodes(NodeID)%Node
        ELSEIF (LowerCase(ExtrusionDirection) .EQ. 'z') THEN
          ElemNodes(VertexMap(iVertex))%Node => MeshNodes(NodeID)%Node
        END IF
      END DO
      CALL CreateElem_CGNS(aElem,ElemType,PP_NGeo,ElemNodes,ElemID)
      CALL AddElemToList(aElem,ElemList)
    END DO
  END DO
END DO

IF (ALLOCATED(MeshNodes)) THEN
  DEALLOCATE(MeshNodes)
END IF
IF (ALLOCATED(ElemNodes)) THEN
  DEALLOCATE(ElemNodes)
END IF
IF (ALLOCATED(MeshData_ElementsToNodesCoordinates3D)) THEN
  DEALLOCATE(MeshData_ElementsToNodesCoordinates3D)
END IF

! Add Boundary Condition Information to Sides
aElem => ElemList%FirstElem
DO WHILE(ASSOCIATED(aElem))
  aSide => aElem%FirstSide
  ElemType = aElem%ElemType
  DO WHILE(ASSOCIATED(aSide))
    CALL GetnElemFaces(ElemType,nElemFaces)
    DO iSide=1,nElemFaces
      IsOnBoundary = .TRUE.
      DO iNode=1,aSide%nNodes
        IF ((MOD(aSide%Nodes(iNode)%Node%BCFlag,10**iSide)/(10**(iSide-1))) .NE. iSide) THEN
          IsOnBoundary = .FALSE.
          EXIT
        END IF
      END DO
      IF (IsOnBoundary .EQV. .TRUE.) THEN
        CALL CreateBC(aSide%BC)
        aSide%BC%BCMark = MeshData_BoundaryMark(iSide)
      END IF
    END DO
    aSide => aSide%NextElemSide
  END DO
  aElem => aElem%NextElem
END DO

CALL SetUniqueNodes(Debug=DebugMode)
CALL SetUniqueElemID(Debug=DebugMode)
CALL SetUniqueSideID(Debug=DebugMode)

IF (DebugMode .EQV. .TRUE.) THEN
  WRITE(UNIT_SCREEN,*)
  WRITE(UNIT_SCREEN,*)
  WRITE(UNIT_SCREEN,*) "======================================================="
  WRITE(UNIT_SCREEN,*) "Printing Elements-Nodes List and Local Faces-Nodes List"
  WRITE(UNIT_SCREEN,*) "======================================================="
  CALL PrintElemList()
END IF

CalcTimeEnd = RunningTime()
CALL ComputeRuntime(CalcTimeEnd-CalcTimeIni,ElapsedTime)
Header = "Elapsed Time"
CALL PrintAnalyze(Header,ElapsedTime,nTabIn=4)

!----------------------------------------------------------------------------------------------------------------------!
END SUBROUTINE CreateElementsListFromMeshBuiltIn3D
!======================================================================================================================!
!
!
!
!======================================================================================================================!
SUBROUTINE CreateElementsListFromMeshImport(Debug)
!----------------------------------------------------------------------------------------------------------------------!
USE MOD_DataStructures,ONLY: LowerCase
USE MOD_DataStructures,ONLY: tBucket
USE MOD_DataStructures,ONLY: GetBucketID
USE MOD_DataStructures,ONLY: CheckBucketIDIsInHashTable
USE MOD_DataStructures,ONLY: AddBucketIDToHashTable
USE MOD_DataStructures,ONLY: PrintHashTable
USE MOD_DataStructures,ONLY: CreateHashTable
USE MOD_DataStructures,ONLY: CountLinkedListNodes
!----------------------------------------------------------------------------------------------------------------------!
USE MOD_MeshMain_vars,ONLY: PP_NGeo
USE MOD_MeshMain_vars,ONLY: tElem
USE MOD_MeshMain_vars,ONLY: tSide
USE MOD_MeshMain_vars,ONLY: tNodePtr
USE MOD_MeshMain_vars,ONLY: ElemList
!----------------------------------------------------------------------------------------------------------------------!
USE MOD_MeshMain_vars,ONLY: MeshInfo
USE MOD_MeshMain_vars,ONLY: MeshData_BoundaryMark
USE MOD_MeshMain_vars,ONLY: MeshData_BCFacesToMark
USE MOD_MeshMain_vars,ONLY: MeshData_BCFacesToNodes
USE MOD_MeshMain_vars,ONLY: MeshData_BCFacesToElementType
USE MOD_MeshMain_vars,ONLY: MeshData_ElementsToElementType
USE MOD_MeshMain_vars,ONLY: MeshData_ElementsToNodes
USE MOD_MeshMain_vars,ONLY: MeshData_NodesCoordinates
!----------------------------------------------------------------------------------------------------------------------!
USE MOD_MeshMainMethods,ONLY: CreateBC
USE MOD_MeshMainMethods,ONLY: CreateElem
USE MOD_MeshMainMethods,ONLY: CreateNode
USE MOD_MeshMainMethods,ONLY: RemoveElem
USE MOD_MeshMainMethods,ONLY: AddDataToNode
USE MOD_MeshMainMethods,ONLY: AddElemToList
USE MOD_MeshMainMethods,ONLY: PrintElemList
USE MOD_MeshMainMethods,ONLY: CountBCFaces
USE MOD_MeshMainMethods,ONLY: CreateElem_CGNS
!----------------------------------------------------------------------------------------------------------------------!
USE MOD_Mesh_CGNS_Definitions
!----------------------------------------------------------------------------------------------------------------------!
IMPLICIT NONE
!----------------------------------------------------------------------------------------------------------------------!
! FORMAL ARGUMENTS
!----------------------------------------------------------------------------------------------------------------------!
LOGICAL,INTENT(IN),OPTIONAL :: Debug
!----------------------------------------------------------------------------------------------------------------------!
! LOCAL VARIABLES
!----------------------------------------------------------------------------------------------------------------------!
INTEGER :: iVertex
INTEGER :: iBCFace
INTEGER :: ElemID
INTEGER :: NodeID
INTEGER :: nData
INTEGER :: nTabIn
INTEGER :: nElems
INTEGER :: nNodes
INTEGER :: ElemType
INTEGER :: nElemNodes
INTEGER :: nBCFaces
INTEGER :: nBCFaces_EDGE
INTEGER :: nBCFaces_TRI
INTEGER :: nBCFaces_QUAD
INTEGER :: iBCIndex
INTEGER :: BCMarker
INTEGER :: BCFaceNodeID_EDGE(1:2)
INTEGER :: BCFaceNodeID_TRI(1:3)
INTEGER :: BCFaceNodeID_QUAD(1:4)
INTEGER :: FaceNodeID_EDGE(1:2)
INTEGER :: FaceNodeID_TRI(1:3)
INTEGER :: FaceNodeID_QUAD(1:4)
LOGICAL :: IsOnBoundary
LOGICAL :: DebugMode
REAL    :: Coords(1:3)
!----------------------------------------------------------------------------------------------------------------------!
TYPE(tElem),POINTER        :: aElem
TYPE(tSide),POINTER        :: aSide
TYPE(tNodePtr),ALLOCATABLE :: MeshNodes(:)
TYPE(tNodePtr),ALLOCATABLE :: ElemNodes(:)
!----------------------------------------------------------------------------------------------------------------------!
TYPE(tBucket),ALLOCATABLE :: HashTable_EDGE(:)
TYPE(tBucket),ALLOCATABLE :: HashTable_TRI(:)
TYPE(tBucket),ALLOCATABLE :: HashTable_QUAD(:)
!----------------------------------------------------------------------------------------------------------------------!
REAL                :: CalcTimeIni
REAL                :: CalcTimeEnd
CHARACTER(LEN=256)  :: Header
CHARACTER(LEN=256)  :: ElapsedTime
!----------------------------------------------------------------------------------------------------------------------!

nTabIn = 2
Header = "CREATING ELEMENTS LIST"
CALL PrintMessage(Header,nTabIn=nTabIn)

CalcTimeIni = RunningTime()

nElems = MeshInfo%nElems
nNodes = MeshInfo%nNodes

IF (PRESENT(Debug) .EQV. .TRUE.) THEN
  DebugMode = Debug
ELSE 
  DebugMode = .FALSE.
END IF

!--------------------------------------------------!
! ELEMTYPE_EDGE2
!--------------------------------------------------!
IF (ANY(MeshData_BCFacesToElementType(:) .EQ. ELEMTYPE_EDGE2)) THEN
  nBCFaces_EDGE = 0
  nBCFaces = SIZE(MeshData_BCFacesToNodes,2)
  DO iBCFace=1,nBCFaces
    IF (MeshData_BCFacesToElementType(iBCFace) .EQ. ELEMTYPE_EDGE2) THEN
      nBCFaces_EDGE = nBCFaces_EDGE+1
    END IF
  END DO
  CALL GetnElemNodes(ELEMTYPE_EDGE2,nData)
  CALL CreateHashTable(HashTable_EDGE,nBCFaces_EDGE,nData)
  DO iBCFace=1,nBCFaces
    IF (MeshData_BCFacesToElementType(iBCFace) .EQ. ELEMTYPE_EDGE2) THEN
      BCMarker                   = MeshData_BCFacesToMark(iBCFace)
      BCFaceNodeID_EDGE(1:nData) = MeshData_BCFacesToNodes(1:nData,iBCFace)
      CALL AddBucketIDToHashTable(HashTable_EDGE,BCMarker,BCFaceNodeID_EDGE)
    END IF
  END DO
  IF (DebugMode .EQV. .TRUE.) THEN
    WRITE(UNIT_SCREEN,*)
    WRITE(UNIT_SCREEN,"(A)") "======================================================="
    WRITE(UNIT_SCREEN,"(A)") "HashTable_EDGE of Boundary Faces"
    WRITE(UNIT_SCREEN,"(A)") "======================================================="
    CALL PrintHashTable(HashTable_EDGE)
  END IF
END IF

!--------------------------------------------------!
! ELEMTYPE_TRI3
!--------------------------------------------------!
IF (ANY(MeshData_BCFacesToElementType(:) .EQ. ELEMTYPE_TRI3)) THEN
  nBCFaces_TRI = 0
  nBCFaces = SIZE(MeshData_BCFacesToNodes,2)
  DO iBCFace=1,nBCFaces
    IF (MeshData_BCFacesToElementType(iBCFace) .EQ. ELEMTYPE_TRI3) THEN
      nBCFaces_TRI = nBCFaces_TRI+1
    END IF
  END DO
  CALL GetnElemNodes(ELEMTYPE_TRI3,nData)
  CALL CreateHashTable(HashTable_TRI,nBCFaces_TRI,nData)
  DO iBCFace=1,nBCFaces
    IF (MeshData_BCFacesToElementType(iBCFace) .EQ. ELEMTYPE_TRI3) THEN
      BCMarker                  = MeshData_BCFacesToMark(iBCFace)
      BCFaceNodeID_TRI(1:nData) = MeshData_BCFacesToNodes(1:nData,iBCFace)
      CALL AddBucketIDToHashTable(HashTable_TRI,BCMarker,BCFaceNodeID_TRI)
    END IF
  END DO
  IF (DebugMode .EQV. .TRUE.) THEN
    WRITE(UNIT_SCREEN,*)
    WRITE(UNIT_SCREEN,"(2X,A)") "======================================================="
    WRITE(UNIT_SCREEN,"(2X,A)") "HashTable_TRI of Boundary Faces"
    WRITE(UNIT_SCREEN,"(2X,A)") "======================================================="
    CALL PrintHashTable(HashTable_TRI)
  END IF
END IF

!--------------------------------------------------!
! ELEMTYPE_QUAD4
!--------------------------------------------------!
IF (ANY(MeshData_BCFacesToElementType(:) .EQ. ELEMTYPE_QUAD4)) THEN
  nBCFaces_QUAD = 0
  nBCFaces = SIZE(MeshData_BCFacesToNodes,2)
  DO iBCFace=1,nBCFaces
    IF (MeshData_BCFacesToElementType(iBCFace) .EQ. ELEMTYPE_QUAD4) THEN
      nBCFaces_QUAD = nBCFaces_QUAD+1
    END IF
  END DO
  CALL GetnElemNodes(ELEMTYPE_QUAD4,nData)
  CALL CreateHashTable(HashTable_QUAD,nBCFaces_QUAD,nData)
  DO iBCFace=1,nBCFaces
    IF (MeshData_BCFacesToElementType(iBCFace) .EQ. ELEMTYPE_QUAD4) THEN
      BCMarker                   = MeshData_BCFacesToMark(iBCFace)
      BCFaceNodeID_QUAD(1:nData) = MeshData_BCFacesToNodes(1:nData,iBCFace)
      CALL AddBucketIDToHashTable(HashTable_QUAD,BCMarker,BCFaceNodeID_QUAD)
    END IF
  END DO
  IF (DebugMode .EQV. .TRUE.) THEN
    WRITE(UNIT_SCREEN,*)
    WRITE(UNIT_SCREEN,"(2X,A)") "======================================================="
    WRITE(UNIT_SCREEN,"(2X,A)") "HashTable_QUAD of Boundary Faces"
    WRITE(UNIT_SCREEN,"(2X,A)") "======================================================="
    CALL PrintHashTable(HashTable_QUAD)
  END IF
END IF

ALLOCATE(MeshNodes(1:nNodes))

! Create MeshNodes pointers
DO NodeID=1,nNodes
  ! Create Pointers to Mesh Nodes
  Coords(1:3) = 0.0
  Coords(1:PP_nDims) = MeshData_NodesCoordinates(1:PP_nDims,NodeID)
  CALL CreateNode(MeshNodes(NodeID)%Node)
  CALL AddDataToNode(MeshNodes(NodeID)%Node,NodeID,Coords(1:3))
END DO

! Create Elements and Sides
DO ElemID=1,nElems
  ElemType = MeshData_ElementsToElementType(ElemID)
  CALL GetnElemNodes(ElemType,nElemNodes)
  IF (ALLOCATED(ElemNodes)) THEN
    DEALLOCATE(ElemNodes)
  END IF
  ALLOCATE(ElemNodes(1:nElemNodes))
  DO iVertex=1,nElemNodes
    NodeID = MeshData_ElementsToNodes(iVertex,ElemID)
    ElemNodes(iVertex)%Node => MeshNodes(NodeID)%Node
  END DO
  CALL CreateElem_CGNS(aElem,ElemType,PP_NGeo,ElemNodes,ElemID)
  CALL AddElemToList(aElem,ElemList)
END DO

! Add Boundary Condition Information to Sides
aElem => ElemList%FirstElem
DO WHILE(ASSOCIATED(aElem))
  aSide => aElem%FirstSide
  DO WHILE(ASSOCIATED(aSide))
    SELECT CASE(aSide%nNodes)
      CASE(2) ! EDGE2
        FaceNodeID_EDGE(1) = aSide%Nodes(1)%Node%NodeID
        FaceNodeID_EDGE(2) = aSide%Nodes(2)%Node%NodeID
        IsOnBoundary = .FALSE.
        IF (ALLOCATED(HashTable_EDGE)) THEN
          IsOnBoundary = CheckBucketIDIsInHashTable(HashTable_EDGE,FaceNodeID_EDGE(1:2))
        END IF
        IF (IsOnBoundary .EQV. .TRUE.) THEN
          BCMarker = GetBucketID(HashTable_EDGE,FaceNodeID_EDGE)
          iBCIndex = MINLOC(ABS(MeshData_BoundaryMark-BCMarker),1)
          CALL CreateBC(aSide%BC)
          aSide%BC%BCMark = MeshData_BoundaryMark(iBCIndex)
        END IF
      CASE(3) ! TRI3
        FaceNodeID_TRI(1) = aSide%Nodes(1)%Node%NodeID
        FaceNodeID_TRI(2) = aSide%Nodes(2)%Node%NodeID
        FaceNodeID_TRI(3) = aSide%Nodes(3)%Node%NodeID
        IsOnBoundary = .FALSE.
        IF (ALLOCATED(HashTable_TRI)) THEN
          IsOnBoundary = CheckBucketIDIsInHashTable(HashTable_TRI,FaceNodeID_TRI(1:3))
        END IF
        IF (IsOnBoundary .EQV. .TRUE.) THEN
          BCMarker = GetBucketID(HashTable_TRI,FaceNodeID_TRI)
          iBCIndex = MINLOC(ABS(MeshData_BoundaryMark-BCMarker),1)
          CALL CreateBC(aSide%BC)
          aSide%BC%BCMark = MeshData_BoundaryMark(iBCIndex)
        END IF
      CASE(4) ! QUAD4
        FaceNodeID_QUAD(1) = aSide%Nodes(1)%Node%NodeID
        FaceNodeID_QUAD(2) = aSide%Nodes(2)%Node%NodeID
        FaceNodeID_QUAD(3) = aSide%Nodes(3)%Node%NodeID
        FaceNodeID_QUAD(4) = aSide%Nodes(4)%Node%NodeID
        IsOnBoundary = .FALSE.
        IF (ALLOCATED(HashTable_QUAD)) THEN
          IsOnBoundary = CheckBucketIDIsInHashTable(HashTable_QUAD,FaceNodeID_QUAD(1:4))
        END IF
        IF (IsOnBoundary .EQV. .TRUE.) THEN
          BCMarker = GetBucketID(HashTable_QUAD,FaceNodeID_QUAD)
          iBCIndex = MINLOC(ABS(MeshData_BoundaryMark-BCMarker),1)
          CALL CreateBC(aSide%BC)
          aSide%BC%BCMark = MeshData_BoundaryMark(iBCIndex)
        END IF
    END SELECT
    aSide => aSide%NextElemSide
  END DO
  aElem => aElem%NextElem
END DO

IF (ALLOCATED(MeshNodes)) THEN
  DEALLOCATE(MeshNodes)
END IF

CALL SetUniqueNodes(Debug=DebugMode)
CALL SetUniqueElemID(Debug=DebugMode)
CALL SetUniqueSideID(Debug=DebugMode)

IF (DebugMode .EQV. .TRUE.) THEN
  WRITE(UNIT_SCREEN,*)
  WRITE(UNIT_SCREEN,*)
  WRITE(UNIT_SCREEN,*) "======================================================="
  WRITE(UNIT_SCREEN,*) "Printing Elements-Nodes List and Local Faces-Nodes List"
  WRITE(UNIT_SCREEN,*) "======================================================="
  CALL PrintElemList()
END IF

CalcTimeEnd = RunningTime()
CALL ComputeRuntime(CalcTimeEnd-CalcTimeIni,ElapsedTime)
Header = "Elapsed Time"
CALL PrintAnalyze(Header,ElapsedTime,nTabIn=4)

!----------------------------------------------------------------------------------------------------------------------!
END SUBROUTINE CreateElementsListFromMeshImport
!======================================================================================================================!
!
!
!
!======================================================================================================================!
SUBROUTINE SetUniqueElemID(Debug)
!----------------------------------------------------------------------------------------------------------------------!
USE MOD_MeshMain_vars,ONLY: tElem
USE MOD_MeshMain_vars,ONLY: ElemList
!----------------------------------------------------------------------------------------------------------------------!
IMPLICIT NONE
!----------------------------------------------------------------------------------------------------------------------!
! FORMAL ARGUMENTS
!----------------------------------------------------------------------------------------------------------------------!
LOGICAL,INTENT(IN),OPTIONAL :: Debug
!----------------------------------------------------------------------------------------------------------------------!
! LOCAL VARIABLES
!----------------------------------------------------------------------------------------------------------------------!
INTEGER :: ElemID
INTEGER :: nElems
LOGICAL :: DebugMode
!----------------------------------------------------------------------------------------------------------------------!
TYPE(tElem),POINTER :: aElem
!----------------------------------------------------------------------------------------------------------------------!

IF (PRESENT(Debug) .EQV. .TRUE.) THEN
  DebugMode = Debug
ELSE 
  DebugMode = .FALSE.
END IF

ElemID = 0
aElem => ElemList%FirstElem
DO WHILE (ASSOCIATED(aElem))
  ElemID = ElemID+1
  aElem%ElemID = ElemID
  aElem => aElem%NextElem
END DO
nElems = ElemID

IF (DebugMode .EQV. .TRUE.) THEN
  WRITE(UNIT_SCREEN,*)
  WRITE(UNIT_SCREEN,*) "======================================================="
  WRITE(UNIT_SCREEN,*) "FUNCTION: SET_UNIQUE_ELEMID"
  WRITE(UNIT_SCREEN,*) "======================================================="
  WRITE(UNIT_SCREEN,"(2X,A,I0)") "nElems        : ", nElems
  WRITE(UNIT_SCREEN,*) "======================================================="
END IF

!----------------------------------------------------------------------------------------------------------------------!
END SUBROUTINE SetUniqueElemID
!======================================================================================================================!
!
!
!
!======================================================================================================================!
SUBROUTINE SetUniqueSideID(Debug)
!----------------------------------------------------------------------------------------------------------------------!
USE MOD_MeshMain_vars,ONLY: tElem
USE MOD_MeshMain_vars,ONLY: tSide
USE MOD_MeshMain_vars,ONLY: ElemList
!----------------------------------------------------------------------------------------------------------------------!
USE MOD_MeshMainMethods,ONLY: CountElems
!----------------------------------------------------------------------------------------------------------------------!
USE MOD_DataStructures,ONLY: QuickSort
USE MOD_DataStructures,ONLY: QuickSortArray
!----------------------------------------------------------------------------------------------------------------------!
USE MOD_Mesh_CGNS_Definitions
!----------------------------------------------------------------------------------------------------------------------!
IMPLICIT NONE
!----------------------------------------------------------------------------------------------------------------------!
! FORMAL ARGUMENTS
!----------------------------------------------------------------------------------------------------------------------!
LOGICAL,INTENT(IN),OPTIONAL :: Debug
!----------------------------------------------------------------------------------------------------------------------!
! LOCAL VARIABLES
!----------------------------------------------------------------------------------------------------------------------!
INTEGER :: iNode
INTEGER :: iFace
INTEGER :: ElemID
INTEGER :: FaceID
INTEGER :: nFaces
INTEGER :: nElems
INTEGER :: nNodesOnFace
INTEGER :: nElemFaces
INTEGER :: nMaxFaceNodes
INTEGER :: nMaxElemSides
INTEGER :: LocSide
INTEGER :: ElemType
INTEGER :: nFacesTotal
!----------------------------------------------------------------------------------------------------------------------!
INTEGER :: nElems_EDGE
INTEGER :: nElems_TRI
INTEGER :: nElems_QUAD
INTEGER :: nElems_TETRA
INTEGER :: nElems_HEXA
INTEGER :: nElems_PRISM
INTEGER :: nElems_PYRA
LOGICAL :: DebugMode
!----------------------------------------------------------------------------------------------------------------------!
INTEGER,ALLOCATABLE :: FaceNodesID(:)
!----------------------------------------------------------------------------------------------------------------------!
TYPE(tElem),POINTER :: aElem
TYPE(tSide),POINTER :: aSide
!----------------------------------------------------------------------------------------------------------------------!
INTEGER,ALLOCATABLE :: FacesToNodesArray1(:,:)
INTEGER,ALLOCATABLE :: FacesToNodesArray2(:,:)
!----------------------------------------------------------------------------------------------------------------------!

IF (PRESENT(Debug) .EQV. .TRUE.) THEN
  DebugMode = Debug
ELSE 
  DebugMode = .FALSE.
END IF

! Count Elements
CALL CountElems(ElemList,nElems)

! For PP_nDims=2, MAX(nMaxFaceNodes)=MAX({2})=2
! For PP_nDims=2, MAX(nMaxElemSides)=MAX({3,4})=4
! For PP_nDims=3, MAX(nMaxFaceNodes)=MAX({3,4})=4
! For PP_nDims=3, MAX(nMaxElemSides)=MAX({4,5,6})=6
SELECT CASE(PP_nDims)
  CASE(2)
    nMaxFaceNodes = 2
    nMaxElemSides = 4
  CASE(3)
    nMaxFaceNodes = 4
    nMaxElemSides = 6
END SELECT

nElems_EDGE  = 0
nElems_TRI   = 0
nElems_QUAD  = 0
nElems_TETRA = 0
nElems_HEXA  = 0
nElems_PRISM = 0
nElems_PYRA  = 0

aElem => ElemList%FirstElem
DO WHILE (ASSOCIATED(aElem))
  ElemType = aElem%ElemType
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
  aElem => aElem%NextElem
END DO

nFacesTotal = 0

CALL GetnElemFaces(ELEMTYPE_EDGE2,nElemFaces)
nFacesTotal = nFacesTotal + nElemFaces*nElems_EDGE

CALL GetnElemFaces(ELEMTYPE_TRI3,nElemFaces)
nFacesTotal = nFacesTotal + nElemFaces*nElems_TRI

CALL GetnElemFaces(ELEMTYPE_QUAD4,nElemFaces)
nFacesTotal = nFacesTotal + nElemFaces*nElems_QUAD

CALL GetnElemFaces(ELEMTYPE_TETRA4,nElemFaces)
nFacesTotal = nFacesTotal + nElemFaces*nElems_TETRA

CALL GetnElemFaces(ELEMTYPE_HEXA8,nElemFaces)
nFacesTotal = nFacesTotal + nElemFaces*nElems_HEXA

CALL GetnElemFaces(ELEMTYPE_PRISM6,nElemFaces)
nFacesTotal = nFacesTotal + nElemFaces*nElems_PRISM

CALL GetnElemFaces(ELEMTYPE_PYRA5,nElemFaces)
nFacesTotal = nFacesTotal + nElemFaces*nElems_PYRA

IF (ALLOCATED(FacesToNodesArray1) .EQV. .TRUE.) THEN
  DEALLOCATE(FacesToNodesArray1)
END IF
ALLOCATE(FacesToNodesArray1(1:nFacesTotal,1:nMaxFaceNodes+2))

IF (ALLOCATED(FacesToNodesArray2) .EQV. .TRUE.) THEN
  DEALLOCATE(FacesToNodesArray2)
END IF
ALLOCATE(FacesToNodesArray2(1:nMaxElemSides,1:nElems))

IF (ALLOCATED(FaceNodesID)) THEN
  DEALLOCATE(FaceNodesID)
END IF
ALLOCATE(FaceNodesID(1:nMaxFaceNodes))

FaceID = 0
aElem => ElemList%FirstElem
DO WHILE (ASSOCIATED(aElem))
  aSide => aElem%FirstSide
  DO WHILE (ASSOCIATED(aSide))
    FaceID = FaceID+1
    FaceNodesID(1:nMaxFaceNodes) = 0    
    DO iNode=1,aSide%nNodes
      FaceNodesID(iNode) = aSide%Nodes(iNode)%Node%NodeID
    END DO
    CALL GetnNodesOnFace(aElem%ElemType,aSide%LocSide,nNodesOnFace)
    CALL QuickSort(FaceNodesID(1:nNodesOnFace))
    FacesToNodesArray1(FaceID,:) = 0
    FacesToNodesArray1(FaceID,1:nNodesOnFace)  = FaceNodesID(1:nNodesOnFace)
    FacesToNodesArray1(FaceID,nMaxFaceNodes+1) = aElem%ElemID
    FacesToNodesArray1(FaceID,nMaxFaceNodes+2) = aSide%LocSide
    aSide => aSide%NextElemSide
  END DO
  aElem => aElem%NextElem
END DO

CALL QuickSortArray(FacesToNodesArray1,nMaxFaceNodes)

iFace  = 1
FaceID = 0
DO WHILE (iFace .LE. nFacesTotal)
  ElemID  = FacesToNodesArray1(iFace,nMaxFaceNodes+1)
  LocSide = FacesToNodesArray1(iFace,nMaxFaceNodes+2)
  IF (iFace .GT. 1) THEN
    IF (ALL(FacesToNodesArray1(iFace,1:nMaxFaceNodes) .EQ. FacesToNodesArray1(iFace-1,1:nMaxFaceNodes))) THEN
      FacesToNodesArray2(LocSide,ElemID) = FaceID
      iFace = iFace+1
      CYCLE
    ELSE
      FaceID = FaceID+1
    END IF
  ELSE
    FaceID = FaceID+1
  END IF
  FacesToNodesArray2(LocSide,ElemID) = FaceID
  iFace = iFace+1
END DO
nFaces = FaceID

aElem => ElemList%FirstElem
DO WHILE (ASSOCIATED(aElem))
  aSide => aElem%FirstSide
  DO WHILE (ASSOCIATED(aSide))
    aSide%SideID = FacesToNodesArray2(aSide%LocSide,aElem%ElemID)
    aSide => aSide%NextElemSide
  END DO
  aElem => aElem%NextElem
END DO

IF (ALLOCATED(FacesToNodesArray1)) THEN
  DEALLOCATE(FacesToNodesArray1)
END IF
IF (ALLOCATED(FacesToNodesArray2)) THEN
  DEALLOCATE(FacesToNodesArray2)
END IF

IF (DebugMode .EQV. .TRUE.) THEN
  WRITE(UNIT_SCREEN,*)
  WRITE(UNIT_SCREEN,*) "======================================================="
  WRITE(UNIT_SCREEN,*) "FUNCTION: SET_UNIQUE_SIDEID"
  WRITE(UNIT_SCREEN,*) "======================================================="
  WRITE(UNIT_SCREEN,"(2X,A,I0)") "nFaces        : ", nFaces
  WRITE(UNIT_SCREEN,*) "======================================================="
END IF

!----------------------------------------------------------------------------------------------------------------------!
END SUBROUTINE SetUniqueSideID
!======================================================================================================================!
!
!
!
!======================================================================================================================!
SUBROUTINE SetUniqueNodes(Debug)
!----------------------------------------------------------------------------------------------------------------------!
USE MOD_MeshMain_vars,ONLY: tElem
USE MOD_MeshMain_vars,ONLY: tNode
USE MOD_MeshMain_vars,ONLY: tNodePtr
USE MOD_MeshMain_vars,ONLY: ElemList
!----------------------------------------------------------------------------------------------------------------------!
USE MOD_DataStructures,ONLY: QuickSortArray
!----------------------------------------------------------------------------------------------------------------------!
IMPLICIT NONE
!----------------------------------------------------------------------------------------------------------------------!
! FORMAL ARGUMENTS
!----------------------------------------------------------------------------------------------------------------------!
LOGICAL,INTENT(IN),OPTIONAL :: Debug
!----------------------------------------------------------------------------------------------------------------------!
! LOCAL VARIABLES
!----------------------------------------------------------------------------------------------------------------------!
INTEGER :: iNode
INTEGER :: jNode
INTEGER :: NodeID
INTEGER :: nNodes
LOGICAL :: DebugMode
!----------------------------------------------------------------------------------------------------------------------!
TYPE(tElem),POINTER :: aElem
TYPE(tNode),POINTER :: aNode
!----------------------------------------------------------------------------------------------------------------------!
INTEGER                    :: nDeletedNodes
REAL                       :: Point1(1:3)
REAL                       :: Point2(1:3)
REAL,ALLOCATABLE           :: NodesCoordinates(:,:)
TYPE(tNodePtr),ALLOCATABLE :: NodesList(:)
!----------------------------------------------------------------------------------------------------------------------!

IF (PRESENT(Debug) .EQV. .TRUE.) THEN
  DebugMode = Debug
ELSE 
  DebugMode = .FALSE.
END IF

! Set marker (tmp) to zero
aElem => ElemList%FirstElem
DO WHILE(ASSOCIATED(aElem))
  CALL SetNodesMarkerToZero(aElem)
  aElem => aElem%NextElem
END DO

! Set marker (tmp) to NodeID
NodeID = 0
aElem => ElemList%FirstElem
DO WHILE(ASSOCIATED(aElem))
  CALL SetNodesMarkerToNodeID(aElem,NodeID)
  aElem => aElem%NextElem
END DO

! Create list of nodes
nNodes = NodeID
ALLOCATE(NodesList(1:nNodes))
DO iNode=1,nNodes
  NULLIFY(NodesList(iNode)%Node)
END DO

! Save all nodes to nodes list
aElem => ElemList%FirstElem
DO WHILE(ASSOCIATED(aElem))
  CALL SaveNodesToNodesList(aElem,NodesList)
  aElem => aElem%NextElem
END DO

! Create nodes coordinates list to remove duplicated nodes
ALLOCATE(NodesCoordinates(1:nNodes,1:4))
DO iNode=1,nNodes
  aNode => NodesList(iNode)%Node
  NodesCoordinates(iNode,1) = aNode%Coords(1)
  NodesCoordinates(iNode,2) = aNode%Coords(2)
  NodesCoordinates(iNode,3) = aNode%Coords(3)
  NodesCoordinates(iNode,4) = REAL(aNode%tmp)
END DO
CALL QuickSortArray(NodesCoordinates,4)

DO iNode=1,nNodes
  NodesList(iNode)%Node%tmp = 0
END DO

NodeID = 0
nDeletedNodes = 0
DO iNode=1,nNodes
  jNode = INT(NodesCoordinates(iNode,4))
  IF (iNode .GT. 1) THEN
    Point1(1:3) = NodesCoordinates(iNode  ,1:3)
    Point2(1:3) = NodesCoordinates(iNode-1,1:3)
    IF (ComparePoints(Point1,Point2) .EQV. .TRUE.) THEN
      nDeletedNodes = nDeletedNodes+1
      NodesCoordinates(iNode,4) = NodesCoordinates(iNode-1,4)
      NodesList(jNode)%Node%tmp    = INT(NodesCoordinates(iNode,4))
      NodesList(jNode)%Node%NodeID = NodeID
      CYCLE
    ELSE
      NodeID = NodeID+1
    END IF
  ELSE
    NodeID = NodeID+1
  END IF
  NodesList(jNode)%Node%tmp    = INT(NodesCoordinates(iNode,4))
  NodesList(jNode)%Node%NodeID = NodeID
END DO

! Nodes point now to nodes with new NodeIDs
aElem => ElemList%FirstElem
DO WHILE(ASSOCIATED(aElem))
  CALL SetNodesToNewNodes(aElem,NodesList)
  aElem => aElem%NextElem
END DO

IF (ALLOCATED(NodesCoordinates)) THEN
  DEALLOCATE(NodesCoordinates)
END IF
IF (ALLOCATED(NodesList)) THEN
  DEALLOCATE(NodesList)
END IF

IF (DebugMode .EQV. .TRUE.) THEN
  WRITE(UNIT_SCREEN,*)
  WRITE(UNIT_SCREEN,*) "======================================================="
  WRITE(UNIT_SCREEN,*) "FUNCTION: SET_UNIQUE_NODES"
  WRITE(UNIT_SCREEN,*) "======================================================="
  WRITE(UNIT_SCREEN,"(2X,A,I0)") "nNodes        : ", nNodes
  WRITE(UNIT_SCREEN,"(2X,A,I0)") "nDeletedNodes : ", nDeletedNodes
  WRITE(UNIT_SCREEN,"(2X,A,I0)") "nUniqueNodes  : ", nNodes-nDeletedNodes
  WRITE(UNIT_SCREEN,*) "======================================================="
END IF

!----------------------------------------------------------------------------------------------------------------------!
END SUBROUTINE SetUniqueNodes
!======================================================================================================================!
!
!
!
!======================================================================================================================!
SUBROUTINE SetNodesMarkerToZero(aElem)
!----------------------------------------------------------------------------------------------------------------------!
USE MOD_MeshMain_vars,ONLY: tElem
USE MOD_MeshMain_vars,ONLY: tSide
!----------------------------------------------------------------------------------------------------------------------!
IMPLICIT NONE
!----------------------------------------------------------------------------------------------------------------------!
! FORMAL ARGUMENTS
!----------------------------------------------------------------------------------------------------------------------!
TYPE(tElem),POINTER,INTENT(INOUT) :: aElem
!----------------------------------------------------------------------------------------------------------------------!
! LOCAL VARIABLES
!----------------------------------------------------------------------------------------------------------------------!
INTEGER :: iNode
!----------------------------------------------------------------------------------------------------------------------!
TYPE(tSide),POINTER :: aSide
!----------------------------------------------------------------------------------------------------------------------!

DO iNode=1,aElem%nNodes
  aElem%Nodes(iNode)%Node%tmp = 0
END DO

aSide => aElem%FirstSide
DO WHILE(ASSOCIATED(aSide))
  DO iNode=1,aSide%nNodes
    aSide%Nodes(iNode)%Node%tmp = 0
  END DO
  aSide => aSide%NextElemSide
END DO

!----------------------------------------------------------------------------------------------------------------------!
END SUBROUTINE SetNodesMarkerToZero
!======================================================================================================================!
!
!
!
!======================================================================================================================!
SUBROUTINE SetNodesMarkerToNodeID(aElem,NodeID)
!----------------------------------------------------------------------------------------------------------------------!
USE MOD_MeshMain_vars,ONLY: tElem
USE MOD_MeshMain_vars,ONLY: tSide
!----------------------------------------------------------------------------------------------------------------------!
USE MOD_MeshMainMethods,ONLY: SetAndCountNodeID
!----------------------------------------------------------------------------------------------------------------------!
IMPLICIT NONE
!----------------------------------------------------------------------------------------------------------------------!
! FORMAL ARGUMENTS
!----------------------------------------------------------------------------------------------------------------------!
TYPE(tElem),POINTER,INTENT(INOUT) :: aElem
INTEGER,INTENT(INOUT)             :: NodeID
!----------------------------------------------------------------------------------------------------------------------!
! LOCAL VARIABLES
!----------------------------------------------------------------------------------------------------------------------!
INTEGER :: iNode
!----------------------------------------------------------------------------------------------------------------------!
TYPE(tSide),POINTER :: aSide
!----------------------------------------------------------------------------------------------------------------------!

DO iNode=1,aElem%nNodes
  CALL SetAndCountNodeID(aElem%Nodes(iNode)%Node%tmp,NodeID)
END DO

aSide => aElem%FirstSide
DO WHILE(ASSOCIATED(aSide))
  DO iNode=1,aSide%nNodes
    CALL SetAndCountNodeID(aSide%Nodes(iNode)%Node%tmp,NodeID)
  END DO
  aSide => aSide%NextElemSide
END DO

!----------------------------------------------------------------------------------------------------------------------!
END SUBROUTINE SetNodesMarkerToNodeID
!======================================================================================================================!
!
!
!
!======================================================================================================================!
SUBROUTINE SaveNodesToNodesList(aElem,NodesList)
!----------------------------------------------------------------------------------------------------------------------!
USE MOD_MeshMain_vars,ONLY: tElem
USE MOD_MeshMain_vars,ONLY: tSide
USE MOD_MeshMain_vars,ONLY: tNodePtr
!----------------------------------------------------------------------------------------------------------------------!
IMPLICIT NONE
!----------------------------------------------------------------------------------------------------------------------!
! FORMAL ARGUMENTS
!----------------------------------------------------------------------------------------------------------------------!
TYPE(tElem),POINTER,INTENT(INOUT) :: aElem
TYPE(tNodePtr),INTENT(INOUT)      :: NodesList(:)
!----------------------------------------------------------------------------------------------------------------------!
! LOCAL VARIABLES
!----------------------------------------------------------------------------------------------------------------------!
INTEGER :: iNode
!----------------------------------------------------------------------------------------------------------------------!
TYPE(tSide),POINTER :: aSide
!----------------------------------------------------------------------------------------------------------------------!

DO iNode=1,aElem%nNodes
  NodesList(aElem%Nodes(iNode)%Node%tmp)%Node => aElem%Nodes(iNode)%Node
END DO

aSide => aElem%FirstSide
DO WHILE(ASSOCIATED(aSide))
  DO iNode=1,aSide%nNodes
    NodesList(aSide%Nodes(iNode)%Node%tmp)%Node => aSide%Nodes(iNode)%Node
  END DO
  aSide => aSide%NextElemSide
END DO

!----------------------------------------------------------------------------------------------------------------------!
END SUBROUTINE SaveNodesToNodesList
!======================================================================================================================!
!
!
!
!======================================================================================================================!
SUBROUTINE SetNodesToNewNodes(aElem,NodesList)
!----------------------------------------------------------------------------------------------------------------------!
USE MOD_MeshMain_vars,ONLY: tElem
USE MOD_MeshMain_vars,ONLY: tSide
USE MOD_MeshMain_vars,ONLY: tNodePtr
!----------------------------------------------------------------------------------------------------------------------!
IMPLICIT NONE
!----------------------------------------------------------------------------------------------------------------------!
! FORMAL ARGUMENTS
!----------------------------------------------------------------------------------------------------------------------!
TYPE(tElem),POINTER,INTENT(INOUT) :: aElem
TYPE(tNodePtr),INTENT(INOUT)      :: NodesList(:)
!----------------------------------------------------------------------------------------------------------------------!
! LOCAL VARIABLES
!----------------------------------------------------------------------------------------------------------------------!
INTEGER :: iNode
!----------------------------------------------------------------------------------------------------------------------!
TYPE(tSide),POINTER :: aSide
!----------------------------------------------------------------------------------------------------------------------!

DO iNode=1,aElem%nNodes
  aElem%Nodes(iNode)%Node => NodesList(aElem%Nodes(iNode)%Node%tmp)%Node
END DO

aSide => aElem%FirstSide
DO WHILE(ASSOCIATED(aSide))
  DO iNode=1,aSide%nNodes
    aSide%Nodes(iNode)%Node => NodesList(aSide%Nodes(iNode)%Node%tmp)%Node
  END DO
  aSide => aSide%NextElemSide
END DO

!----------------------------------------------------------------------------------------------------------------------!
END SUBROUTINE SetNodesToNewNodes
!======================================================================================================================!
!
!
!
!======================================================================================================================!
FUNCTION ComparePoints(Point1,Point2) RESULT(Flag)
!----------------------------------------------------------------------------------------------------------------------!
IMPLICIT NONE
!----------------------------------------------------------------------------------------------------------------------!
! FORMAL ARGUMENTS
!----------------------------------------------------------------------------------------------------------------------!
REAL,INTENT(IN) :: Point1(1:3)
REAL,INTENT(IN) :: Point2(1:3)
LOGICAL         :: Flag
!----------------------------------------------------------------------------------------------------------------------!
! LOCAL VARIABLES
!----------------------------------------------------------------------------------------------------------------------!
REAL            :: ds
!----------------------------------------------------------------------------------------------------------------------!

ds = SQRT(DOT_PRODUCT(Point1-Point2,Point1-Point2))
Flag = .FALSE.
IF (ds .LT. PP_ACCURACY) THEN
  Flag = .TRUE.
END IF

!----------------------------------------------------------------------------------------------------------------------!
END FUNCTION ComparePoints
!======================================================================================================================!
!
!
!
!======================================================================================================================!
SUBROUTINE ConvertHEXA2PRISM(Debug)
!----------------------------------------------------------------------------------------------------------------------!
USE MOD_MeshMain_vars,ONLY: tElem
USE MOD_MeshMain_vars,ONLY: ElemList
!----------------------------------------------------------------------------------------------------------------------!
IMPLICIT NONE
!----------------------------------------------------------------------------------------------------------------------!
! FORMAL ARGUMENTS
!----------------------------------------------------------------------------------------------------------------------!
LOGICAL,INTENT(IN),OPTIONAL :: Debug
!----------------------------------------------------------------------------------------------------------------------!
! LOCAL VARIABLES
!----------------------------------------------------------------------------------------------------------------------!
LOGICAL :: DebugMode
!----------------------------------------------------------------------------------------------------------------------!
TYPE(tElem),POINTER :: aElem
!----------------------------------------------------------------------------------------------------------------------!

IF (PRESENT(Debug) .EQV. .TRUE.) THEN
  DebugMode = Debug
ELSE 
  DebugMode = .FALSE.
END IF

aElem => ElemList%FirstElem
DO WHILE(ASSOCIATED(aElem))
  CALL SplitElem_HEXA8_PRISM6_XY(aElem)
  aElem => aElem%NextElem
END DO

!----------------------------------------------------------------------------------------------------------------------!
END SUBROUTINE ConvertHEXA2PRISM
!======================================================================================================================!
!
!
!
!======================================================================================================================!
SUBROUTINE SplitElem_HEXA8_PRISM6_XY(Elem)
!----------------------------------------------------------------------------------------------------------------------!
USE MOD_MeshMain_vars,ONLY: tElem
USE MOD_MeshMain_vars,ONLY: tSide
USE MOD_MeshMain_vars,ONLY: tElemPtr
USE MOD_MeshMain_vars,ONLY: tNodePtr
USE MOD_MeshMain_vars,ONLY: tSidePtr
USE MOD_MeshMain_vars,ONLY: ElemList
!----------------------------------------------------------------------------------------------------------------------!
USE MOD_MeshMainMethods,ONLY: CountElems
USE MOD_MeshMainMethods,ONLY: CopyBC
USE MOD_MeshMainMethods,ONLY: RemoveElem
USE MOD_MeshMainMethods,ONLY: CreateNode
USE MOD_MeshMainMethods,ONLY: CreateElem_CGNS
!----------------------------------------------------------------------------------------------------------------------!
USE MOD_Mesh_CGNS_Definitions
!----------------------------------------------------------------------------------------------------------------------!
IMPLICIT NONE
!----------------------------------------------------------------------------------------------------------------------!
! FORMAL ARGUMENTS
!----------------------------------------------------------------------------------------------------------------------!
TYPE(tElem),POINTER,INTENT(INOUT) :: Elem
!----------------------------------------------------------------------------------------------------------------------!
! LOCAL VARIABLES
!----------------------------------------------------------------------------------------------------------------------!
INTEGER :: ElemType
INTEGER :: LocSideID
INTEGER :: LocElemID
REAL    :: dh
!----------------------------------------------------------------------------------------------------------------------!
INTEGER,PARAMETER :: nElemFaces  = 5
INTEGER,PARAMETER :: nTotalElem  = 4
INTEGER,PARAMETER :: nElemNodes  = 6
INTEGER,PARAMETER :: nTotalNodes = 10
!----------------------------------------------------------------------------------------------------------------------!
TYPE(tElem),POINTER :: aElem
TYPE(tElem),POINTER :: PrevElem
TYPE(tElem),POINTER :: NextElem
TYPE(tSide),POINTER :: aSide
!----------------------------------------------------------------------------------------------------------------------!
TYPE(tElemPtr) :: NewElems(1:nTotalElem)
TYPE(tNodePtr) :: NewNodes(1:nTotalNodes)
TYPE(tSidePtr) :: NewSides(1:nTotalElem,1:nElemFaces)
TYPE(tNodePtr) :: ElemNodes(1:nElemNodes)
!----------------------------------------------------------------------------------------------------------------------!

! WARNING
! For splitting the HEXA8 element into 4 HEXA8 subelements,
! we use the CGNS-HEXA27 nodes numbering

dh = 0.5

NewNodes(1)%Node => Elem%Nodes(1)%Node
NewNodes(2)%Node => Elem%Nodes(2)%Node
NewNodes(3)%Node => Elem%Nodes(3)%Node
NewNodes(4)%Node => Elem%Nodes(4)%Node
NewNodes(5)%Node => Elem%Nodes(5)%Node
NewNodes(6)%Node => Elem%Nodes(6)%Node
NewNodes(7)%Node => Elem%Nodes(7)%Node
NewNodes(8)%Node => Elem%Nodes(8)%Node

! New midnodes at edges
CALL CreateNode(NewNodes(9)%Node)
CALL CreateNode(NewNodes(10)%Node)

! New midnodes at faces ******** WARNING ******** CENTROID OF QUADRILATERAL ******** WARNING ********
NewNodes(9)%Node%Coords  = dh*(NewNodes(1)%Node%Coords + NewNodes(3)%Node%Coords)
NewNodes(10)%Node%Coords = dh*(NewNodes(5)%Node%Coords + NewNodes(7)%Node%Coords)

!--------------------------------------------------!
! Create Child = 1
!--------------------------------------------------!
LocElemID = 1
ElemType  = ELEMTYPE_PRISM6
ElemNodes(1)%Node => NewNodes(1)%Node
ElemNodes(2)%Node => NewNodes(2)%Node
ElemNodes(3)%Node => NewNodes(9)%Node
ElemNodes(4)%Node => NewNodes(5)%Node
ElemNodes(5)%Node => NewNodes(6)%Node
ElemNodes(6)%Node => NewNodes(10)%Node
CALL CreateElem_CGNS(NewElems(LocElemID)%Elem,ElemType,1,ElemNodes)
NewElems(LocElemID)%Elem%ElemType = ElemType
NewElems(LocElemID)%Elem%Flag     = Elem%Flag
NewElems(LocElemID)%Elem%Level    = Elem%Level

LocSideID = 0
aSide => NewElems(LocElemID)%Elem%FirstSide
DO WHILE (ASSOCIATED(aSide))
  LocSideID = LocSideID+1
  NewSides(LocElemID,LocSideID)%Side => aSide
  aSide => aSide%NextElemSide
END DO

!--------------------------------------------------!
! Create Child = 2
!--------------------------------------------------!
LocElemID = 2
ElemType  = ELEMTYPE_PRISM6
ElemNodes(1)%Node => NewNodes(2)%Node
ElemNodes(2)%Node => NewNodes(3)%Node
ElemNodes(3)%Node => NewNodes(9)%Node
ElemNodes(4)%Node => NewNodes(6)%Node
ElemNodes(5)%Node => NewNodes(7)%Node
ElemNodes(6)%Node => NewNodes(10)%Node
CALL CreateElem_CGNS(NewElems(LocElemID)%Elem,ElemType,1,ElemNodes)
NewElems(LocElemID)%Elem%ElemType = ElemType
NewElems(LocElemID)%Elem%Flag     = Elem%Flag
NewElems(LocElemID)%Elem%Level    = Elem%Level

LocSideID = 0
aSide => NewElems(LocElemID)%Elem%FirstSide
DO WHILE (ASSOCIATED(aSide))
  LocSideID = LocSideID+1
  NewSides(LocElemID,LocSideID)%Side => aSide
  aSide => aSide%NextElemSide
END DO

!--------------------------------------------------!
! Create Child = 3
!--------------------------------------------------!
LocElemID = 3
ElemType  = ELEMTYPE_PRISM6
ElemNodes(1)%Node => NewNodes(3)%Node
ElemNodes(2)%Node => NewNodes(4)%Node
ElemNodes(3)%Node => NewNodes(9)%Node
ElemNodes(4)%Node => NewNodes(7)%Node
ElemNodes(5)%Node => NewNodes(8)%Node
ElemNodes(6)%Node => NewNodes(10)%Node
CALL CreateElem_CGNS(NewElems(LocElemID)%Elem,ElemType,1,ElemNodes)
NewElems(LocElemID)%Elem%ElemType = ElemType
NewElems(LocElemID)%Elem%Flag     = Elem%Flag
NewElems(LocElemID)%Elem%Level    = Elem%Level

LocSideID = 0
aSide => NewElems(LocElemID)%Elem%FirstSide
DO WHILE (ASSOCIATED(aSide))
  LocSideID = LocSideID+1
  NewSides(LocElemID,LocSideID)%Side => aSide
  aSide => aSide%NextElemSide
END DO

!--------------------------------------------------!
! Create Child = 4
!--------------------------------------------------!
LocElemID = 4
ElemType  = ELEMTYPE_PRISM6
ElemNodes(1)%Node => NewNodes(4)%Node
ElemNodes(2)%Node => NewNodes(1)%Node
ElemNodes(3)%Node => NewNodes(9)%Node
ElemNodes(4)%Node => NewNodes(8)%Node
ElemNodes(5)%Node => NewNodes(5)%Node
ElemNodes(6)%Node => NewNodes(10)%Node
CALL CreateElem_CGNS(NewElems(LocElemID)%Elem,ElemType,1,ElemNodes)
NewElems(LocElemID)%Elem%ElemType = ElemType
NewElems(LocElemID)%Elem%Flag     = Elem%Flag
NewElems(LocElemID)%Elem%Level    = Elem%Level

LocSideID = 0
aSide => NewElems(LocElemID)%Elem%FirstSide
DO WHILE (ASSOCIATED(aSide))
  LocSideID = LocSideID+1
  NewSides(LocElemID,LocSideID)%Side => aSide
  aSide => aSide%NextElemSide
END DO

!--------------------------------------------------!
! Set Boundary Conditions
!--------------------------------------------------!
LocSideID = 0
aSide => Elem%FirstSide
DO WHILE (ASSOCIATED(aSide))
  LocSideID = LocSideID+1
  IF (ASSOCIATED(aSide%BC)) THEN
    DO LocElemID=1,nTotalElem
      SELECT CASE (LocElemID)
        CASE(1)
          SELECT CASE (LocSideID)
            CASE(1)
              CALL CopyBC(aSide,NewSides(LocElemID,4)%Side)
            CASE(2)
              CALL CopyBC(aSide,NewSides(LocElemID,1)%Side)
            CASE(6)
              CALL CopyBC(aSide,NewSides(LocElemID,5)%Side)
          END SELECT
        CASE(2)
          SELECT CASE (LocSideID)
            CASE(1)
              CALL CopyBC(aSide,NewSides(LocElemID,4)%Side)
            CASE(3)
              CALL CopyBC(aSide,NewSides(LocElemID,1)%Side)
            CASE(6)
              CALL CopyBC(aSide,NewSides(LocElemID,5)%Side)
          END SELECT
        CASE(3)
          SELECT CASE (LocSideID)
            CASE(1)
              CALL CopyBC(aSide,NewSides(LocElemID,4)%Side)
            CASE(4)
              CALL CopyBC(aSide,NewSides(LocElemID,1)%Side)
            CASE(6)
              CALL CopyBC(aSide,NewSides(LocElemID,5)%Side)
          END SELECT
        CASE(4)
          SELECT CASE (LocSideID)
            CASE(1)
              CALL CopyBC(aSide,NewSides(LocElemID,4)%Side)
            CASE(5)
              CALL CopyBC(aSide,NewSides(LocElemID,1)%Side)
            CASE(6)
              CALL CopyBC(aSide,NewSides(LocElemID,5)%Side)
          END SELECT
      END SELECT
    END DO
  END IF
  aSide => aSide%NextElemSide
END DO

aElem    => Elem
NextElem => Elem%NextElem
PrevElem => Elem%PrevElem

DO LocElemID=1,nTotalElem
  aElem%NextElem          => NewElems(LocElemID)%Elem
  aElem%NextElem%PrevElem => aElem
  aElem                   => aElem%NextElem
END DO

IF (ASSOCIATED(NextElem)) THEN
  aElem%NextElem    => NextElem
  NextElem%PrevElem => aElem
END IF

CALL RemoveElem(Elem,ElemList)

Elem => aElem

IF (ASSOCIATED(PrevElem)) THEN
  NewElems(1)%Elem%PrevElem => PrevElem
  PrevElem%NextElem         => NewElems(1)%Elem
ELSE
  ElemList%FirstElem => NewElems(1)%Elem
END IF

!----------------------------------------------------------------------------------------------------------------------!
END SUBROUTINE SplitElem_HEXA8_PRISM6_XY
!======================================================================================================================!
!
!
!
!----------------------------------------------------------------------------------------------------------------------!
END MODULE MOD_MeshElementsList
!======================================================================================================================!
