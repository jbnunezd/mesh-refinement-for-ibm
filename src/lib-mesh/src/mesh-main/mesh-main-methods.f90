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
MODULE MOD_MeshMainMethods
!----------------------------------------------------------------------------------------------------------------------!
USE MOD_GLOBAL_vars
!----------------------------------------------------------------------------------------------------------------------!
USE MOD_MeshMain_vars
!----------------------------------------------------------------------------------------------------------------------!
IMPLICIT NONE
!----------------------------------------------------------------------------------------------------------------------!
PRIVATE
!----------------------------------------------------------------------------------------------------------------------!
! GLOBAL VARIABLES
!----------------------------------------------------------------------------------------------------------------------!
!
!----------------------------------------------------------------------------------------------------------------------!
INTERFACE CreateElem
  MODULE PROCEDURE CreateElem
END INTERFACE

INTERFACE RemoveElem
  MODULE PROCEDURE RemoveElem
END INTERFACE
!----------------------------------------------------------------------------------------------------------------------!
INTERFACE CreateSide
  MODULE PROCEDURE CreateSide
END INTERFACE

INTERFACE CreateBC
  MODULE PROCEDURE CreateBC
END INTERFACE

INTERFACE CopyBC
  MODULE PROCEDURE CopyBC
END INTERFACE
!----------------------------------------------------------------------------------------------------------------------!
INTERFACE CreateNode
  MODULE PROCEDURE CreateNode
END INTERFACE

INTERFACE AddDataToNode
  MODULE PROCEDURE AddDataToNode
END INTERFACE

INTERFACE GetNodeData
  MODULE PROCEDURE GetNodeData
END INTERFACE
!----------------------------------------------------------------------------------------------------------------------!
INTERFACE CreateElem_CGNS
  MODULE PROCEDURE CreateElem_CGNS
END INTERFACE
!----------------------------------------------------------------------------------------------------------------------!
INTERFACE CountNodes
  MODULE PROCEDURE CountNodes
END INTERFACE

INTERFACE CountElems
  MODULE PROCEDURE CountElems
END INTERFACE

INTERFACE CountElemID
  MODULE PROCEDURE CountElemID
END INTERFACE

INTERFACE CountElemsPerLevel
  MODULE PROCEDURE CountElemsPerLevel
END INTERFACE

INTERFACE CountBCFaces
  MODULE PROCEDURE CountBCFaces
END INTERFACE

INTERFACE SetAndCountNodeID
  MODULE PROCEDURE SetAndCountNodeID
END INTERFACE
!----------------------------------------------------------------------------------------------------------------------!
INTERFACE AddElemToList
  MODULE PROCEDURE AddElemToList
END INTERFACE

INTERFACE PrintElemList
  MODULE PROCEDURE PrintElemList
END INTERFACE

INTERFACE DestructElemList
  MODULE PROCEDURE DestructElemList
END INTERFACE
!----------------------------------------------------------------------------------------------------------------------!
INTERFACE CreateFaceNodes
  MODULE PROCEDURE CreateFaceNodes
END INTERFACE

INTERFACE GetFaceNodesData
  MODULE PROCEDURE GetFaceNodesData
END INTERFACE

INTERFACE AddFaceNodesToList
  MODULE PROCEDURE AddFaceNodesToList
END INTERFACE

INTERFACE PrintFaceNodesList
  MODULE PROCEDURE PrintFaceNodesList
END INTERFACE

INTERFACE DestructFaceNodesList
  MODULE PROCEDURE DestructFaceNodesList
END INTERFACE
!----------------------------------------------------------------------------------------------------------------------!
INTERFACE CreateEdgeNodes
  MODULE PROCEDURE CreateEdgeNodes
END INTERFACE

INTERFACE GetEdgeNodesData
  MODULE PROCEDURE GetEdgeNodesData
END INTERFACE

INTERFACE AddEdgeNodesToList
  MODULE PROCEDURE AddEdgeNodesToList
END INTERFACE

INTERFACE PrintEdgeNodesList
  MODULE PROCEDURE PrintEdgeNodesList
END INTERFACE

INTERFACE DestructEdgeNodesList
  MODULE PROCEDURE DestructEdgeNodesList
END INTERFACE
!----------------------------------------------------------------------------------------------------------------------!
INTERFACE CheckFaceIDIsInNodesToFacesMap
  MODULE PROCEDURE CheckFaceIDIsInNodesToFacesMap
END INTERFACE

INTERFACE GetFaceIDFromNodesToFacesMap
  MODULE PROCEDURE GetFaceIDFromNodesToFacesMap
END INTERFACE
!----------------------------------------------------------------------------------------------------------------------!
PUBLIC :: CreateElem
PUBLIC :: RemoveElem
PUBLIC :: CreateSide
PUBLIC :: CreateBC
PUBLIC :: CopyBC
PUBLIC :: CreateNode
PUBLIC :: AddDataToNode
PUBLIC :: GetNodeData
PUBLIC :: CreateElem_CGNS
PUBLIC :: CountNodes
PUBLIC :: CountElems
PUBLIC :: CountElemID
PUBLIC :: CountElemsPerLevel
PUBLIC :: CountBCFaces
PUBLIC :: SetAndCountNodeID
PUBLIC :: AddElemToList
PUBLIC :: PrintElemList
PUBLIC :: DestructElemList
PUBLIC :: CreateFaceNodes
PUBLIC :: GetFaceNodesData
PUBLIC :: AddFaceNodesToList
PUBLIC :: PrintFaceNodesList
PUBLIC :: DestructFaceNodesList
PUBLIC :: CreateEdgeNodes
PUBLIC :: GetEdgeNodesData
PUBLIC :: AddEdgeNodesToList
PUBLIC :: PrintEdgeNodesList
PUBLIC :: DestructEdgeNodesList
PUBLIC :: CheckFaceIDIsInNodesToFacesMap
PUBLIC :: GetFaceIDFromNodesToFacesMap
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
SUBROUTINE CreateNode(Node,RefCount)
!----------------------------------------------------------------------------------------------------------------------!
IMPLICIT NONE
!----------------------------------------------------------------------------------------------------------------------!
! FORMAL ARGUMENTS
!----------------------------------------------------------------------------------------------------------------------!
TYPE(tNode),POINTER,INTENT(INOUT) :: Node
INTEGER,OPTIONAL,INTENT(IN)       :: RefCount
!----------------------------------------------------------------------------------------------------------------------!
! LOCAL VARIABLES
!----------------------------------------------------------------------------------------------------------------------!

ALLOCATE(Node)
Node%NodeID = 0
Node%BCFlag = 0

IF(PRESENT(RefCount)) THEN
  Node%RefCount = RefCount
ELSE
  Node%RefCount = 0
END IF

! ! ! NodeCount = NodeCount+1

!----------------------------------------------------------------------------------------------------------------------!
END SUBROUTINE CreateNode
!======================================================================================================================!
!
!
!
!======================================================================================================================!
SUBROUTINE CreateNodeAndSetNodeID(Node,NodeID)
!----------------------------------------------------------------------------------------------------------------------!
IMPLICIT NONE
!----------------------------------------------------------------------------------------------------------------------!
! FORMAL ARGUMENTS
!----------------------------------------------------------------------------------------------------------------------!
TYPE(tNode),POINTER,INTENT(INOUT) :: Node
INTEGER,OPTIONAL,INTENT(INOUT)    :: NodeID
!----------------------------------------------------------------------------------------------------------------------!
! LOCAL VARIABLES
!----------------------------------------------------------------------------------------------------------------------!

CALL CreateNode(Node)

NodeID      = NodeID+1
Node%NodeID = NodeID

!----------------------------------------------------------------------------------------------------------------------!
END SUBROUTINE CreateNodeAndSetNodeID
!======================================================================================================================!
!
!
!
!======================================================================================================================!
SUBROUTINE AddDataToNode(Node,NodeID,Coords)
!----------------------------------------------------------------------------------------------------------------------!
IMPLICIT NONE
!----------------------------------------------------------------------------------------------------------------------!
! FORMAL ARGUMENTS
!----------------------------------------------------------------------------------------------------------------------!
TYPE(tNode),INTENT(INOUT) :: Node 
INTEGER,INTENT(IN)        :: NodeID
REAL,INTENT(IN)           :: Coords(1:3)
!----------------------------------------------------------------------------------------------------------------------!
! LOCAL VARIABLES
!----------------------------------------------------------------------------------------------------------------------!

Node%NodeID = NodeID
Node%Coords = Coords

!----------------------------------------------------------------------------------------------------------------------!
END SUBROUTINE AddDataToNode
!======================================================================================================================!
!
!
!
!======================================================================================================================!
SUBROUTINE GetNodeData(Node,NodeID,Coords)
!----------------------------------------------------------------------------------------------------------------------!
IMPLICIT NONE
!----------------------------------------------------------------------------------------------------------------------!
! FORMAL ARGUMENTS
!----------------------------------------------------------------------------------------------------------------------!
TYPE(tNode),INTENT(IN)       :: Node
INTEGER,OPTIONAL,INTENT(OUT) :: NodeID
REAL,OPTIONAL,INTENT(OUT)    :: Coords(1:3)
!----------------------------------------------------------------------------------------------------------------------!
! LOCAL VARIABLES
!----------------------------------------------------------------------------------------------------------------------!

IF (PRESENT(NodeID)) THEN
  NodeID = Node%NodeID
END IF

IF (PRESENT(Coords)) THEN
  Coords = Node%Coords
END IF

!----------------------------------------------------------------------------------------------------------------------!
END SUBROUTINE GetNodeData
!======================================================================================================================!
!
!
!
!======================================================================================================================!
SUBROUTINE CreateElem(Elem)
!----------------------------------------------------------------------------------------------------------------------!
IMPLICIT NONE
!----------------------------------------------------------------------------------------------------------------------!
! FORMAL ARGUMENTS
!----------------------------------------------------------------------------------------------------------------------!
TYPE(tElem),POINTER,INTENT(INOUT) :: Elem
!----------------------------------------------------------------------------------------------------------------------!
! LOCAL VARIABLES
!----------------------------------------------------------------------------------------------------------------------!

ALLOCATE(Elem)

NULLIFY(Elem%PrevElem)
NULLIFY(Elem%NextElem)
NULLIFY(Elem%FirstSide)

NULLIFY(Elem%Nodes)

Elem%ElemType = 0
Elem%nNodes   = 0
Elem%ElemID   = 0
Elem%Flag     = 0
Elem%Level    = 0
Elem%nPoints  = 0
Elem%nFacets  = 0

!----------------------------------------------------------------------------------------------------------------------!
END SUBROUTINE CreateElem
!======================================================================================================================!
!
!
!
!======================================================================================================================!
SUBROUTINE DestructElemList(ElemList)
!----------------------------------------------------------------------------------------------------------------------!
IMPLICIT NONE
!----------------------------------------------------------------------------------------------------------------------!
! FORMAL ARGUMENTS
!----------------------------------------------------------------------------------------------------------------------!
TYPE(tElemList),INTENT(INOUT) :: ElemList
!----------------------------------------------------------------------------------------------------------------------!
! LOCAL VARIABLES
!----------------------------------------------------------------------------------------------------------------------!
TYPE(tElem),POINTER :: aElem
TYPE(tElem),POINTER :: bElem
!----------------------------------------------------------------------------------------------------------------------!

IF (.NOT. ASSOCIATED(ElemList%FirstElem)) THEN
  RETURN
END IF

aElem => ElemList%FirstElem
DO WHILE(ASSOCIATED(aElem))
  bElem => aElem%NextElem
  CALL RemoveElem(aElem,ElemList)
  aElem => bElem
END DO

NULLIFY(ElemList%FirstElem)
NULLIFY(ElemList%LastElem)

!----------------------------------------------------------------------------------------------------------------------!
END SUBROUTINE DestructElemList
!======================================================================================================================!
!
!
!
!======================================================================================================================!
SUBROUTINE AddElemToList(Elem,ElemList)
!----------------------------------------------------------------------------------------------------------------------!
IMPLICIT NONE
!----------------------------------------------------------------------------------------------------------------------!
! FORMAL ARGUMENTS
!----------------------------------------------------------------------------------------------------------------------!
TYPE(tElem),POINTER,INTENT(INOUT) :: Elem
TYPE(tElemList),INTENT(INOUT)     :: ElemList
!----------------------------------------------------------------------------------------------------------------------!
! LOCAL VARIABLES
!----------------------------------------------------------------------------------------------------------------------!

Elem%PrevElem => ElemList%LastElem
NULLIFY(Elem%NextElem)

IF (.NOT. ASSOCIATED(ElemList%LastElem)) THEN
  ElemList%FirstElem => Elem
  ElemList%LastElem  => Elem
ELSE
  ElemList%LastElem%NextElem => Elem
END IF

ElemList%LastElem => Elem

!----------------------------------------------------------------------------------------------------------------------!
END SUBROUTINE AddElemToList
!======================================================================================================================!
!
!
!
!======================================================================================================================!
SUBROUTINE PrintElemList()
!----------------------------------------------------------------------------------------------------------------------!
IMPLICIT NONE
!----------------------------------------------------------------------------------------------------------------------!
! FORMAL ARGUMENTS
!----------------------------------------------------------------------------------------------------------------------!
! LOCAL VARIABLES
!----------------------------------------------------------------------------------------------------------------------!
INTEGER :: iNode
INTEGER :: ElemID
INTEGER :: ElemType
!----------------------------------------------------------------------------------------------------------------------!
REAL,ALLOCATABLE :: CornerCoords(:,:)
!----------------------------------------------------------------------------------------------------------------------!
TYPE(tElem),POINTER :: aElem
TYPE(tSide),POINTER :: aSide
!----------------------------------------------------------------------------------------------------------------------!

WRITE(UNIT_SCREEN,*)
WRITE(UNIT_SCREEN,*)
WRITE(UNIT_SCREEN,"(A)") "======================================================="
WRITE(UNIT_SCREEN,"(A)") "ELEMENTS: NODES AND COORDINATES"
WRITE(UNIT_SCREEN,"(A)") "======================================================="
WRITE(UNIT_SCREEN,*)

aElem => ElemList%FirstElem
DO WHILE(ASSOCIATED(aElem))
  ElemType = aElem%ElemType
  ElemID   = aElem%ElemID
  IF (ALLOCATED(CornerCoords)) THEN
    DEALLOCATE(CornerCoords)
  END IF
  ALLOCATE(CornerCoords(1:aElem%nNodes,1:PP_nDims))
  DO iNode=1,aElem%nNodes
    CornerCoords(iNode,1:PP_nDims) = aElem%Nodes(iNode)%Node%Coords(1:PP_nDims)
  END DO
  WRITE(UNIT_SCREEN,'(2X,A,I0)') "ElemID   = ", ElemID
  WRITE(UNIT_SCREEN,'(2X,A,I0)') "ElemType = ", ElemType
  SELECT CASE(PP_nDims)
    CASE(2)
      WRITE(UNIT_SCREEN,'(4X,A6,2(2X,A13))') "NodeID", "CoordinateX", "CoordinateY"
      DO iNode=1,aElem%nNodes
        WRITE(UNIT_SCREEN,'(4X,I6,2(2X,SP,ES13.6E2))') &
          aElem%Nodes(iNode)%Node%NodeID, &
          CornerCoords(iNode,1:PP_nDims)
      END DO
    CASE(3)
      WRITE(UNIT_SCREEN,'(4X,A6,3(2X,A13))') "NodeID", "CoordinateX", "CoordinateY", "CoordinateZ"
      DO iNode=1,aElem%nNodes
        WRITE(UNIT_SCREEN,'(4X,I6,3(2X,SP,ES13.6E2))') &
          aElem%Nodes(iNode)%Node%NodeID, &
          CornerCoords(iNode,1:PP_nDims)
      END DO
  END SELECT
  WRITE(UNIT_SCREEN,*)
  aElem => aElem%NextElem
END DO

WRITE(UNIT_SCREEN,*)
WRITE(UNIT_SCREEN,*)
WRITE(UNIT_SCREEN,"(A)") "======================================================="
WRITE(UNIT_SCREEN,"(A)") "FACES: NODES AND TYPES"
WRITE(UNIT_SCREEN,"(A)") "======================================================="
WRITE(UNIT_SCREEN,*)

aElem => ElemList%FirstElem
DO WHILE(ASSOCIATED(aElem))
  WRITE(UNIT_SCREEN,'(2X,A,1X,I0)') "ElemID    =", aElem%ElemID
  WRITE(UNIT_SCREEN,'(2X,A,1X,I0)') "ElemType  =", aElem%ElemType
  WRITE(UNIT_SCREEN,'(2X,A)',ADVANCE='NO') "Nodes     ="
  DO iNode=1,aElem%nNodes
    WRITE(UNIT_SCREEN,'(1X,I0)',ADVANCE='NO') aElem%Nodes(iNode)%Node%NodeID
  END DO
  WRITE(UNIT_SCREEN,*)

  aSide => aElem%FirstSide
  DO WHILE(ASSOCIATED(aSide))
    WRITE(UNIT_SCREEN,'(2X,2(A,1X,I0,1X,A1,1X),A)',ADVANCE='NO') &
      "LocalSide =", aSide%LocSide, "-", &
      "SideID =", aSide%SideID, "-", &
      "Nodes ="
    DO iNode=1,aSide%nNodes
      WRITE(UNIT_SCREEN,'(1X,I0)',ADVANCE='NO') aSide%Nodes(iNode)%Node%NodeID
    END DO
    WRITE(UNIT_SCREEN,*)
    aSide => aSide%NextElemSide
  END DO
  WRITE(UNIT_SCREEN,*)
  aElem => aElem%NextElem
END DO

WRITE(UNIT_SCREEN,*)
WRITE(UNIT_SCREEN,*)
WRITE(UNIT_SCREEN,"(A)") "======================================================="
WRITE(UNIT_SCREEN,"(A)") "BCFACES: NODES AND TYPES"
WRITE(UNIT_SCREEN,"(A)") "======================================================="
WRITE(UNIT_SCREEN,*)

aElem => ElemList%FirstElem
DO WHILE(ASSOCIATED(aElem))
  aSide => aElem%FirstSide
  DO WHILE(ASSOCIATED(aSide))
    IF (ASSOCIATED(aSide%BC)) THEN
      WRITE(UNIT_SCREEN,'(2X,A,1X,I0)') "ElemID    =", aElem%ElemID
      WRITE(UNIT_SCREEN,'(2X,A,1X,I0)') "ElemType  =", aElem%ElemType
      WRITE(UNIT_SCREEN,'(2X,A)',ADVANCE='NO') "Nodes     ="
      DO iNode=1,aElem%nNodes
        WRITE(UNIT_SCREEN,'(1X,I0)',ADVANCE='NO') aElem%Nodes(iNode)%Node%NodeID
      END DO
      WRITE(UNIT_SCREEN,*)
    
      WRITE(UNIT_SCREEN,'(2X,A,1X,I0)') "LocalSide =", aSide%LocSide
      WRITE(UNIT_SCREEN,'(2X,A,1X,I0)') "BCType    =", aSide%BC%BCMark
      WRITE(UNIT_SCREEN,'(2X,A,1X,I0)') "SideID    =", aSide%SideID
      WRITE(UNIT_SCREEN,'(2X,A)',ADVANCE='NO') "Nodes     ="
      DO iNode=1,aSide%nNodes
        WRITE(UNIT_SCREEN,'(1X,I0)',ADVANCE='NO') aSide%Nodes(iNode)%Node%NodeID
      END DO
      WRITE(UNIT_SCREEN,*)
      WRITE(UNIT_SCREEN,*)
    END IF
    aSide => aSide%NextElemSide
  END DO
  aElem => aElem%NextElem
END DO

!----------------------------------------------------------------------------------------------------------------------!
END SUBROUTINE PrintElemList
!======================================================================================================================!
!
!
!
!======================================================================================================================!
SUBROUTINE CreateFaceNodes(FaceNodes,FaceID,nNodes,NodeID)
!----------------------------------------------------------------------------------------------------------------------!
IMPLICIT NONE
!----------------------------------------------------------------------------------------------------------------------!
! FORMAL ARGUMENTS
!----------------------------------------------------------------------------------------------------------------------!
TYPE(tFaceNodes),POINTER,INTENT(INOUT) :: FaceNodes
INTEGER,INTENT(IN)                     :: FaceID
INTEGER,INTENT(IN)                     :: nNodes
INTEGER,INTENT(IN)                     :: NodeID(1:nNodes)
!----------------------------------------------------------------------------------------------------------------------!
! LOCAL VARIABLES
!----------------------------------------------------------------------------------------------------------------------!

ALLOCATE(FaceNodes)

NULLIFY(FaceNodes%PrevFaceNodes)
NULLIFY(FaceNodes%NextFaceNodes)

FaceNodes%FaceID = FaceID

ALLOCATE(FaceNodes%NodeID(1:nNodes))
FaceNodes%NodeID(1:nNodes) = NodeID(1:nNodes)

!----------------------------------------------------------------------------------------------------------------------!
END SUBROUTINE CreateFaceNodes
!======================================================================================================================!
!
!
!
!======================================================================================================================!
SUBROUTINE DestructFaceNodesList(FaceNodesList)
!----------------------------------------------------------------------------------------------------------------------!
IMPLICIT NONE
!----------------------------------------------------------------------------------------------------------------------!
! FORMAL ARGUMENTS
!----------------------------------------------------------------------------------------------------------------------!
TYPE(tFaceNodesList),INTENT(INOUT) :: FaceNodesList
!----------------------------------------------------------------------------------------------------------------------!
! LOCAL VARIABLES
!----------------------------------------------------------------------------------------------------------------------!
TYPE(tFaceNodes),POINTER :: aFaceNodes
TYPE(tFaceNodes),POINTER :: bFaceNodes
!----------------------------------------------------------------------------------------------------------------------!

IF (.NOT. ASSOCIATED(FaceNodesList%FirstFaceNodes)) THEN
  RETURN
END IF

aFaceNodes => FaceNodesList%FirstFaceNodes
DO WHILE(ASSOCIATED(aFaceNodes))
  bFaceNodes => aFaceNodes%NextFaceNodes
  NULLIFY(aFaceNodes%PrevFaceNodes)
  NULLIFY(aFaceNodes%NextFaceNodes)
  DEALLOCATE(aFaceNodes%NodeID)
  DEALLOCATE(aFaceNodes)
  aFaceNodes => bFaceNodes
END DO

NULLIFY(FaceNodesList%FirstFaceNodes)
NULLIFY(FaceNodesList%LastFaceNodes)

!----------------------------------------------------------------------------------------------------------------------!
END SUBROUTINE DestructFaceNodesList
!======================================================================================================================!
!
!
!
!======================================================================================================================!
SUBROUTINE AddFaceNodesToList(FaceNodes,FaceNodesList)
!----------------------------------------------------------------------------------------------------------------------!
IMPLICIT NONE
!----------------------------------------------------------------------------------------------------------------------!
! FORMAL ARGUMENTS
!----------------------------------------------------------------------------------------------------------------------!
TYPE(tFaceNodes),POINTER,INTENT(INOUT) :: FaceNodes
TYPE(tFaceNodesList),INTENT(INOUT)     :: FaceNodesList
!----------------------------------------------------------------------------------------------------------------------!
! LOCAL VARIABLES
!----------------------------------------------------------------------------------------------------------------------!

FaceNodes%PrevFaceNodes => FaceNodesList%LastFaceNodes
NULLIFY(FaceNodes%NextFaceNodes)

IF (.NOT. ASSOCIATED(FaceNodesList%LastFaceNodes)) THEN
  FaceNodesList%FirstFaceNodes => FaceNodes
  FaceNodesList%LastFaceNodes  => FaceNodes
ELSE
  FaceNodesList%LastFaceNodes%NextFaceNodes => FaceNodes
END IF

FaceNodesList%LastFaceNodes => FaceNodes

!----------------------------------------------------------------------------------------------------------------------!
END SUBROUTINE AddFaceNodesToList
!======================================================================================================================!
!
!
!
!======================================================================================================================!
SUBROUTINE GetFaceNodesData(FaceNodes,FaceID,NodeID)
!----------------------------------------------------------------------------------------------------------------------!
IMPLICIT NONE
!----------------------------------------------------------------------------------------------------------------------!
! FORMAL ARGUMENTS
!----------------------------------------------------------------------------------------------------------------------!
TYPE(tFaceNodes),POINTER,INTENT(IN) :: FaceNodes
INTEGER,OPTIONAL,INTENT(OUT)        :: FaceID
INTEGER,OPTIONAL,INTENT(OUT)        :: NodeID(:)
!----------------------------------------------------------------------------------------------------------------------!
! LOCAL VARIABLES
!----------------------------------------------------------------------------------------------------------------------!

IF (PRESENT(FaceID)) THEN
  FaceID = FaceNodes%FaceID
END IF

IF (PRESENT(NodeID)) THEN
  NodeID(:) = FaceNodes%NodeID(:)
END IF

!----------------------------------------------------------------------------------------------------------------------!
END SUBROUTINE GetFaceNodesData
!======================================================================================================================!
!
!
!
!======================================================================================================================!
SUBROUTINE PrintFaceNodesList(FaceNodesList)
!----------------------------------------------------------------------------------------------------------------------!
IMPLICIT NONE
!----------------------------------------------------------------------------------------------------------------------!
! FORMAL ARGUMENTS
!----------------------------------------------------------------------------------------------------------------------!
TYPE(tFaceNodesList),INTENT(IN) :: FaceNodesList
!----------------------------------------------------------------------------------------------------------------------!
! LOCAL VARIABLES
!----------------------------------------------------------------------------------------------------------------------!
INTEGER :: iNode
!----------------------------------------------------------------------------------------------------------------------!
TYPE(tFaceNodes),POINTER :: aFaceNodes
!----------------------------------------------------------------------------------------------------------------------!

WRITE(UNIT_SCREEN,"(2(2X,A8))") "FaceID", "NodeID(:)"

aFaceNodes => FaceNodesList%FirstFaceNodes
DO WHILE(ASSOCIATED(aFaceNodes))
  WRITE(UNIT_SCREEN,'(2X,I8,1X)',ADVANCE='NO') aFaceNodes%FaceID
  DO iNode=1,SIZE(aFaceNodes%NodeID)
    WRITE(UNIT_SCREEN,'(I0,1X)',ADVANCE='NO') aFaceNodes%NodeID(iNode)
  END DO
  WRITE(UNIT_SCREEN,*)
  aFaceNodes => aFaceNodes%NextFaceNodes
END DO

!----------------------------------------------------------------------------------------------------------------------!
END SUBROUTINE PrintFaceNodesList
!======================================================================================================================!
!
!
!
!======================================================================================================================!
SUBROUTINE CreateEdgeNodes(EdgeNodes,EdgeID,nNodes,NodeID)
!----------------------------------------------------------------------------------------------------------------------!
IMPLICIT NONE
!----------------------------------------------------------------------------------------------------------------------!
! FORMAL ARGUMENTS
!----------------------------------------------------------------------------------------------------------------------!
TYPE(tEdgeNodes),POINTER,INTENT(INOUT) :: EdgeNodes
INTEGER,INTENT(IN)                     :: EdgeID
INTEGER,INTENT(IN)                     :: nNodes
INTEGER,INTENT(IN)                     :: NodeID(1:nNodes)
!----------------------------------------------------------------------------------------------------------------------!
! LOCAL VARIABLES
!----------------------------------------------------------------------------------------------------------------------!

ALLOCATE(EdgeNodes)

NULLIFY(EdgeNodes%PrevEdgeNodes)
NULLIFY(EdgeNodes%NextEdgeNodes)

EdgeNodes%EdgeID = EdgeID

ALLOCATE(EdgeNodes%NodeID(1:nNodes))
EdgeNodes%NodeID(1:nNodes) = NodeID(1:nNodes)

!----------------------------------------------------------------------------------------------------------------------!
END SUBROUTINE CreateEdgeNodes
!======================================================================================================================!
!
!
!
!======================================================================================================================!
SUBROUTINE DestructEdgeNodesList(EdgeNodesList)
!----------------------------------------------------------------------------------------------------------------------!
IMPLICIT NONE
!----------------------------------------------------------------------------------------------------------------------!
! FORMAL ARGUMENTS
!----------------------------------------------------------------------------------------------------------------------!
TYPE(tEdgeNodesList),INTENT(INOUT) :: EdgeNodesList
!----------------------------------------------------------------------------------------------------------------------!
! LOCAL VARIABLES
!----------------------------------------------------------------------------------------------------------------------!
TYPE(tEdgeNodes),POINTER :: aEdgeNodes
TYPE(tEdgeNodes),POINTER :: bEdgeNodes
!----------------------------------------------------------------------------------------------------------------------!

IF (.NOT. ASSOCIATED(EdgeNodesList%FirstEdgeNodes)) THEN
  RETURN
END IF

aEdgeNodes => EdgeNodesList%FirstEdgeNodes
DO WHILE(ASSOCIATED(aEdgeNodes))
  bEdgeNodes => aEdgeNodes%NextEdgeNodes
  NULLIFY(aEdgeNodes%PrevEdgeNodes)
  NULLIFY(aEdgeNodes%NextEdgeNodes)
  DEALLOCATE(aEdgeNodes%NodeID)
  DEALLOCATE(aEdgeNodes)
  aEdgeNodes => bEdgeNodes
END DO

NULLIFY(EdgeNodesList%FirstEdgeNodes)
NULLIFY(EdgeNodesList%LastEdgeNodes)

!----------------------------------------------------------------------------------------------------------------------!
END SUBROUTINE DestructEdgeNodesList
!======================================================================================================================!
!
!
!
!======================================================================================================================!
SUBROUTINE AddEdgeNodesToList(EdgeNodes,EdgeNodesList)
!----------------------------------------------------------------------------------------------------------------------!
IMPLICIT NONE
!----------------------------------------------------------------------------------------------------------------------!
! FORMAL ARGUMENTS
!----------------------------------------------------------------------------------------------------------------------!
TYPE(tEdgeNodes),POINTER,INTENT(INOUT) :: EdgeNodes
TYPE(tEdgeNodesList),INTENT(INOUT)     :: EdgeNodesList
!----------------------------------------------------------------------------------------------------------------------!
! LOCAL VARIABLES
!----------------------------------------------------------------------------------------------------------------------!

EdgeNodes%PrevEdgeNodes => EdgeNodesList%LastEdgeNodes
NULLIFY(EdgeNodes%NextEdgeNodes)

IF (.NOT. ASSOCIATED(EdgeNodesList%LastEdgeNodes)) THEN
  EdgeNodesList%FirstEdgeNodes => EdgeNodes
  EdgeNodesList%LastEdgeNodes  => EdgeNodes
ELSE
  EdgeNodesList%LastEdgeNodes%NextEdgeNodes => EdgeNodes
END IF

EdgeNodesList%LastEdgeNodes => EdgeNodes

!----------------------------------------------------------------------------------------------------------------------!
END SUBROUTINE AddEdgeNodesToList
!======================================================================================================================!
!
!
!
!======================================================================================================================!
SUBROUTINE GetEdgeNodesData(EdgeNodes,EdgeID,NodeID)
!----------------------------------------------------------------------------------------------------------------------!
IMPLICIT NONE
!----------------------------------------------------------------------------------------------------------------------!
! FORMAL ARGUMENTS
!----------------------------------------------------------------------------------------------------------------------!
TYPE(tEdgeNodes),POINTER,INTENT(IN) :: EdgeNodes
INTEGER,OPTIONAL,INTENT(OUT)        :: EdgeID
INTEGER,OPTIONAL,INTENT(OUT)        :: NodeID(:)
!----------------------------------------------------------------------------------------------------------------------!
! LOCAL VARIABLES
!----------------------------------------------------------------------------------------------------------------------!

IF (PRESENT(EdgeID)) THEN
  EdgeID = EdgeNodes%EdgeID
END IF

IF (PRESENT(NodeID)) THEN
  NodeID(:) = EdgeNodes%NodeID(:)
END IF

!----------------------------------------------------------------------------------------------------------------------!
END SUBROUTINE GetEdgeNodesData
!======================================================================================================================!
!
!
!
!======================================================================================================================!
SUBROUTINE PrintEdgeNodesList(EdgeNodesList)
!----------------------------------------------------------------------------------------------------------------------!
IMPLICIT NONE
!----------------------------------------------------------------------------------------------------------------------!
! FORMAL ARGUMENTS
!----------------------------------------------------------------------------------------------------------------------!
TYPE(tEdgeNodesList),INTENT(IN) :: EdgeNodesList
!----------------------------------------------------------------------------------------------------------------------!
! LOCAL VARIABLES
!----------------------------------------------------------------------------------------------------------------------!
TYPE(tEdgeNodes),POINTER :: aEdgeNodes
!----------------------------------------------------------------------------------------------------------------------!
CHARACTER(LEN=256) :: FormatString
!----------------------------------------------------------------------------------------------------------------------!

SELECT CASE(PP_nDims)
  CASE(2)
    FormatString = "(1(2X,I8),2(2X,I8))"
  CASE(3)
    FormatString = "(1(2X,I8),2(2X,I8))"
END SELECT

WRITE(UNIT_SCREEN,"(2(2X,A8))") "iEdge", "NodeIDs"

aEdgeNodes => EdgeNodesList%FirstEdgeNodes
DO WHILE(ASSOCIATED(aEdgeNodes))
  WRITE(UNIT_SCREEN,FormatString) aEdgeNodes%EdgeID, aEdgeNodes%NodeID(:)
  aEdgeNodes => aEdgeNodes%NextEdgeNodes
END DO

!----------------------------------------------------------------------------------------------------------------------!
END SUBROUTINE PrintEdgeNodesList
!======================================================================================================================!
!
!
!
!======================================================================================================================!
SUBROUTINE CreateSide(Side,nNodes)
!----------------------------------------------------------------------------------------------------------------------!
IMPLICIT NONE
!----------------------------------------------------------------------------------------------------------------------!
! FORMAL ARGUMENTS
!----------------------------------------------------------------------------------------------------------------------!
TYPE(tSide),POINTER,INTENT(INOUT) :: Side
INTEGER,INTENT(IN)                :: nNodes
!----------------------------------------------------------------------------------------------------------------------!
! LOCAL VARIABLES
!----------------------------------------------------------------------------------------------------------------------!
INTEGER :: iNode
!----------------------------------------------------------------------------------------------------------------------!

ALLOCATE(Side)
ALLOCATE(Side%Nodes(1:nNodes))

DO iNode=1,nNodes
  NULLIFY(Side%Nodes(iNode)%Node)
END DO

Side%nNodes  = nNodes
Side%LocSide = 0
Side%SideID  = 0

NULLIFY(Side%BC)
NULLIFY(Side%NextElemSide)

!----------------------------------------------------------------------------------------------------------------------!
END SUBROUTINE CreateSide
!======================================================================================================================!
!
!
!
!======================================================================================================================!
SUBROUTINE CreateSides(Elem)
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
INTEGER :: iNode
INTEGER :: iSide
INTEGER :: nElemFaces
INTEGER :: nNodesOnFace
INTEGER,ALLOCATABLE :: NodesOnFace(:)
!----------------------------------------------------------------------------------------------------------------------!
TYPE(tSide),POINTER :: aSide
!----------------------------------------------------------------------------------------------------------------------!

CALL GetnElemFaces(Elem%ElemType,nElemFaces)

DO iSide=1,nElemFaces
  IF (iSide .EQ. 1) THEN
    CALL GetnNodesOnFace(Elem%ElemType,iSide,nNodesOnFace)
    CALL CreateSide(Elem%FirstSide,nNodesOnFace)
    aSide => Elem%FirstSide
  ELSE
    CALL GetnNodesOnFace(Elem%ElemType,iSide,nNodesOnFace)
    CALL CreateSide(aSide%NextElemSide,nNodesOnFace)
    aSide => aSide%NextElemSide
  END IF
  aSide%LocSide = iSide
  CALL GetNodesOnFace(Elem%ElemType,iSide,NodesOnFace)
  DO iNode=1,aSide%nNodes
    aSide%Nodes(iNode)%Node          => Elem%Nodes(NodesOnFace(iNode))%Node
    aSide%Nodes(iNode)%Node%RefCount = aSide%Nodes(iNode)%Node%RefCount+1
  END DO
END DO

!----------------------------------------------------------------------------------------------------------------------!
END SUBROUTINE CreateSides
!======================================================================================================================!
!
!
!
!======================================================================================================================!
SUBROUTINE CreateBC(BC)
!----------------------------------------------------------------------------------------------------------------------!
IMPLICIT NONE
!----------------------------------------------------------------------------------------------------------------------!
! FORMAL ARGUMENTS
!----------------------------------------------------------------------------------------------------------------------!
TYPE(tBC),POINTER,INTENT(INOUT) :: BC
!----------------------------------------------------------------------------------------------------------------------!
! LOCAL VARIABLES
!----------------------------------------------------------------------------------------------------------------------!

ALLOCATE(BC)

BC%BCMark = 0

!----------------------------------------------------------------------------------------------------------------------!
END SUBROUTINE CreateBC
!======================================================================================================================!
!
!
!
!======================================================================================================================!
SUBROUTINE CopyBC(BCSide,Side)
!----------------------------------------------------------------------------------------------------------------------!
IMPLICIT NONE
!----------------------------------------------------------------------------------------------------------------------!
! FORMAL ARGUMENTS
!----------------------------------------------------------------------------------------------------------------------!
TYPE(tSide),POINTER,INTENT(IN)    :: BCSide
TYPE(tSide),POINTER,INTENT(INOUT) :: Side
!----------------------------------------------------------------------------------------------------------------------!
! LOCAL VARIABLES
!----------------------------------------------------------------------------------------------------------------------!

CALL CreateBC(Side%BC)

Side%BC%BCMark = BCSide%BC%BCMark

!----------------------------------------------------------------------------------------------------------------------!
END SUBROUTINE CopyBC
!======================================================================================================================!
!
!
!
!======================================================================================================================!
SUBROUTINE CreateElem_CGNS(Elem,ElemType,NGeo,Nodes,ElemID,ParentID)
!----------------------------------------------------------------------------------------------------------------------!
USE MOD_Mesh_CGNS_Definitions
!----------------------------------------------------------------------------------------------------------------------!
IMPLICIT NONE
!----------------------------------------------------------------------------------------------------------------------!
! FORMAL ARGUMENTS
!----------------------------------------------------------------------------------------------------------------------!
TYPE(tElem),POINTER,INTENT(INOUT) :: Elem
INTEGER,INTENT(IN)                :: ElemType
INTEGER,INTENT(IN)                :: NGeo
TYPE(tNodePtr),INTENT(IN)         :: Nodes(:)
INTEGER,OPTIONAL,INTENT(IN)       :: ElemID
INTEGER,OPTIONAL,INTENT(IN)       :: ParentID
!----------------------------------------------------------------------------------------------------------------------!
! LOCAL VARIABLES
!----------------------------------------------------------------------------------------------------------------------!
INTEGER :: iNode
INTEGER :: nElemNodes
!----------------------------------------------------------------------------------------------------------------------!

CALL CreateElem(Elem)

Elem%Level   = 0
Elem%nPoints = 0
Elem%nFacets = 0

IF (PRESENT(ElemID)) THEN
  Elem%ElemID   = ElemID
END IF

Elem%ElemType = ElemType

CALL GetnElemNodes(ElemType,nElemNodes)
Elem%nNodes = nElemNodes

ALLOCATE(Elem%Nodes(Elem%nNodes))
DO iNode=1,nElemNodes
  Elem%Nodes(iNode)%Node => Nodes(iNode)%Node
END DO

CALL CreateSides(Elem)

!----------------------------------------------------------------------------------------------------------------------!
END SUBROUTINE CreateElem_CGNS
!======================================================================================================================!
!
!
!
!======================================================================================================================!
SUBROUTINE RemoveElem(Elem,ElemList)
!----------------------------------------------------------------------------------------------------------------------!
IMPLICIT NONE
!----------------------------------------------------------------------------------------------------------------------!
! FORMAL ARGUMENTS
!----------------------------------------------------------------------------------------------------------------------!
TYPE(tElem),POINTER,INTENT(INOUT) :: Elem
TYPE(tElemList),INTENT(INOUT)     :: ElemList
!----------------------------------------------------------------------------------------------------------------------!
! LOCAL VARIABLES
!----------------------------------------------------------------------------------------------------------------------!
INTEGER :: iNode
!----------------------------------------------------------------------------------------------------------------------!
TYPE(tSide),POINTER :: aSide
!----------------------------------------------------------------------------------------------------------------------!

IF (.NOT. ASSOCIATED(Elem)) THEN
  RETURN
END IF

DO WHILE (ASSOCIATED(Elem%FirstSide))
  aSide => Elem%FirstSide
  CALL RemoveSide(aSide,Elem%FirstSide)
END DO

IF (ASSOCIATED(Elem%Nodes)) THEN
  DO iNode=1,Elem%nNodes
    CALL RemoveNode(Elem%Nodes(iNode)%Node)
  END DO
  DEALLOCATE(Elem%Nodes)
END IF

CALL DisconnectElem(Elem,ElemList)

DEALLOCATE(Elem)

!----------------------------------------------------------------------------------------------------------------------!
END SUBROUTINE RemoveElem
!======================================================================================================================!
!
!
!
!======================================================================================================================!
SUBROUTINE DisconnectElem(Elem,ElemList)
!----------------------------------------------------------------------------------------------------------------------!
IMPLICIT NONE
!----------------------------------------------------------------------------------------------------------------------!
! FORMAL ARGUMENTS
!----------------------------------------------------------------------------------------------------------------------!
TYPE(tElem),POINTER,INTENT(INOUT) :: Elem
TYPE(tElemList),INTENT(INOUT)     :: ElemList
!----------------------------------------------------------------------------------------------------------------------!
! LOCAL VARIABLES
!----------------------------------------------------------------------------------------------------------------------!

IF (.NOT. ASSOCIATED(Elem)) THEN
  RETURN
END IF

IF (ASSOCIATED(Elem%PrevElem)) THEN
  Elem%PrevElem%NextElem => Elem%NextElem
ELSE
  ElemList%FirstElem => Elem%NextElem
END IF

IF (ASSOCIATED(Elem%NextElem)) THEN
  Elem%NextElem%PrevElem => Elem%PrevElem
ELSE
  ElemList%LastElem => Elem%PrevElem
END IF

!----------------------------------------------------------------------------------------------------------------------!
END SUBROUTINE DisconnectElem
!======================================================================================================================!
!
!
!
!======================================================================================================================!
SUBROUTINE RemoveSide(Side,FirstSide)
!----------------------------------------------------------------------------------------------------------------------!
IMPLICIT NONE
!----------------------------------------------------------------------------------------------------------------------!
! FORMAL ARGUMENTS
!----------------------------------------------------------------------------------------------------------------------!
TYPE(tSide),POINTER,INTENT(INOUT) :: Side
TYPE(tSide),POINTER,INTENT(INOUT) :: FirstSide
!----------------------------------------------------------------------------------------------------------------------!
! LOCAL VARIABLES
!----------------------------------------------------------------------------------------------------------------------!
INTEGER :: iNode
!----------------------------------------------------------------------------------------------------------------------!
TYPE(tSide),POINTER :: aFirstSide
!----------------------------------------------------------------------------------------------------------------------!

IF (.NOT. ASSOCIATED(Side)) THEN
  RETURN
END IF

aFirstSide => FirstSide

CALL DisconnectSide(Side,aFirstSide)

IF (ASSOCIATED(Side%BC)) THEN
  CALL RemoveBC(Side%BC)
END IF

IF (ASSOCIATED(Side%Nodes)) THEN
  DO iNode=1,Side%nNodes
    CALL RemoveNode(Side%Nodes(iNode)%Node)
  END DO
  DEALLOCATE(Side%Nodes)
END IF

IF (ASSOCIATED(Side)) THEN
  DEALLOCATE(Side)
END IF

FirstSide => aFirstSide

!----------------------------------------------------------------------------------------------------------------------!
END SUBROUTINE RemoveSide
!======================================================================================================================!
!
!
!
!======================================================================================================================!
SUBROUTINE DisconnectSide(Side,FirstSide)
!----------------------------------------------------------------------------------------------------------------------!
IMPLICIT NONE
!----------------------------------------------------------------------------------------------------------------------!
! FORMAL ARGUMENTS
!----------------------------------------------------------------------------------------------------------------------!
TYPE(tSide),POINTER,INTENT(IN)    :: Side
TYPE(tSide),POINTER,INTENT(INOUT) :: FirstSide
!----------------------------------------------------------------------------------------------------------------------!
! LOCAL VARIABLES
!----------------------------------------------------------------------------------------------------------------------!
TYPE(tSide),POINTER :: aSide
!----------------------------------------------------------------------------------------------------------------------!

IF (.NOT. ASSOCIATED(Side)) THEN
  RETURN
END IF

IF (ASSOCIATED(FirstSide,Side)) THEN
  FirstSide => Side%NextElemSide
ELSE
  aSide => FirstSide
  DO WHILE (ASSOCIATED(aSide%NextElemSide))
    IF (ASSOCIATED(aSide%NextElemSide,Side)) THEN
      aSide%NextElemSide => Side%NextElemSide
      EXIT
    END IF
    aSide => aSide%NextElemSide
  END DO
END IF

!----------------------------------------------------------------------------------------------------------------------!
END SUBROUTINE DisconnectSide
!======================================================================================================================!
!
!
!
!======================================================================================================================!
SUBROUTINE RemoveNode(Node)
!----------------------------------------------------------------------------------------------------------------------!
IMPLICIT NONE
!----------------------------------------------------------------------------------------------------------------------!
! FORMAL ARGUMENTS
!----------------------------------------------------------------------------------------------------------------------!
TYPE(tNode),POINTER,INTENT(INOUT) :: Node
!----------------------------------------------------------------------------------------------------------------------!
! LOCAL VARIABLES
!----------------------------------------------------------------------------------------------------------------------!

IF (.NOT. ASSOCIATED(Node)) THEN
  RETURN
END IF

Node%RefCount = Node%RefCount-1

IF (Node%RefCount .LE. 0) THEN
  NULLIFY(Node)
END IF

!----------------------------------------------------------------------------------------------------------------------!
END SUBROUTINE RemoveNode
!======================================================================================================================!
!
!
!
!======================================================================================================================!
SUBROUTINE RemoveBC(BC)
!----------------------------------------------------------------------------------------------------------------------!
IMPLICIT NONE
!----------------------------------------------------------------------------------------------------------------------!
! FORMAL ARGUMENTS
!----------------------------------------------------------------------------------------------------------------------!
TYPE(tBC),POINTER,INTENT(INOUT) :: BC
!----------------------------------------------------------------------------------------------------------------------!
! LOCAL VARIABLES
!----------------------------------------------------------------------------------------------------------------------!

DEALLOCATE(BC)
NULLIFY(BC)

!----------------------------------------------------------------------------------------------------------------------!
END SUBROUTINE RemoveBC
!======================================================================================================================!
!
!
!
!======================================================================================================================!
SUBROUTINE BuildQuadMap(NGeo,nNodes,QuadMap,QuadMapInv)
!----------------------------------------------------------------------------------------------------------------------!
IMPLICIT NONE
!----------------------------------------------------------------------------------------------------------------------!
! FORMAL ARGUMENTS
!----------------------------------------------------------------------------------------------------------------------!
INTEGER,INTENT(IN)                       :: NGeo
INTEGER,INTENT(OUT)                      :: nNodes
INTEGER,ALLOCATABLE,INTENT(OUT)          :: QuadMap(:,:)
INTEGER,ALLOCATABLE,OPTIONAL,INTENT(OUT) :: QuadMapInv(:,:)
!----------------------------------------------------------------------------------------------------------------------!
! LOCAL VARIABLES
!----------------------------------------------------------------------------------------------------------------------!
INTEGER :: i, j
INTEGER :: iNode
!----------------------------------------------------------------------------------------------------------------------!

nNodes = (NGeo+1)**2
ALLOCATE(QuadMap(1:nNodes,1:2))

IF (NGeo .EQ. 1) THEN
  QuadMap(1,1:2) = (/0,0/)
  QuadMap(2,1:2) = (/1,0/)
  QuadMap(3,1:2) = (/1,1/)
  QuadMap(4,1:2) = (/0,1/)
  RETURN
END IF

iNode = 0
DO j=0,NGeo
  DO i=0,NGeo
    iNode = iNode+1
    QuadMap(iNode,1:2) = (/i,j/)
  END DO
END DO

IF(PRESENT(QuadMapInv))THEN
  ALLOCATE(QuadMapInv(0:NGeo,0:NGeo))
  QuadMapInv=0
  iNode=0
  DO j=0,NGeo
    DO i=0,NGeo
      iNode = iNode+1
      QuadMapInv(i,j) = iNode
    END DO
  END DO
END IF

!----------------------------------------------------------------------------------------------------------------------!
END SUBROUTINE BuildQuadMap
!======================================================================================================================!
!
!
!
!======================================================================================================================!
SUBROUTINE BuildHexaMap(NGeo,nNodes,HexaMap,HexaMapInv)
!----------------------------------------------------------------------------------------------------------------------!
IMPLICIT NONE
!----------------------------------------------------------------------------------------------------------------------!
! FORMAL ARGUMENTS
!----------------------------------------------------------------------------------------------------------------------!
INTEGER,INTENT(IN)                       :: NGeo
INTEGER,INTENT(OUT)                      :: nNodes
INTEGER,ALLOCATABLE,INTENT(OUT)          :: HexaMap(:,:)
INTEGER,ALLOCATABLE,OPTIONAL,INTENT(OUT) :: HexaMapInv(:,:,:)
!----------------------------------------------------------------------------------------------------------------------!
! LOCAL VARIABLES
!----------------------------------------------------------------------------------------------------------------------!
INTEGER :: i,j,k
INTEGER :: iNode
!----------------------------------------------------------------------------------------------------------------------!

nNodes = (NGeo+1)**3
ALLOCATE(HexaMap(1:nNodes,1:3))

IF (NGeo .EQ. 1) THEN
  HexaMap(1,1:3) = (/0,0,0/)
  HexaMap(2,1:3) = (/1,0,0/)
  HexaMap(3,1:3) = (/1,1,0/)
  HexaMap(4,1:3) = (/0,1,0/)
  HexaMap(5,1:3) = (/0,0,1/)
  HexaMap(6,1:3) = (/1,0,1/)
  HexaMap(7,1:3) = (/1,1,1/)
  HexaMap(8,1:3) = (/0,1,1/)
  RETURN
END IF

iNode = 0
DO k=0,NGeo
  DO j=0,NGeo
    DO i=0,NGeo
      iNode = iNode+1
      HexaMap(iNode,1:3) = (/i,j,k/)
    END DO
  END DO
END DO

IF(PRESENT(HexaMapInv))THEN
  ALLOCATE(HexaMapInv(0:NGeo,0:NGeo,0:NGeo))
  HexaMapInv=0
  iNode=0
  DO k=0,NGeo
    DO j=0,NGeo
      DO i=0,NGeo
        iNode = iNode+1
        HexaMapInv(i,j,k) = iNode
      END DO
    END DO 
  END DO 
END IF

!----------------------------------------------------------------------------------------------------------------------!
END SUBROUTINE BuildHexaMap
!======================================================================================================================!
!
!
!
!======================================================================================================================!
SUBROUTINE CountNodes(ElemList,nNodes)
!----------------------------------------------------------------------------------------------------------------------!
IMPLICIT NONE
!----------------------------------------------------------------------------------------------------------------------!
! FORMAL ARGUMENTS
!----------------------------------------------------------------------------------------------------------------------!
TYPE(tElemList),INTENT(IN) :: ElemList
INTEGER,INTENT(INOUT)      :: nNodes
!----------------------------------------------------------------------------------------------------------------------!
! LOCAL VARIABLES
!----------------------------------------------------------------------------------------------------------------------!
INTEGER :: iNode
!----------------------------------------------------------------------------------------------------------------------!
TYPE(tElem),POINTER :: aElem
!----------------------------------------------------------------------------------------------------------------------!

! Compute nNodes
nNodes = 0
aElem => ElemList%FirstElem
DO WHILE (ASSOCIATED(aElem))
  DO iNode=1,aElem%nNodes
    IF (aElem%Nodes(iNode)%Node%NodeID .GT. nNodes) THEN
      nNodes = aElem%Nodes(iNode)%Node%NodeID
    END IF
  END DO
  aElem => aElem%NextElem
END DO

!----------------------------------------------------------------------------------------------------------------------!
END SUBROUTINE CountNodes
!======================================================================================================================!
!
!
!
!======================================================================================================================!
SUBROUTINE CountElems(ElemList,nElems)
!----------------------------------------------------------------------------------------------------------------------!
IMPLICIT NONE
!----------------------------------------------------------------------------------------------------------------------!
! FORMAL ARGUMENTS
!----------------------------------------------------------------------------------------------------------------------!
TYPE(tElemList),INTENT(IN) :: ElemList
INTEGER,INTENT(INOUT)      :: nElems
!----------------------------------------------------------------------------------------------------------------------!
! LOCAL VARIABLES
!----------------------------------------------------------------------------------------------------------------------!
TYPE(tElem),POINTER :: aElem
!----------------------------------------------------------------------------------------------------------------------!

! Compute nElems
nElems = 0
aElem => ElemList%FirstElem
DO WHILE(ASSOCIATED(aElem))
  nElems = nElems+1
  aElem => aElem%NextElem
END DO

!----------------------------------------------------------------------------------------------------------------------!
END SUBROUTINE CountElems
!======================================================================================================================!
!
!
!
!======================================================================================================================!
SUBROUTINE CountElemID(ElemList,ElemID)
!----------------------------------------------------------------------------------------------------------------------!
IMPLICIT NONE
!----------------------------------------------------------------------------------------------------------------------!
! FORMAL ARGUMENTS
!----------------------------------------------------------------------------------------------------------------------!
TYPE(tElemList),INTENT(IN) :: ElemList
INTEGER,INTENT(INOUT)      :: ElemID
!----------------------------------------------------------------------------------------------------------------------!
! LOCAL VARIABLES
!----------------------------------------------------------------------------------------------------------------------!
TYPE(tElem),POINTER :: aElem
!----------------------------------------------------------------------------------------------------------------------!

! Compute nElems
ElemID = 0
aElem => ElemList%FirstElem
DO WHILE(ASSOCIATED(aElem))
  ElemID = ElemID+1
  aElem => aElem%NextElem
END DO

!----------------------------------------------------------------------------------------------------------------------!
END SUBROUTINE CountElemID
!======================================================================================================================!
!
!
!
!======================================================================================================================!
SUBROUTINE CountElemsPerLevel(ElemList,MaxLevel,nElems)
!----------------------------------------------------------------------------------------------------------------------!
IMPLICIT NONE
!----------------------------------------------------------------------------------------------------------------------!
! FORMAL ARGUMENTS
!----------------------------------------------------------------------------------------------------------------------!
TYPE(tElemList),INTENT(IN)        :: ElemList
INTEGER,INTENT(IN)                :: MaxLevel
INTEGER,INTENT(INOUT),ALLOCATABLE :: nElems(:)
!----------------------------------------------------------------------------------------------------------------------!
! LOCAL VARIABLES
!----------------------------------------------------------------------------------------------------------------------!
TYPE(tElem),POINTER :: aElem
!----------------------------------------------------------------------------------------------------------------------!

IF (ALLOCATED(nElems)) THEN
  DEALLOCATE(nElems)
END IF
ALLOCATE(nElems(0:MaxLevel))
nElems = 0

! Compute nElems
aElem => ElemList%FirstElem
DO WHILE(ASSOCIATED(aElem))
  nElems(aElem%Level) = nElems(aElem%Level)+1
  aElem => aElem%NextElem
END DO

!----------------------------------------------------------------------------------------------------------------------!
END SUBROUTINE CountElemsPerLevel
!======================================================================================================================!
!
!
!
!======================================================================================================================!
SUBROUTINE CountBCFaces(ElemList,nBCFaces)
!----------------------------------------------------------------------------------------------------------------------!
IMPLICIT NONE
!----------------------------------------------------------------------------------------------------------------------!
! FORMAL ARGUMENTS
!----------------------------------------------------------------------------------------------------------------------!
TYPE(tElemList),INTENT(IN) :: ElemList
INTEGER,INTENT(INOUT)      :: nBCFaces
!----------------------------------------------------------------------------------------------------------------------!
! LOCAL VARIABLES
!----------------------------------------------------------------------------------------------------------------------!
TYPE(tElem),POINTER :: aElem
TYPE(tSide),POINTER :: aSide
!----------------------------------------------------------------------------------------------------------------------!

! Compute nBCFaces
nBCFaces = 0
aElem => ElemList%FirstElem
DO WHILE (ASSOCIATED(aElem))
  aSide => aElem%FirstSide
  DO WHILE(ASSOCIATED(aSide))
    IF (ASSOCIATED(aSide%BC) .EQV. .TRUE.) THEN
      nBCFaces = nBCFaces+1
    END IF
    aSide => aSide%NextElemSide
  END DO
  aElem => aElem%NextElem
END DO

!----------------------------------------------------------------------------------------------------------------------!
END SUBROUTINE CountBCFaces
!======================================================================================================================!
!
!
!
!======================================================================================================================!
SUBROUTINE SetAndCountNodeID(iNodeID,jNodeID)
!----------------------------------------------------------------------------------------------------------------------!
IMPLICIT NONE
!----------------------------------------------------------------------------------------------------------------------!
! FORMAL ARGUMENTS
!----------------------------------------------------------------------------------------------------------------------!
INTEGER,INTENT(INOUT) :: iNodeID
INTEGER,INTENT(INOUT) :: jNodeID
!----------------------------------------------------------------------------------------------------------------------!
! LOCAL VARIABLES
!----------------------------------------------------------------------------------------------------------------------!

IF (iNodeID .EQ. 0) THEN
  jNodeID = jNodeID+1
  iNodeID = jNodeID
END IF

!----------------------------------------------------------------------------------------------------------------------!
END SUBROUTINE SetAndCountNodeID
!======================================================================================================================!
!
!
!
!======================================================================================================================!
FUNCTION CheckFaceIDIsInNodesToFacesMap(Data) RESULT(Flag)
!----------------------------------------------------------------------------------------------------------------------!
USE MOD_MeshMain_vars,ONLY: NodesToFacesMap_EDGE
USE MOD_MeshMain_vars,ONLY: NodesToFacesMap_TRI
USE MOD_MeshMain_vars,ONLY: NodesToFacesMap_QUAD
!----------------------------------------------------------------------------------------------------------------------!
USE MOD_DataStructures,ONLY: CheckBucketIDIsInHashTable
!----------------------------------------------------------------------------------------------------------------------!
USE MOD_Mesh_CGNS_Definitions
!----------------------------------------------------------------------------------------------------------------------!
IMPLICIT NONE
!----------------------------------------------------------------------------------------------------------------------!
! FORMAL ARGUMENTS
!----------------------------------------------------------------------------------------------------------------------!
INTEGER,INTENT(IN) :: Data(:)
LOGICAL            :: Flag
!----------------------------------------------------------------------------------------------------------------------!
! LOCAL VARIABLES
!----------------------------------------------------------------------------------------------------------------------!
INTEGER :: nFaceNodes_EDGE
INTEGER :: nFaceNodes_TRI
INTEGER :: nFaceNodes_QUAD
!----------------------------------------------------------------------------------------------------------------------!

CALL GetnElemNodes(ELEMTYPE_EDGE2,nFaceNodes_EDGE)
CALL GetnElemNodes(ELEMTYPE_TRI3,nFaceNodes_TRI)
CALL GetnElemNodes(ELEMTYPE_QUAD4,nFaceNodes_QUAD)

Flag = .FALSE.

IF (SIZE(Data) .EQ. nFaceNodes_EDGE) THEN
  Flag = CheckBucketIDIsInHashTable(NodesToFacesMap_EDGE,Data)
  RETURN
END IF

IF (SIZE(Data) .EQ. nFaceNodes_TRI) THEN
  Flag = CheckBucketIDIsInHashTable(NodesToFacesMap_TRI,Data)
  RETURN
END IF

IF (SIZE(Data) .EQ. nFaceNodes_QUAD) THEN
  Flag = CheckBucketIDIsInHashTable(NodesToFacesMap_QUAD,Data)
  RETURN
END IF

!----------------------------------------------------------------------------------------------------------------------!
END FUNCTION CheckFaceIDIsInNodesToFacesMap
!======================================================================================================================!
!
!
!
!======================================================================================================================!
FUNCTION GetFaceIDFromNodesToFacesMap(Data) RESULT(FaceID)
!----------------------------------------------------------------------------------------------------------------------!
USE MOD_MeshMain_vars,ONLY: NodesToFacesMap_EDGE
USE MOD_MeshMain_vars,ONLY: NodesToFacesMap_TRI
USE MOD_MeshMain_vars,ONLY: NodesToFacesMap_QUAD
!----------------------------------------------------------------------------------------------------------------------!
USE MOD_DataStructures,ONLY: GetBucketID
USE MOD_DataStructures,ONLY: CheckBucketIDIsInHashTable
!----------------------------------------------------------------------------------------------------------------------!
USE MOD_Mesh_CGNS_Definitions
!----------------------------------------------------------------------------------------------------------------------!
IMPLICIT NONE
!----------------------------------------------------------------------------------------------------------------------!
! FORMAL ARGUMENTS
!----------------------------------------------------------------------------------------------------------------------!
INTEGER,INTENT(IN) :: Data(:)
INTEGER            :: FaceID
!----------------------------------------------------------------------------------------------------------------------!
! LOCAL VARIABLES
!----------------------------------------------------------------------------------------------------------------------!
INTEGER :: nFaceNodes_EDGE
INTEGER :: nFaceNodes_TRI
INTEGER :: nFaceNodes_QUAD
!----------------------------------------------------------------------------------------------------------------------!

CALL GetnElemNodes(ELEMTYPE_EDGE2,nFaceNodes_EDGE)
CALL GetnElemNodes(ELEMTYPE_TRI3,nFaceNodes_TRI)
CALL GetnElemNodes(ELEMTYPE_QUAD4,nFaceNodes_QUAD)

IF (SIZE(Data) .EQ. nFaceNodes_EDGE) THEN
  IF (CheckBucketIDIsInHashTable(NodesToFacesMap_EDGE,Data) .EQV. .TRUE.) THEN
    FaceID = GetBucketID(NodesToFacesMap_EDGE,Data)
    RETURN
  END IF
END IF

IF (SIZE(Data) .EQ. nFaceNodes_TRI) THEN
  IF (CheckBucketIDIsInHashTable(NodesToFacesMap_TRI,Data) .EQV. .TRUE.) THEN
    FaceID = GetBucketID(NodesToFacesMap_TRI,Data)
    RETURN
  END IF
END IF

IF (SIZE(Data) .EQ. nFaceNodes_QUAD) THEN
  IF (CheckBucketIDIsInHashTable(NodesToFacesMap_QUAD,Data) .EQV. .TRUE.) THEN
    FaceID = GetBucketID(NodesToFacesMap_QUAD,Data)
    RETURN
  END IF
END IF

!----------------------------------------------------------------------------------------------------------------------!
END FUNCTION GetFaceIDFromNodesToFacesMap
!======================================================================================================================!
!
!
!
!----------------------------------------------------------------------------------------------------------------------!
END MODULE MOD_MeshMainMethods
!======================================================================================================================!
