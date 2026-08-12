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
MODULE MOD_Quadtrees
!----------------------------------------------------------------------------------------------------------------------!
USE MOD_GLOBAL_vars
!----------------------------------------------------------------------------------------------------------------------!
IMPLICIT NONE
!----------------------------------------------------------------------------------------------------------------------!
PRIVATE
!----------------------------------------------------------------------------------------------------------------------!
! GLOBAL VARIABLES
!----------------------------------------------------------------------------------------------------------------------!
INTERFACE Refine_Quadtree
  MODULE PROCEDURE Refine_Quadtree
END INTERFACE

INTERFACE GetForestSize
  MODULE PROCEDURE GetForestSize
END INTERFACE

INTERFACE CreateQuadtreeMesh
  MODULE PROCEDURE CreateQuadtreeMesh
END INTERFACE
!----------------------------------------------------------------------------------------------------------------------!
TYPE tPoint2D
  INTEGER :: PointID
  REAL    :: PointCoords(1:2)
END TYPE
!----------------------------------------------------------------------------------------------------------------------!
TYPE tQuadNode
  INTEGER :: Level
  INTEGER :: QuadID
  REAL    :: QuadCoords(1:4,1:2)
  LOGICAL :: isActive
  INTEGER :: nPoints
  INTEGER,ALLOCATABLE :: PointsID(:)
  TYPE(tQuadNode),POINTER :: Parent
  TYPE(tQuadNode),POINTER :: Children(:)
END TYPE
!----------------------------------------------------------------------------------------------------------------------!
TYPE tQuadtree
  TYPE(tQuadNode),POINTER :: TreeNode
  CONTAINS
  PROCEDURE :: Construct  => Construct_Quadtree
!     PROCEDURE :: TreeSize     => TreeSize_Quadtree
!     PROCEDURE :: Empty        => Empty_Quadtree
!     PROCEDURE :: Root         => Root_Quadtree
!     PROCEDURE :: Elems        => Elems_Quadtree
!   PROCEDURE :: AddQuadtree => AddQuadtree_Quadtree
END TYPE
!----------------------------------------------------------------------------------------------------------------------!
TYPE tQuadtreesForest
  TYPE(tQuadtree),ALLOCATABLE :: Trees(:)
  CONTAINS
  PROCEDURE :: Construct  => Construct_QuadtreeForest
  PROCEDURE :: Destruct    => Destruct_QuadtreeForest
  PROCEDURE :: PrintForest => Print_QuadtreeForest
END TYPE
!----------------------------------------------------------------------------------------------------------------------!
INTEGER :: nMaxLevels   = 10
INTEGER :: nMaxPoints = 1
!----------------------------------------------------------------------------------------------------------------------!
PUBLIC :: tPoint2D
PUBLIC :: tQuadtreesForest
!----------------------------------------------------------------------------------------------------------------------!
PUBLIC :: GetForestSize
PUBLIC :: Refine_Quadtree
PUBLIC :: CreateQuadtreeMesh
!----------------------------------------------------------------------------------------------------------------------!
CHARACTER(LEN=256) :: ModuleName = "DataStructures::Quadtrees"
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
SUBROUTINE GetData_QuadNode(QuadNode,QuadID,QuadCoords,Level,isActive,nPoints,PointsID)
!----------------------------------------------------------------------------------------------------------------------!
IMPLICIT NONE
!----------------------------------------------------------------------------------------------------------------------!
! FORMAL ARGUMENTS
!----------------------------------------------------------------------------------------------------------------------!
TYPE(tQuadNode),INTENT(IN)   :: QuadNode
INTEGER,INTENT(OUT),OPTIONAL :: QuadID
REAL   ,INTENT(OUT),OPTIONAL :: QuadCoords(1:4,1:2)
INTEGER,INTENT(OUT),OPTIONAL :: Level
LOGICAL,INTENT(OUT),OPTIONAL :: isActive
INTEGER,INTENT(OUT),OPTIONAL :: nPoints
INTEGER,INTENT(OUT),OPTIONAL :: PointsID(:)
!----------------------------------------------------------------------------------------------------------------------!
! LOCAL VARIABLES
!----------------------------------------------------------------------------------------------------------------------!

IF (PRESENT(QuadID)) THEN
  QuadID = QuadNode%QuadID
END IF
IF (PRESENT(QuadCoords)) THEN
  QuadCoords = QuadNode%QuadCoords
END IF
IF (PRESENT(Level)) THEN
  Level = QuadNode%Level
END IF
IF (PRESENT(isActive)) THEN
  isActive = QuadNode%isActive
END IF
IF (PRESENT(nPoints)) THEN
  nPoints = QuadNode%nPoints
END IF
IF (PRESENT(PointsID)) THEN
  PointsID = QuadNode%PointsID
END IF

!----------------------------------------------------------------------------------------------------------------------!
END SUBROUTINE GetData_QuadNode
!======================================================================================================================!
!
!
!
!======================================================================================================================!
RECURSIVE SUBROUTINE Clean_QuadNode(QuadNode)
!----------------------------------------------------------------------------------------------------------------------!
IMPLICIT NONE
!----------------------------------------------------------------------------------------------------------------------!
! FORMAL ARGUMENTS
!----------------------------------------------------------------------------------------------------------------------!
TYPE(tQuadNode),INTENT(INOUT) :: QuadNode
!----------------------------------------------------------------------------------------------------------------------!
! LOCAL VARIABLES
!----------------------------------------------------------------------------------------------------------------------!
INTEGER :: iChild
!----------------------------------------------------------------------------------------------------------------------!

IF (ASSOCIATED(QuadNode%Children)) THEN
  DO iChild=1,4
    CALL Clean_QuadNode(QuadNode%Children(iChild))
    DEALLOCATE(QuadNode%Children(iChild)%PointsID)
  END DO
  DEALLOCATE(QuadNode%Children)
END IF

!----------------------------------------------------------------------------------------------------------------------!
END SUBROUTINE Clean_QuadNode
!======================================================================================================================!
!
!
!
!======================================================================================================================!
SUBROUTINE Reset_QuadNode(QuadNode)
!----------------------------------------------------------------------------------------------------------------------!
IMPLICIT NONE
!----------------------------------------------------------------------------------------------------------------------!
! FORMAL ARGUMENTS
!----------------------------------------------------------------------------------------------------------------------!
TYPE(tQuadNode),INTENT(INOUT) :: QuadNode
!----------------------------------------------------------------------------------------------------------------------!
! LOCAL VARIABLES
!----------------------------------------------------------------------------------------------------------------------!

QuadNode%nPoints = 0
IF (.NOT. ALLOCATED(QuadNode%PointsID)) THEN
  ALLOCATE(QuadNode%PointsID(1:nMaxPoints))
END IF
NULLIFY(QuadNode%Parent)
IF (SIZE(QuadNode%Children) .EQ. 4) THEN
  DEALLOCATE(QuadNode%Children)
END IF
NULLIFY(QuadNode%Children)

!----------------------------------------------------------------------------------------------------------------------!
END SUBROUTINE Reset_QuadNode
!======================================================================================================================!
!
!
!
!======================================================================================================================!
RECURSIVE SUBROUTINE Print_Quadtree(QuadNode)
!----------------------------------------------------------------------------------------------------------------------!
IMPLICIT NONE
!----------------------------------------------------------------------------------------------------------------------!
! FORMAL ARGUMENTS
!----------------------------------------------------------------------------------------------------------------------!
TYPE(tQuadNode),INTENT(IN) :: QuadNode
!----------------------------------------------------------------------------------------------------------------------!
! LOCAL VARIABLES
!----------------------------------------------------------------------------------------------------------------------!
INTEGER :: iChild
INTEGER :: iCorner
!----------------------------------------------------------------------------------------------------------------------!
INTEGER :: QuadID
REAL    :: QuadCoords(1:4,1:2)
INTEGER :: Level
LOGICAL :: isActive
INTEGER :: nPoints
!----------------------------------------------------------------------------------------------------------------------!
CHARACTER(LEN=256) :: StrFormat
!----------------------------------------------------------------------------------------------------------------------!

IF (ASSOCIATED(QuadNode%Children)) THEN
  CALL GetData_QuadNode(QuadNode,QuadID,QuadCoords,Level,isActive,nPoints)
  WRITE(UNIT_SCREEN,*) "Branch Node"
  WRITE(UNIT_SCREEN,"(1X,A,I0,1X,A,I0,1X,A,L1,1X,A,I0)") &
    " - QuadID: ", QuadID, &
    " - Level: ", Level, &
    " - isActive: ", isActive, &
    " - nPoints: ", nPoints
  DO iCorner=1,4
    WRITE(UNIT_SCREEN,"(2X,A1,SP,ES13.6,A1,SP,ES13.6,A1)") "[", QuadCoords(iCorner,1), ",", QuadCoords(iCorner,2), "]"
  END DO
  DO iChild=1,4
    CALL Print_Quadtree(QuadNode%Children(iChild))
  END DO
ELSE
!   IF (QuadNode%nPoints .EQ. 0) THEN
!     RETURN
!   END IF
  CALL GetData_QuadNode(QuadNode,QuadID,QuadCoords,Level,isActive,nPoints)
  WRITE(StrFormat,"(A2,I0,A31)") "(", 1, "X,A1,SP,ES13.6,A1,SP,ES13.6,A1)"
  IF (Level .GT. 0) THEN
    WRITE(StrFormat,"(A2,I0,A31)") "(", 2*(Level+1), "X,A1,SP,ES13.6,A1,SP,ES13.6,A1)"
  END IF
  WRITE(UNIT_SCREEN,*) "Leaf Node"
  WRITE(UNIT_SCREEN,"(1X,A,I0,1X,A,I0,1X,A,L1,1X,A,I0)") &
    " - QuadID: ", QuadID, &
    " - Level: ", Level, &
    " - isActive: ", isActive, &
    " - nPoints: ", nPoints
  DO iCorner=1,4
    WRITE(UNIT_SCREEN,StrFormat) "[", QuadCoords(iCorner,1), ",", QuadCoords(iCorner,2), "]"
  END DO
END IF

!----------------------------------------------------------------------------------------------------------------------!
END SUBROUTINE Print_Quadtree
!======================================================================================================================!
!
!
!
!======================================================================================================================!
RECURSIVE SUBROUTINE Refine_Quadtree(QuadNode,Points,ElemID)
!----------------------------------------------------------------------------------------------------------------------!
IMPLICIT NONE
!----------------------------------------------------------------------------------------------------------------------!
! FORMAL ARGUMENTS
!----------------------------------------------------------------------------------------------------------------------!
TYPE(tQuadNode),TARGET,INTENT(INOUT) :: QuadNode
TYPE(tPoint2D),TARGET,INTENT(IN)       :: Points(:)
INTEGER,INTENT(INOUT)                :: ElemID
!----------------------------------------------------------------------------------------------------------------------!
! LOCAL VARIABLES
!----------------------------------------------------------------------------------------------------------------------!
INTEGER :: iPoint
INTEGER :: jPoint
INTEGER :: iChild
!----------------------------------------------------------------------------------------------------------------------!
INTEGER                  :: nContainedPoints
TYPE(tPoint2D),ALLOCATABLE :: ContainedPoints(:)
!----------------------------------------------------------------------------------------------------------------------!

IF ((QuadNode%Level .GE. nMaxLevels) .OR. (SIZE(Points) .LE. nMaxPoints)) THEN
  IF (SIZE(Points) .GT. SIZE(QuadNode%PointsID)) THEN
    DEALLOCATE(QuadNode%PointsID)
    ALLOCATE(QuadNode%PointsID(1:SIZE(Points)))
  END IF
  jPoint = 1
  DO iPoint=1,SIZE(Points)
    IF (PointIsOutsideBox(Points(iPoint)%PointCoords(1:2),QuadNode%QuadCoords(1:4,1:2))) THEN
      CYCLE
    END IF
    QuadNode%PointsID(jPoint) = Points(iPoint)%PointID
    jPoint = jPoint+1
  END DO
  QuadNode%nPoints = jPoint-1
! ! !   IF (QuadNode%isActive .EQV. .TRUE.) THEN
! ! !     QuadNode%QuadID = ElemID
! ! !   END IF
  RETURN
END IF

nContainedPoints = 0
DO iPoint=1,SIZE(Points)
  IF (PointIsOutsideBox(Points(iPoint)%PointCoords(1:2),QuadNode%QuadCoords(1:4,1:2))) THEN
    CYCLE
  END IF
  nContainedPoints = nContainedPoints+1
END DO

IF (nContainedPoints .EQ. 0) THEN
  IF (QuadNode%isActive .EQV. .TRUE.) THEN
    QuadNode%QuadID = ElemID
  END IF
  RETURN
END IF

ALLOCATE(ContainedPoints(1:nContainedPoints))

jPoint = 1
DO iPoint=1,SIZE(Points)
  IF (PointIsOutsideBox(Points(iPoint)%PointCoords(1:2),QuadNode%QuadCoords(1:4,1:2))) THEN
    CYCLE
  END IF
  ContainedPoints(jPoint)%PointID     = Points(iPoint)%PointID
  ContainedPoints(jPoint)%PointCoords = Points(iPoint)%PointCoords
  jPoint = jPoint+1
END DO

QuadNode%isActive = .FALSE.
QuadNode%QuadID   = -999

CALL Split_QuadNode(QuadNode,ElemID)
DO iChild=1,4
  CALL Refine_Quadtree(QuadNode%Children(iChild),ContainedPoints,ElemID)
END DO

!----------------------------------------------------------------------------------------------------------------------!
END SUBROUTINE Refine_Quadtree
!======================================================================================================================!
!
!
!
!======================================================================================================================!
RECURSIVE SUBROUTINE Split_QuadNode(QuadNode,ElemID)
!----------------------------------------------------------------------------------------------------------------------!
IMPLICIT NONE
!----------------------------------------------------------------------------------------------------------------------!
! FORMAL ARGUMENTS
!----------------------------------------------------------------------------------------------------------------------!
TYPE(tQuadNode),TARGET,INTENT(INOUT) :: QuadNode
INTEGER,INTENT(INOUT)                :: ElemID
!----------------------------------------------------------------------------------------------------------------------!
! LOCAL VARIABLES
!----------------------------------------------------------------------------------------------------------------------!
INTEGER :: ii
INTEGER :: jj
INTEGER :: iChild
REAL    :: dx
REAL    :: dy
!----------------------------------------------------------------------------------------------------------------------!

ALLOCATE(QuadNode%Children(1:4))

dx = ABS(QuadNode%QuadCoords(2,1)-QuadNode%QuadCoords(1,1))
dy = ABS(QuadNode%QuadCoords(3,2)-QuadNode%QuadCoords(2,2))

iChild = 1
ElemID = ElemID+1
DO jj=1,2
  DO ii=1,2
    CALL Reset_QuadNode(QuadNode%Children(iChild))
    QuadNode%Children(iChild)%Level    = QuadNode%Level+1
    QuadNode%Children(iChild)%Parent   => QuadNode
    QuadNode%Children(iChild)%isActive = .TRUE.
    QuadNode%Children(iChild)%QuadID   = ElemID
    QuadNode%Children(iChild)%QuadCoords(1,1) = QuadNode%QuadCoords(1,1) + 0.5*(ii-1)*dx
    QuadNode%Children(iChild)%QuadCoords(1,2) = QuadNode%QuadCoords(1,2) + 0.5*(jj-1)*dy
    QuadNode%Children(iChild)%QuadCoords(3,1) = QuadNode%QuadCoords(3,1) - 0.5*(2-ii)*dx
    QuadNode%Children(iChild)%QuadCoords(3,2) = QuadNode%QuadCoords(3,2) - 0.5*(2-jj)*dy
    
    QuadNode%Children(iChild)%QuadCoords(2,1) = QuadNode%QuadCoords(2,1) - 0.5*(2-ii)*dx
    QuadNode%Children(iChild)%QuadCoords(2,2) = QuadNode%QuadCoords(2,2) + 0.5*(jj-1)*dy
    QuadNode%Children(iChild)%QuadCoords(4,1) = QuadNode%QuadCoords(4,1) + 0.5*(ii-1)*dx
    QuadNode%Children(iChild)%QuadCoords(4,2) = QuadNode%QuadCoords(4,2) - 0.5*(2-jj)*dy
    iChild = iChild+1
    ElemID = ElemID+1
  END DO
END DO

!----------------------------------------------------------------------------------------------------------------------!
END SUBROUTINE Split_QuadNode
!======================================================================================================================!
!
!
!
!======================================================================================================================!
FUNCTION PointIsOutsideBox(Point,Box) RESULT(Flag)
!----------------------------------------------------------------------------------------------------------------------!
IMPLICIT NONE
!----------------------------------------------------------------------------------------------------------------------!
! FORMAL ARGUMENTS
!----------------------------------------------------------------------------------------------------------------------!
REAL,INTENT(IN)       :: Point(1:2)
REAL,INTENT(IN)       :: Box(1:4,1:2)
LOGICAL               :: Flag
!----------------------------------------------------------------------------------------------------------------------!
! LOCAL VARIABLES
!----------------------------------------------------------------------------------------------------------------------!

Flag = Point(1) < Box(1,1) .OR. Point(1) > Box(3,1) .OR. &
       Point(2) < Box(1,2) .OR. Point(2) > Box(3,2)

!----------------------------------------------------------------------------------------------------------------------!
END FUNCTION PointIsOutsideBox
!======================================================================================================================!
!
!
!
!======================================================================================================================!
SUBROUTINE Construct_Quadtree(self,QuadID,QuadCoords)
!----------------------------------------------------------------------------------------------------------------------!
IMPLICIT NONE
!----------------------------------------------------------------------------------------------------------------------!
! FORMAL ARGUMENTS
!----------------------------------------------------------------------------------------------------------------------!
CLASS(tQuadtree),INTENT(INOUT) :: self
INTEGER,INTENT(IN)             :: QuadID
REAL,INTENT(IN)                :: QuadCoords(1:4,1:2)
!----------------------------------------------------------------------------------------------------------------------!
! LOCAL VARIABLES
!----------------------------------------------------------------------------------------------------------------------!

NULLIFY(self%TreeNode)
IF (.NOT. ASSOCIATED(self%TreeNode)) THEN
  ALLOCATE(self%TreeNode)
END IF

CALL Reset_QuadNode(self%TreeNode)

self%TreeNode%Level = 0
self%TreeNode%QuadID = -999 !QuadID
self%TreeNode%QuadCoords(1:4,1:2) = QuadCoords

self%TreeNode%isActive = .TRUE.


! ! ! self%TreeNode%nPoints = 0
! ! ! self%TreeNode%PointsID = 0

! ! ! NULLIFY(self%TreeNode%Parent)
! ! ! NULLIFY(self%TreeNode%Children)

!----------------------------------------------------------------------------------------------------------------------!
END SUBROUTINE Construct_Quadtree
!======================================================================================================================!
!
!
!
!======================================================================================================================!
SUBROUTINE Construct_QuadtreeForest(self,nTrees)
!----------------------------------------------------------------------------------------------------------------------!
IMPLICIT NONE
!----------------------------------------------------------------------------------------------------------------------!
! FORMAL ARGUMENTS
!----------------------------------------------------------------------------------------------------------------------!
CLASS(tQuadtreesForest),INTENT(INOUT) :: self
INTEGER,INTENT(IN)                    :: nTrees
!----------------------------------------------------------------------------------------------------------------------!
! LOCAL VARIABLES
!----------------------------------------------------------------------------------------------------------------------!

ALLOCATE(self%Trees(1:nTrees))

!----------------------------------------------------------------------------------------------------------------------!
END SUBROUTINE Construct_QuadtreeForest
!======================================================================================================================!
!
!
!
!======================================================================================================================!
SUBROUTINE Destruct_QuadtreeForest(self)
!----------------------------------------------------------------------------------------------------------------------!
IMPLICIT NONE
!----------------------------------------------------------------------------------------------------------------------!
! FORMAL ARGUMENTS
!----------------------------------------------------------------------------------------------------------------------!
CLASS(tQuadtreesForest),INTENT(INOUT) :: self
!----------------------------------------------------------------------------------------------------------------------!
! LOCAL VARIABLES
!----------------------------------------------------------------------------------------------------------------------!
INTEGER :: iTree
INTEGER :: nTrees
!----------------------------------------------------------------------------------------------------------------------!
CHARACTER(LEN=256) :: ErrorMessage
CHARACTER(LEN=256) :: FunctionName = "Destruct_QuadtreeForest"
!----------------------------------------------------------------------------------------------------------------------!

IF (.NOT. ALLOCATED(self%Trees)) THEN
  ErrorMessage = "Forest can not be destroyed. Forest has not been allocated."
  CALL PrintError(__STAMP__,ErrorMessage,ModuleName,FunctionName)
END IF

nTrees = SIZE(self%Trees(:))
DO iTree=1,nTrees
  CALL Clean_QuadNode(self%Trees(iTree)%TreeNode)
END DO

DEALLOCATE(self%Trees)

!----------------------------------------------------------------------------------------------------------------------!
END SUBROUTINE Destruct_QuadtreeForest
!======================================================================================================================!
!
!
!
!======================================================================================================================!
SUBROUTINE Print_QuadtreeForest(self)
!----------------------------------------------------------------------------------------------------------------------!
IMPLICIT NONE
!----------------------------------------------------------------------------------------------------------------------!
! FORMAL ARGUMENTS
!----------------------------------------------------------------------------------------------------------------------!
CLASS(tQuadtreesForest),INTENT(INOUT) :: self
!----------------------------------------------------------------------------------------------------------------------!
! LOCAL VARIABLES
!----------------------------------------------------------------------------------------------------------------------!
INTEGER            :: iTree
INTEGER            :: nTrees
CHARACTER(LEN=256) :: ErrorMessage
CHARACTER(LEN=256) :: FunctionName = "Print_QuadtreeForest"
!----------------------------------------------------------------------------------------------------------------------!

IF (.NOT. ALLOCATED(self%Trees)) THEN
  ErrorMessage = "Forest can not be printed. Forest has not been allocated."
  CALL PrintError(__STAMP__,ErrorMessage,ModuleName,FunctionName)
END IF

nTrees = SIZE(self%Trees(:))
DO iTree=1,nTrees
  CALL Print_Quadtree(self%Trees(iTree)%TreeNode)
  WRITE(UNIT_SCREEN,*)
END DO

!----------------------------------------------------------------------------------------------------------------------!
END SUBROUTINE Print_QuadtreeForest
!======================================================================================================================!
!
!
!
!======================================================================================================================!
SUBROUTINE CreateQuadtreeMesh(Forest,ElemsNodesArray)
!----------------------------------------------------------------------------------------------------------------------!
IMPLICIT NONE
!----------------------------------------------------------------------------------------------------------------------!
! FORMAL ARGUMENTS
!----------------------------------------------------------------------------------------------------------------------!
TYPE(tQuadtreesForest),INTENT(IN) :: Forest
REAL,ALLOCATABLE,INTENT(OUT)      :: ElemsNodesArray(:,:,:)
!----------------------------------------------------------------------------------------------------------------------!
! LOCAL VARIABLES
!----------------------------------------------------------------------------------------------------------------------!
INTEGER :: iTree
INTEGER :: nTrees
INTEGER :: ElemID
INTEGER :: nElems
INTEGER :: ForestSize
!----------------------------------------------------------------------------------------------------------------------!

CALL GetForestSize(Forest,ForestSize)
nElems = ForestSize
ALLOCATE(ElemsNodesArray(1:4,1:2,1:nElems))

ElemID = 0
nTrees = SIZE(Forest%Trees(:))
DO iTree=1,nTrees
  CALL GetQuadtreeCoords(Forest%Trees(iTree)%TreeNode,nElems,ElemID,ElemsNodesArray(1:4,1:2,1:nElems))
END DO

!----------------------------------------------------------------------------------------------------------------------!
END SUBROUTINE CreateQuadtreeMesh
!======================================================================================================================!
!
!
!
!======================================================================================================================!
RECURSIVE SUBROUTINE GetQuadtreeCoords(QuadNode,nElems,ElemID,ElemsNodesArray)
!----------------------------------------------------------------------------------------------------------------------!
IMPLICIT NONE
!----------------------------------------------------------------------------------------------------------------------!
! FORMAL ARGUMENTS
!----------------------------------------------------------------------------------------------------------------------!
TYPE(tQuadNode),INTENT(IN) :: QuadNode
INTEGER,INTENT(IN)         :: nElems
INTEGER,INTENT(INOUT)      :: ElemID
REAL,INTENT(INOUT)         :: ElemsNodesArray(1:4,1:2,1:nElems)
!----------------------------------------------------------------------------------------------------------------------!
! LOCAL VARIABLES
!----------------------------------------------------------------------------------------------------------------------!
INTEGER :: iChild
!----------------------------------------------------------------------------------------------------------------------!
REAL    :: QuadCoords(1:4,1:2)
LOGICAL :: isActive
!----------------------------------------------------------------------------------------------------------------------!

IF (.NOT. ASSOCIATED(QuadNode%Children)) THEN
  CALL GetData_QuadNode(QuadNode=QuadNode,isActive=isActive,QuadCoords=QuadCoords)
  IF (isActive .EQV. .TRUE.) THEN
    ElemID = ElemID+1
    ElemsNodesArray(1:4,1:2,ElemID) = QuadCoords(1:4,1:2)
  END IF
ELSE
  DO iChild=1,4
    CALL GetQuadtreeCoords(QuadNode%Children(iChild),nElems,ElemID,ElemsNodesArray)
  END DO
END IF

!----------------------------------------------------------------------------------------------------------------------!
END SUBROUTINE GetQuadtreeCoords
!======================================================================================================================!
!
!
!
!======================================================================================================================!
RECURSIVE SUBROUTINE GetQuadtreeSize(QuadNode,TreeSize)
!----------------------------------------------------------------------------------------------------------------------!
IMPLICIT NONE
!----------------------------------------------------------------------------------------------------------------------!
! FORMAL ARGUMENTS
!----------------------------------------------------------------------------------------------------------------------!
TYPE(tQuadNode),INTENT(INOUT) :: QuadNode
INTEGER,INTENT(INOUT)           :: TreeSize
!----------------------------------------------------------------------------------------------------------------------!
! LOCAL VARIABLES
!----------------------------------------------------------------------------------------------------------------------!
INTEGER :: iChild
!----------------------------------------------------------------------------------------------------------------------!
LOGICAL :: isActive
!----------------------------------------------------------------------------------------------------------------------!

IF (.NOT. ASSOCIATED(QuadNode%Children)) THEN
  CALL GetData_QuadNode(QuadNode=QuadNode,isActive=isActive)
  IF (isActive .EQV. .TRUE.) THEN
    TreeSize = TreeSize+1
  END IF
ELSE
  DO iChild=1,4
    CALL GetQuadtreeSize(QuadNode%Children(iChild),TreeSize)
  END DO
END IF

!----------------------------------------------------------------------------------------------------------------------!
END SUBROUTINE GetQuadtreeSize
!======================================================================================================================!
!
!
!
!======================================================================================================================!
SUBROUTINE GetForestSize(Forest,ForestSize)
!----------------------------------------------------------------------------------------------------------------------!
IMPLICIT NONE
!----------------------------------------------------------------------------------------------------------------------!
! FORMAL ARGUMENTS
!----------------------------------------------------------------------------------------------------------------------!
TYPE(tQuadtreesForest),INTENT(IN) :: Forest
INTEGER,INTENT(OUT)               :: ForestSize
!----------------------------------------------------------------------------------------------------------------------!
! LOCAL VARIABLES
!----------------------------------------------------------------------------------------------------------------------!
INTEGER :: iTree
INTEGER :: nTrees
!----------------------------------------------------------------------------------------------------------------------!

nTrees = SIZE(Forest%Trees(:))

ForestSize = 0
DO iTree=1,nTrees
  CALL GetQuadtreeSize(Forest%Trees(iTree)%TreeNode,ForestSize)
END DO


!----------------------------------------------------------------------------------------------------------------------!
END SUBROUTINE GetForestSize
!======================================================================================================================!
!
!
!
!----------------------------------------------------------------------------------------------------------------------!
END MODULE MOD_Quadtrees
!======================================================================================================================!
