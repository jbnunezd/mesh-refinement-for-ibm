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
MODULE MOD_Octrees
!----------------------------------------------------------------------------------------------------------------------!
USE MOD_GLOBAL_vars
!----------------------------------------------------------------------------------------------------------------------!
IMPLICIT NONE
!----------------------------------------------------------------------------------------------------------------------!
PRIVATE
!----------------------------------------------------------------------------------------------------------------------!
! GLOBAL VARIABLES
!----------------------------------------------------------------------------------------------------------------------!
TYPE tPoint3D
  INTEGER :: PointID
  REAL    :: PointCoords(1:3)
END TYPE
!----------------------------------------------------------------------------------------------------------------------!
TYPE tOctaNode
  INTEGER :: Level
  INTEGER :: OctaID
  REAL    :: OctaCoords(1:8,1:3)
  LOGICAL :: isActive
  INTEGER :: nPoints
  TYPE(tPoint3D),ALLOCATABLE :: Points(:)
  TYPE(tOctaNode),POINTER :: Parent
  TYPE(tOctaNode),POINTER :: Children(:)
  TYPE(tOctaNode),POINTER :: PrevOctaNode
  TYPE(tOctaNode),POINTER :: NextOctaNode
END TYPE
!----------------------------------------------------------------------------------------------------------------------!
TYPE tOctree
  TYPE(tOctaNode),POINTER :: TreeNode
  CONTAINS
  PROCEDURE :: Construct  => Construct_Octree
END TYPE
!----------------------------------------------------------------------------------------------------------------------!
TYPE tOctreesForest
  TYPE(tOctree),ALLOCATABLE :: Trees(:)
  CONTAINS
  PROCEDURE :: Construct   => Construct_OctreeForest
  PROCEDURE :: Destruct    => Destruct_OctreeForest
  PROCEDURE :: ForestSize  => ForestSize_OctreeForest
  PROCEDURE :: PrintForest => Print_OctreeForest
END TYPE
!----------------------------------------------------------------------------------------------------------------------!
PUBLIC :: tPoint3D
PUBLIC :: tOctaNode
PUBLIC :: tOctreesForest
!----------------------------------------------------------------------------------------------------------------------!
INTERFACE Split_OctaNode
  MODULE PROCEDURE Split_OctaNode
END INTERFACE

INTERFACE GetData_OctaNode
  MODULE PROCEDURE GetData_OctaNode
END INTERFACE
!----------------------------------------------------------------------------------------------------------------------!
PUBLIC :: Split_OctaNode
PUBLIC :: GetData_OctaNode
!----------------------------------------------------------------------------------------------------------------------!
CHARACTER(LEN=256) :: ModuleName = "DataStructures::Octrees"
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
SUBROUTINE GetData_OctaNode(OctaNode,OctaID,OctaCoords,Level,isActive,nPoints,PointsID)
!----------------------------------------------------------------------------------------------------------------------!
IMPLICIT NONE
!----------------------------------------------------------------------------------------------------------------------!
! FORMAL ARGUMENTS
!----------------------------------------------------------------------------------------------------------------------!
TYPE(tOctaNode),INTENT(IN)   :: OctaNode
INTEGER,INTENT(OUT),OPTIONAL :: OctaID
REAL   ,INTENT(OUT),OPTIONAL :: OctaCoords(1:8,1:3)
INTEGER,INTENT(OUT),OPTIONAL :: Level
LOGICAL,INTENT(OUT),OPTIONAL :: isActive
INTEGER,INTENT(OUT),OPTIONAL :: nPoints
INTEGER,INTENT(OUT),OPTIONAL :: PointsID(:)
!----------------------------------------------------------------------------------------------------------------------!
! LOCAL VARIABLES
!----------------------------------------------------------------------------------------------------------------------!

IF (PRESENT(OctaID)) THEN
  OctaID = OctaNode%OctaID
END IF
IF (PRESENT(OctaCoords)) THEN
  OctaCoords = OctaNode%OctaCoords
END IF
IF (PRESENT(Level)) THEN
  Level = OctaNode%Level
END IF
IF (PRESENT(isActive)) THEN
  isActive = OctaNode%isActive
END IF
IF (PRESENT(nPoints)) THEN
  nPoints = OctaNode%nPoints
END IF

!----------------------------------------------------------------------------------------------------------------------!
END SUBROUTINE GetData_OctaNode
!======================================================================================================================!
!
!
!
!======================================================================================================================!
RECURSIVE SUBROUTINE Clean_OctaNode(OctaNode)
!----------------------------------------------------------------------------------------------------------------------!
IMPLICIT NONE
!----------------------------------------------------------------------------------------------------------------------!
! FORMAL ARGUMENTS
!----------------------------------------------------------------------------------------------------------------------!
TYPE(tOctaNode),INTENT(INOUT) :: OctaNode
!----------------------------------------------------------------------------------------------------------------------!
! LOCAL VARIABLES
!----------------------------------------------------------------------------------------------------------------------!
INTEGER :: iChild
!----------------------------------------------------------------------------------------------------------------------!

IF (ASSOCIATED(OctaNode%Children)) THEN
  DO iChild=1,8
    CALL Clean_OctaNode(OctaNode%Children(iChild))
    DEALLOCATE(OctaNode%Children(iChild)%Points)
  END DO
  DEALLOCATE(OctaNode%Children)
END IF

!----------------------------------------------------------------------------------------------------------------------!
END SUBROUTINE Clean_OctaNode
!======================================================================================================================!
!
!
!
!======================================================================================================================!
SUBROUTINE Reset_OctaNode(OctaNode)
!----------------------------------------------------------------------------------------------------------------------!
IMPLICIT NONE
!----------------------------------------------------------------------------------------------------------------------!
! FORMAL ARGUMENTS
!----------------------------------------------------------------------------------------------------------------------!
TYPE(tOctaNode),INTENT(INOUT) :: OctaNode
!----------------------------------------------------------------------------------------------------------------------!
! LOCAL VARIABLES
!----------------------------------------------------------------------------------------------------------------------!

OctaNode%nPoints = 0
NULLIFY(OctaNode%Parent)
NULLIFY(OctaNode%Children)
NULLIFY(OctaNode%PrevOctaNode)
NULLIFY(OctaNode%NextOctaNode)

!----------------------------------------------------------------------------------------------------------------------!
END SUBROUTINE Reset_OctaNode
!======================================================================================================================!
!
!
!
!======================================================================================================================!
RECURSIVE SUBROUTINE Print_Octree(OctaNode)
!----------------------------------------------------------------------------------------------------------------------!
IMPLICIT NONE
!----------------------------------------------------------------------------------------------------------------------!
! FORMAL ARGUMENTS
!----------------------------------------------------------------------------------------------------------------------!
TYPE(tOctaNode),INTENT(IN) :: OctaNode
!----------------------------------------------------------------------------------------------------------------------!
! LOCAL VARIABLES
!----------------------------------------------------------------------------------------------------------------------!
INTEGER :: iChild
INTEGER :: iCorner
!----------------------------------------------------------------------------------------------------------------------!
INTEGER :: OctaID
REAL    :: OctaCoords(1:8,1:3)
INTEGER :: Level
LOGICAL :: isActive
INTEGER :: nPoints
!----------------------------------------------------------------------------------------------------------------------!
CHARACTER(LEN=256) :: StrFormat
!----------------------------------------------------------------------------------------------------------------------!

IF (ASSOCIATED(OctaNode%Children)) THEN
  CALL GetData_OctaNode(OctaNode,OctaID,OctaCoords,Level,isActive,nPoints)
  WRITE(UNIT_SCREEN,*) "Branch Node"
  WRITE(UNIT_SCREEN,"(1X,A,I0,1X,A,I0,1X,A,L1,1X,A,I0)") &
    " - OctaID: ", OctaID, &
    " - Level: ", Level, &
    " - isActive: ", isActive, &
    " - nPoints: ", nPoints
  DO iCorner=1,8
    WRITE(UNIT_SCREEN,"(2X,A1,SP,ES13.6,A1,SP,ES13.6,A1,SP,ES13.6,A1)") &
      "[", OctaCoords(iCorner,1), ",", OctaCoords(iCorner,2), ",", OctaCoords(iCorner,3), "]"
  END DO
  DO iChild=1,8
    CALL Print_Octree(OctaNode%Children(iChild))
  END DO
ELSE
!   IF (OctaNode%nPoints .EQ. 0) THEN
!     RETURN
!   END IF
  CALL GetData_OctaNode(OctaNode,OctaID,OctaCoords,Level,isActive,nPoints)
  WRITE(StrFormat,"(A2,I0,A44)") "(", 1, "X,A1,SP,ES13.6,A1,SP,ES13.6,A1,SP,ES13.6,A1)"
  IF (Level .GT. 0) THEN
    WRITE(StrFormat,"(A2,I0,A44)") "(", 2*(Level+1), "X,A1,SP,ES13.6,A1,SP,ES13.6,A1,SP,ES13.6,A1)"
  END IF
  WRITE(UNIT_SCREEN,*) "Leaf Node"
  WRITE(UNIT_SCREEN,"(1X,A,I0,1X,A,I0,1X,A,L1,1X,A,I0)") &
    " - OctaID: ", OctaID, &
    " - Level: ", Level, &
    " - isActive: ", isActive, &
    " - nPoints: ", nPoints
  DO iCorner=1,8
    WRITE(UNIT_SCREEN,StrFormat) "[", OctaCoords(iCorner,1), ",", OctaCoords(iCorner,2), ",", OctaCoords(iCorner,3), "]"
  END DO
END IF

!----------------------------------------------------------------------------------------------------------------------!
END SUBROUTINE Print_Octree
!======================================================================================================================!
!
!
!
!======================================================================================================================!
RECURSIVE SUBROUTINE Split_OctaNode(OctaNode,ElemID)
!----------------------------------------------------------------------------------------------------------------------!
IMPLICIT NONE
!----------------------------------------------------------------------------------------------------------------------!
! FORMAL ARGUMENTS
!----------------------------------------------------------------------------------------------------------------------!
TYPE(tOctaNode),TARGET,INTENT(INOUT) :: OctaNode
INTEGER,INTENT(INOUT)                :: ElemID
!----------------------------------------------------------------------------------------------------------------------!
! LOCAL VARIABLES
!----------------------------------------------------------------------------------------------------------------------!
INTEGER :: ii
INTEGER :: jj
INTEGER :: kk
INTEGER :: iChild
REAL    :: dx
REAL    :: dy
REAL    :: dz
!----------------------------------------------------------------------------------------------------------------------!

ALLOCATE(OctaNode%Children(1:8))

dx = ABS(OctaNode%OctaCoords(2,1)-OctaNode%OctaCoords(1,1))
dy = ABS(OctaNode%OctaCoords(3,2)-OctaNode%OctaCoords(2,2))
dz = ABS(OctaNode%OctaCoords(5,3)-OctaNode%OctaCoords(1,3))

iChild = 1
ElemID = ElemID+1
DO kk=1,2
  DO jj=1,2
    DO ii=1,2
      CALL Reset_OctaNode(OctaNode%Children(iChild))
      OctaNode%Children(iChild)%Level    = OctaNode%Level+1
      OctaNode%Children(iChild)%Parent   => OctaNode
      OctaNode%Children(iChild)%isActive = .TRUE.
      OctaNode%Children(iChild)%OctaID   = ElemID
      OctaNode%Children(iChild)%OctaCoords(1,1) = OctaNode%OctaCoords(1,1) + 0.5*(ii-1)*dx
      OctaNode%Children(iChild)%OctaCoords(1,2) = OctaNode%OctaCoords(1,2) + 0.5*(jj-1)*dy
      OctaNode%Children(iChild)%OctaCoords(1,3) = OctaNode%OctaCoords(1,3) + 0.5*(kk-1)*dz
      
      OctaNode%Children(iChild)%OctaCoords(2,1) = OctaNode%OctaCoords(2,1) - 0.5*(2-ii)*dx
      OctaNode%Children(iChild)%OctaCoords(2,2) = OctaNode%OctaCoords(2,2) + 0.5*(jj-1)*dy
      OctaNode%Children(iChild)%OctaCoords(2,3) = OctaNode%OctaCoords(2,3) + 0.5*(kk-1)*dz
      
      OctaNode%Children(iChild)%OctaCoords(3,1) = OctaNode%OctaCoords(3,1) - 0.5*(2-ii)*dx
      OctaNode%Children(iChild)%OctaCoords(3,2) = OctaNode%OctaCoords(3,2) - 0.5*(2-jj)*dy
      OctaNode%Children(iChild)%OctaCoords(3,3) = OctaNode%OctaCoords(3,3) + 0.5*(kk-1)*dz
      
      OctaNode%Children(iChild)%OctaCoords(4,1) = OctaNode%OctaCoords(4,1) + 0.5*(ii-1)*dx
      OctaNode%Children(iChild)%OctaCoords(4,2) = OctaNode%OctaCoords(4,2) - 0.5*(2-jj)*dy
      OctaNode%Children(iChild)%OctaCoords(4,3) = OctaNode%OctaCoords(4,3) + 0.5*(kk-1)*dz
      
      OctaNode%Children(iChild)%OctaCoords(5,1) = OctaNode%OctaCoords(5,1) + 0.5*(ii-1)*dx
      OctaNode%Children(iChild)%OctaCoords(5,2) = OctaNode%OctaCoords(5,2) + 0.5*(jj-1)*dy
      OctaNode%Children(iChild)%OctaCoords(5,3) = OctaNode%OctaCoords(5,3) - 0.5*(2-kk)*dz
      
      OctaNode%Children(iChild)%OctaCoords(6,1) = OctaNode%OctaCoords(6,1) - 0.5*(2-ii)*dx
      OctaNode%Children(iChild)%OctaCoords(6,2) = OctaNode%OctaCoords(6,2) + 0.5*(jj-1)*dy
      OctaNode%Children(iChild)%OctaCoords(6,3) = OctaNode%OctaCoords(6,3) - 0.5*(2-kk)*dz
      
      OctaNode%Children(iChild)%OctaCoords(7,1) = OctaNode%OctaCoords(7,1) - 0.5*(2-ii)*dx
      OctaNode%Children(iChild)%OctaCoords(7,2) = OctaNode%OctaCoords(7,2) - 0.5*(2-jj)*dy
      OctaNode%Children(iChild)%OctaCoords(7,3) = OctaNode%OctaCoords(7,3) - 0.5*(2-kk)*dz
      
      OctaNode%Children(iChild)%OctaCoords(8,1) = OctaNode%OctaCoords(8,1) + 0.5*(ii-1)*dx
      OctaNode%Children(iChild)%OctaCoords(8,2) = OctaNode%OctaCoords(8,2) - 0.5*(2-jj)*dy
      OctaNode%Children(iChild)%OctaCoords(8,3) = OctaNode%OctaCoords(8,3) - 0.5*(2-kk)*dz
      iChild = iChild+1
      ElemID = ElemID+1
    END DO
  END DO
END DO

!----------------------------------------------------------------------------------------------------------------------!
END SUBROUTINE Split_OctaNode
!======================================================================================================================!
!
!
!
!======================================================================================================================!
SUBROUTINE Construct_Octree(self,OctaID,OctaCoords)
!----------------------------------------------------------------------------------------------------------------------!
IMPLICIT NONE
!----------------------------------------------------------------------------------------------------------------------!
! FORMAL ARGUMENTS
!----------------------------------------------------------------------------------------------------------------------!
CLASS(tOctree),INTENT(INOUT) :: self
INTEGER,INTENT(IN)           :: OctaID
REAL,INTENT(IN)              :: OctaCoords(1:8,1:3)
!----------------------------------------------------------------------------------------------------------------------!
! LOCAL VARIABLES
!----------------------------------------------------------------------------------------------------------------------!

NULLIFY(self%TreeNode)
IF (.NOT. ASSOCIATED(self%TreeNode)) THEN
  ALLOCATE(self%TreeNode)
END IF

CALL Reset_OctaNode(self%TreeNode)

self%TreeNode%Level = 0
self%TreeNode%OctaID = -999 !OctaID
self%TreeNode%OctaCoords(1:8,1:3) = OctaCoords

self%TreeNode%isActive = .TRUE.

NULLIFY(self%TreeNode%Parent)
NULLIFY(self%TreeNode%Children)

!----------------------------------------------------------------------------------------------------------------------!
END SUBROUTINE Construct_Octree
!======================================================================================================================!
!
!
!
!======================================================================================================================!
SUBROUTINE Construct_OctreeForest(self,nTrees,ElemsNodesArray)
!----------------------------------------------------------------------------------------------------------------------!
IMPLICIT NONE
!----------------------------------------------------------------------------------------------------------------------!
! FORMAL ARGUMENTS
!----------------------------------------------------------------------------------------------------------------------!
CLASS(tOctreesForest),INTENT(INOUT) :: self
INTEGER,INTENT(IN)                  :: nTrees
REAL,INTENT(IN)                     :: ElemsNodesArray(1:8,1:3,1:nTrees)
!----------------------------------------------------------------------------------------------------------------------!
! LOCAL VARIABLES
!----------------------------------------------------------------------------------------------------------------------!
INTEGER :: iTree
!----------------------------------------------------------------------------------------------------------------------!

ALLOCATE(self%Trees(1:nTrees))

DO iTree=1,nTrees
  CALL self%Trees(iTree)%Construct(iTree,ElemsNodesArray(1:8,1:3,iTree))
END DO

!----------------------------------------------------------------------------------------------------------------------!
END SUBROUTINE Construct_OctreeForest
!======================================================================================================================!
!
!
!
!======================================================================================================================!
SUBROUTINE Destruct_OctreeForest(self)
!----------------------------------------------------------------------------------------------------------------------!
IMPLICIT NONE
!----------------------------------------------------------------------------------------------------------------------!
! FORMAL ARGUMENTS
!----------------------------------------------------------------------------------------------------------------------!
CLASS(tOctreesForest),INTENT(INOUT) :: self
!----------------------------------------------------------------------------------------------------------------------!
! LOCAL VARIABLES
!----------------------------------------------------------------------------------------------------------------------!
INTEGER :: iTree
INTEGER :: nTrees
!----------------------------------------------------------------------------------------------------------------------!
CHARACTER(LEN=256) :: ErrorMessage
CHARACTER(LEN=256) :: FunctionName = "Destruct_OctreeForest"
!----------------------------------------------------------------------------------------------------------------------!

IF (.NOT. ALLOCATED(self%Trees)) THEN
  ErrorMessage = "Forest can not be destroyed. Forest has not been allocated."
  CALL PrintError(__STAMP__,ErrorMessage,ModuleName,FunctionName)
END IF

nTrees = SIZE(self%Trees(:))
DO iTree=1,nTrees
  CALL Clean_OctaNode(self%Trees(iTree)%TreeNode)
END DO

DEALLOCATE(self%Trees)

!----------------------------------------------------------------------------------------------------------------------!
END SUBROUTINE Destruct_OctreeForest
!======================================================================================================================!
!
!
!
!======================================================================================================================!
SUBROUTINE Print_OctreeForest(self)
!----------------------------------------------------------------------------------------------------------------------!
IMPLICIT NONE
!----------------------------------------------------------------------------------------------------------------------!
! FORMAL ARGUMENTS
!----------------------------------------------------------------------------------------------------------------------!
CLASS(tOctreesForest),INTENT(INOUT) :: self
!----------------------------------------------------------------------------------------------------------------------!
! LOCAL VARIABLES
!----------------------------------------------------------------------------------------------------------------------!
INTEGER            :: iTree
INTEGER            :: nTrees
CHARACTER(LEN=256) :: ErrorMessage
CHARACTER(LEN=256) :: FunctionName = "Print_OctreeForest"
!----------------------------------------------------------------------------------------------------------------------!

IF (.NOT. ALLOCATED(self%Trees)) THEN
  ErrorMessage = "Forest can not be printed. Forest has not been allocated."
  CALL PrintError(__STAMP__,ErrorMessage,ModuleName,FunctionName)
END IF

nTrees = SIZE(self%Trees(:))
DO iTree=1,nTrees
  CALL Print_Octree(self%Trees(iTree)%TreeNode)
  WRITE(UNIT_SCREEN,*)
END DO

!----------------------------------------------------------------------------------------------------------------------!
END SUBROUTINE Print_OctreeForest
!======================================================================================================================!
!
!
!
!======================================================================================================================!
SUBROUTINE ForestSize_OctreeForest(self,ForestSize)
!----------------------------------------------------------------------------------------------------------------------!
IMPLICIT NONE
!----------------------------------------------------------------------------------------------------------------------!
! FORMAL ARGUMENTS
!----------------------------------------------------------------------------------------------------------------------!
CLASS(tOctreesForest),INTENT(INOUT) :: self
INTEGER,INTENT(OUT)                 :: ForestSize
!----------------------------------------------------------------------------------------------------------------------!
! LOCAL VARIABLES
!----------------------------------------------------------------------------------------------------------------------!
INTEGER :: iTree
INTEGER :: nTrees
!----------------------------------------------------------------------------------------------------------------------!

nTrees = SIZE(self%Trees(:))

ForestSize = 0
DO iTree=1,nTrees
  CALL GetOctreeSize(self%Trees(iTree)%TreeNode,ForestSize)
END DO

!----------------------------------------------------------------------------------------------------------------------!
END SUBROUTINE ForestSize_OctreeForest
!======================================================================================================================!
!
!
!
!======================================================================================================================!
RECURSIVE SUBROUTINE GetOctreeSize(OctaNode,TreeSize)
!----------------------------------------------------------------------------------------------------------------------!
IMPLICIT NONE
!----------------------------------------------------------------------------------------------------------------------!
! FORMAL ARGUMENTS
!----------------------------------------------------------------------------------------------------------------------!
TYPE(tOctaNode),INTENT(INOUT) :: OctaNode
INTEGER,INTENT(INOUT)         :: TreeSize
!----------------------------------------------------------------------------------------------------------------------!
! LOCAL VARIABLES
!----------------------------------------------------------------------------------------------------------------------!
INTEGER :: iChild
!----------------------------------------------------------------------------------------------------------------------!
LOGICAL :: isActive
!----------------------------------------------------------------------------------------------------------------------!

IF (.NOT. ASSOCIATED(OctaNode%Children)) THEN
  CALL GetData_OctaNode(OctaNode=OctaNode,isActive=isActive)
  IF (isActive .EQV. .TRUE.) THEN
    TreeSize = TreeSize+1
  END IF
ELSE
  DO iChild=1,8
    CALL GetOctreeSize(OctaNode%Children(iChild),TreeSize)
  END DO
END IF

!----------------------------------------------------------------------------------------------------------------------!
END SUBROUTINE GetOctreeSize
!======================================================================================================================!
!
!
!
!----------------------------------------------------------------------------------------------------------------------!
END MODULE MOD_Octrees
!======================================================================================================================!
