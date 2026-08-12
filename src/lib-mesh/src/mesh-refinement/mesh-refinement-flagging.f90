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
MODULE MOD_MeshRefinementFlagging
!----------------------------------------------------------------------------------------------------------------------!
USE MOD_GLOBAL_vars
!----------------------------------------------------------------------------------------------------------------------!
IMPLICIT NONE
!----------------------------------------------------------------------------------------------------------------------!
PRIVATE
!----------------------------------------------------------------------------------------------------------------------!
INTERFACE InitializeElementsToRefine
  MODULE PROCEDURE InitializeElementsToRefine
END INTERFACE

INTERFACE FlagElementsForBalancing
  MODULE PROCEDURE FlagElementsForBalancing
END INTERFACE

INTERFACE FlagAllElements
  MODULE PROCEDURE FlagAllElements
END INTERFACE

INTERFACE FlagElementsInsideStandardBox
  MODULE PROCEDURE FlagElementsInsideStandardBox
END INTERFACE

INTERFACE FlagElementsInsideAdaptiveBox
  MODULE PROCEDURE FlagElementsInsideAdaptiveBox
END INTERFACE

INTERFACE FlagElementsWithPoints
  MODULE PROCEDURE FlagElementsWithPoints
END INTERFACE

INTERFACE FlagElementsOverlapFacets
  MODULE PROCEDURE FlagElementsOverlapFacets
END INTERFACE

INTERFACE FlagElementsAroundGeometryAndInsideBox
  MODULE PROCEDURE FlagElementsAroundGeometryAndInsideBox
END INTERFACE
!----------------------------------------------------------------------------------------------------------------------!
PUBLIC :: InitializeElementsToRefine
PUBLIC :: FlagElementsForBalancing
PUBLIC :: FlagAllElements
PUBLIC :: FlagElementsInsideStandardBox
PUBLIC :: FlagElementsInsideAdaptiveBox
PUBLIC :: FlagElementsWithPoints
PUBLIC :: FlagElementsOverlapFacets
PUBLIC :: FlagElementsAroundGeometryAndInsideBox
!----------------------------------------------------------------------------------------------------------------------!
CHARACTER(LEN=256) :: ModuleName = "MeshRefinementFlagging"
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
SUBROUTINE InitializeElementsToRefine()
!----------------------------------------------------------------------------------------------------------------------!
USE MOD_MeshMain_vars,ONLY: ElemList
USE MOD_MeshMain_vars,ONLY: MeshData_ElementsToRefineFlag
!----------------------------------------------------------------------------------------------------------------------!
USE MOD_MeshMainMethods,ONLY: CountElems
!----------------------------------------------------------------------------------------------------------------------!
IMPLICIT NONE
!----------------------------------------------------------------------------------------------------------------------!
! FORMAL ARGUMENTS
!----------------------------------------------------------------------------------------------------------------------!
INTEGER :: nElems
!----------------------------------------------------------------------------------------------------------------------!
! LOCAL VARIABLES
!----------------------------------------------------------------------------------------------------------------------!

! Compute nElems
CALL CountElems(ElemList,nElems)

IF (ALLOCATED(MeshData_ElementsToRefineFlag)) THEN
  DEALLOCATE(MeshData_ElementsToRefineFlag)
END IF
ALLOCATE(MeshData_ElementsToRefineFlag(1:nElems))

MeshData_ElementsToRefineFlag(1:nElems) = .FALSE.

!----------------------------------------------------------------------------------------------------------------------!
END SUBROUTINE InitializeElementsToRefine
!======================================================================================================================!
!
!
!
!======================================================================================================================!
SUBROUTINE FlagElementsForBalancing(Level)
!----------------------------------------------------------------------------------------------------------------------!
USE MOD_MeshMain_vars,ONLY: MeshData_ElementsToLevel
USE MOD_MeshMain_vars,ONLY: MeshData_ElementsToRefineFlag
!----------------------------------------------------------------------------------------------------------------------!
USE MOD_MeshMain_vars,ONLY: MeshData_MasterSlavesToElements_Edge2Edge
USE MOD_MeshMain_vars,ONLY: MeshData_MasterSlavesToElements_Tri4Tri
USE MOD_MeshMain_vars,ONLY: MeshData_MasterSlavesToElements_Quad2Quad
USE MOD_MeshMain_vars,ONLY: MeshData_MasterSlavesToElements_Quad4Quad
!----------------------------------------------------------------------------------------------------------------------!
IMPLICIT NONE
!----------------------------------------------------------------------------------------------------------------------!
! FORMAL ARGUMENTS
!----------------------------------------------------------------------------------------------------------------------!
INTEGER,INTENT(IN) :: Level
!----------------------------------------------------------------------------------------------------------------------!
! LOCAL VARIABLES
!----------------------------------------------------------------------------------------------------------------------!
INTEGER :: iLevel
INTEGER :: iTri4Tri
INTEGER :: nTri4Tri
INTEGER :: iEdge2Edge
INTEGER :: nEdge2Edge
INTEGER :: iQuad2Quad
INTEGER :: nQuad2Quad
INTEGER :: iQuad4Quad
INTEGER :: nQuad4Quad
INTEGER :: iElem_Tri4Tri(1:5)
INTEGER :: iElem_Edge2Edge(1:3)
INTEGER :: iElem_Quad2Quad(1:3)
INTEGER :: iElem_Quad4Quad(1:5)
LOGICAL :: FlagNeighbors
!----------------------------------------------------------------------------------------------------------------------!

IF (.NOT. ALLOCATED(MeshData_MasterSlavesToElements_Edge2Edge)) THEN
  nEdge2Edge = 0
ELSE
  nEdge2Edge = SIZE(MeshData_MasterSlavesToElements_Edge2Edge,2)
END IF

IF (.NOT. ALLOCATED(MeshData_MasterSlavesToElements_Tri4Tri)) THEN
  nTri4Tri = 0
ELSE
  nTri4Tri = SIZE(MeshData_MasterSlavesToElements_Tri4Tri,2)
END IF

IF (.NOT. ALLOCATED(MeshData_MasterSlavesToElements_Quad2Quad)) THEN
  nQuad2Quad = 0
ELSE
  nQuad2Quad = SIZE(MeshData_MasterSlavesToElements_Quad2Quad,2)
END IF

IF (.NOT. ALLOCATED(MeshData_MasterSlavesToElements_Quad4Quad)) THEN
  nQuad4Quad = 0
ELSE
  nQuad4Quad = SIZE(MeshData_MasterSlavesToElements_Quad4Quad,2)
END IF

IF (nEdge2Edge .GT. 0) THEN
  IF (Level .GT. 1) THEN
    DO iLevel=Level-1,1,-1
      DO iEdge2Edge=1,nEdge2Edge
        iElem_Edge2Edge(1:3) = MeshData_MasterSlavesToElements_Edge2Edge(1:3,iEdge2Edge)
        FlagNeighbors = .FALSE.
        FlagNeighbors = ((MeshData_ElementsToRefineFlag(iElem_Edge2Edge(2)) .EQV. .TRUE.) .OR. &
                         (MeshData_ElementsToRefineFlag(iElem_Edge2Edge(3)) .EQV. .TRUE.)) .AND. &
                         (MeshData_ElementsToLevel(iElem_Edge2Edge(1)) .EQ. iLevel-1)
        IF (FlagNeighbors .EQV. .TRUE.) THEN
          MeshData_ElementsToRefineFlag(iElem_Edge2Edge(1)) = .TRUE.
        END IF
      END DO
    END DO
  END IF
END IF

IF (nTri4Tri .GT. 0) THEN
  IF (Level .GT. 1) THEN
    DO iLevel=Level-1,1,-1
      DO iTri4Tri=1,nTri4Tri
        iElem_Tri4Tri(1:5) = MeshData_MasterSlavesToElements_Tri4Tri(1:5,iTri4Tri)
        FlagNeighbors = .FALSE.
        FlagNeighbors = ((MeshData_ElementsToRefineFlag(iElem_Tri4Tri(2)) .EQV. .TRUE.) .OR. &
                         (MeshData_ElementsToRefineFlag(iElem_Tri4Tri(3)) .EQV. .TRUE.) .OR. &
                         (MeshData_ElementsToRefineFlag(iElem_Tri4Tri(4)) .EQV. .TRUE.) .OR. &
                         (MeshData_ElementsToRefineFlag(iElem_Tri4Tri(5)) .EQV. .TRUE.)) .AND. &
                         (MeshData_ElementsToLevel(iElem_Tri4Tri(1)) .EQ. iLevel-1)
        IF (FlagNeighbors .EQV. .TRUE.) THEN
          MeshData_ElementsToRefineFlag(iElem_Tri4Tri(1)) = .TRUE.
        END IF
      END DO
    END DO
  END IF
END IF

IF (nQuad2Quad .GT. 0) THEN
  IF (Level .GT. 1) THEN
    DO iLevel=Level-1,1,-1
      DO iQuad2Quad=1,nQuad2Quad
        iElem_Quad2Quad(1:3) = MeshData_MasterSlavesToElements_Quad2Quad(1:3,iQuad2Quad)
        FlagNeighbors = .FALSE.
        FlagNeighbors = ((MeshData_ElementsToRefineFlag(iElem_Quad2Quad(2)) .EQV. .TRUE.) .OR. &
                         (MeshData_ElementsToRefineFlag(iElem_Quad2Quad(3)) .EQV. .TRUE.)) .AND. &
                         (MeshData_ElementsToLevel(iElem_Quad2Quad(1)) .EQ. iLevel-1)
        IF (FlagNeighbors .EQV. .TRUE.) THEN
          MeshData_ElementsToRefineFlag(iElem_Quad2Quad(1)) = .TRUE.
        END IF
      END DO
    END DO
  END IF
END IF

IF (nQuad4Quad .GT. 0) THEN
  IF (Level .GT. 1) THEN
    DO iLevel=Level-1,1,-1
      DO iQuad4Quad=1,nQuad4Quad
        iElem_Quad4Quad(1:5) = MeshData_MasterSlavesToElements_Quad4Quad(1:5,iQuad4Quad)
        FlagNeighbors = .FALSE.
        FlagNeighbors = ((MeshData_ElementsToRefineFlag(iElem_Quad4Quad(2)) .EQV. .TRUE.) .OR. &
                         (MeshData_ElementsToRefineFlag(iElem_Quad4Quad(3)) .EQV. .TRUE.) .OR. &
                         (MeshData_ElementsToRefineFlag(iElem_Quad4Quad(4)) .EQV. .TRUE.) .OR. &
                         (MeshData_ElementsToRefineFlag(iElem_Quad4Quad(5)) .EQV. .TRUE.)) .AND. &
                         (MeshData_ElementsToLevel(iElem_Quad4Quad(1)) .EQ. iLevel-1)
        IF (FlagNeighbors .EQV. .TRUE.) THEN
          MeshData_ElementsToRefineFlag(iElem_Quad4Quad(1)) = .TRUE.
        END IF
      END DO
    END DO
  END IF
END IF

!----------------------------------------------------------------------------------------------------------------------!
END SUBROUTINE FlagElementsForBalancing
!======================================================================================================================!
!
!
!
!======================================================================================================================!
SUBROUTINE FlagAllElements(Level)
!----------------------------------------------------------------------------------------------------------------------!
USE MOD_MeshMain_vars,ONLY: tElem
USE MOD_MeshMain_vars,ONLY: ElemList
USE MOD_MeshMain_vars,ONLY: MeshData_ElementsToRefineFlag
!----------------------------------------------------------------------------------------------------------------------!
IMPLICIT NONE
!----------------------------------------------------------------------------------------------------------------------!
! FORMAL ARGUMENTS
!----------------------------------------------------------------------------------------------------------------------!
INTEGER,INTENT(IN) :: Level
!----------------------------------------------------------------------------------------------------------------------!
! LOCAL VARIABLES
!----------------------------------------------------------------------------------------------------------------------!
INTEGER :: ElemID
!----------------------------------------------------------------------------------------------------------------------!
TYPE(tElem),POINTER :: aElem
!----------------------------------------------------------------------------------------------------------------------!

aElem => ElemList%FirstElem
DO WHILE (ASSOCIATED(aElem))
  ElemID = aElem%ElemID
  MeshData_ElementsToRefineFlag(ElemID) = .TRUE.
  aElem => aElem%NextElem
END DO

!----------------------------------------------------------------------------------------------------------------------!
END SUBROUTINE FlagAllElements
!======================================================================================================================!
!
!
!
!======================================================================================================================!
SUBROUTINE FlagElementsInsideStandardBox(Level)
!----------------------------------------------------------------------------------------------------------------------!
IMPLICIT NONE
!----------------------------------------------------------------------------------------------------------------------!
! FORMAL ARGUMENTS
!----------------------------------------------------------------------------------------------------------------------!
INTEGER,INTENT(IN) :: Level
!----------------------------------------------------------------------------------------------------------------------!
! LOCAL VARIABLES
!----------------------------------------------------------------------------------------------------------------------!
CHARACTER(LEN=256) :: ErrorMessage
CHARACTER(LEN=256) :: FunctionName = "FlagElementsInsideStandardBox"
!----------------------------------------------------------------------------------------------------------------------!

SELECT CASE(PP_nDims)
  CASE(2)
    CALL FlagElementsInsideStandardBox2D(Level)
  CASE(3)
    CALL FlagElementsInsideStandardBox3D(Level)
  CASE DEFAULT
    ErrorMessage = "Invalid value for MeshDimension"
    CALL PrintError(__STAMP__,ErrorMessage,ModuleName,FunctionName)
END SELECT

!----------------------------------------------------------------------------------------------------------------------!
END SUBROUTINE FlagElementsInsideStandardBox
!======================================================================================================================!
!
!
!
!======================================================================================================================!
SUBROUTINE FlagElementsInsideStandardBox2D(Level)
!----------------------------------------------------------------------------------------------------------------------!
USE MOD_MeshMain_vars,ONLY: tElem
USE MOD_MeshMain_vars,ONLY: ElemList
USE MOD_MeshMain_vars,ONLY: MeshData_ElementsToRefineFlag
!----------------------------------------------------------------------------------------------------------------------!
USE MOD_MeshRefinement_vars,  ONLY: ParametersMeshRefinement
USE MOD_ComputationalGeometry,ONLY: PointIsInsideCube2D
!----------------------------------------------------------------------------------------------------------------------!
IMPLICIT NONE
!----------------------------------------------------------------------------------------------------------------------!
! FORMAL ARGUMENTS
!----------------------------------------------------------------------------------------------------------------------!
INTEGER,INTENT(IN) :: Level
!----------------------------------------------------------------------------------------------------------------------!
! LOCAL VARIABLES
!----------------------------------------------------------------------------------------------------------------------!
INTEGER :: iRefinedBox
INTEGER :: nRefinedBox
!----------------------------------------------------------------------------------------------------------------------!
INTEGER :: iNode
INTEGER :: ElemID
REAL    :: RefinedBox2D(1:4,1:2)
REAL    :: BaryCenterCoords(1:2)
!----------------------------------------------------------------------------------------------------------------------!
TYPE(tElem),POINTER :: aElem
!----------------------------------------------------------------------------------------------------------------------!

nRefinedBox = ParametersMeshRefinement%nRefinedBox
DO iRefinedBox=1,nRefinedBox
  IF (Level .GT. ParametersMeshRefinement%MaxBoxRefinementLevel(iRefinedBox)) THEN
    CYCLE
  END IF
  RefinedBox2D(1:4,1:2) = ParametersMeshRefinement%RefinedBoxCorner(iRefinedBox,1:4,1:2)
  aElem => ElemList%FirstElem
  DO WHILE (ASSOCIATED(aElem))
    ElemID = aElem%ElemID
    BaryCenterCoords(1:2) = 0.0
    DO iNode=1,aElem%nNodes
      BaryCenterCoords(1:2) = BaryCenterCoords(1:2) + aElem%Nodes(iNode)%Node%Coords(1:2)
    END DO
    BaryCenterCoords(1:2) = (1.0/REAL(aElem%nNodes))*BaryCenterCoords(1:2)
    IF (PointIsInsideCube2D(BaryCenterCoords,RefinedBox2D) .EQV. .TRUE.) THEN
      MeshData_ElementsToRefineFlag(ElemID) = .TRUE.
    END IF
    aElem => aElem%NextElem
  END DO
END DO

!----------------------------------------------------------------------------------------------------------------------!
END SUBROUTINE FlagElementsInsideStandardBox2D
!======================================================================================================================!
!
!
!
!======================================================================================================================!
SUBROUTINE FlagElementsInsideStandardBox3D(Level)
!----------------------------------------------------------------------------------------------------------------------!
USE MOD_MeshMain_vars,ONLY: tElem
USE MOD_MeshMain_vars,ONLY: ElemList
USE MOD_MeshMain_vars,ONLY: MeshData_ElementsToRefineFlag
!----------------------------------------------------------------------------------------------------------------------!
USE MOD_MeshRefinement_vars,  ONLY: ParametersMeshRefinement
USE MOD_ComputationalGeometry,ONLY: PointIsInsideCube3D
!----------------------------------------------------------------------------------------------------------------------!
IMPLICIT NONE
!----------------------------------------------------------------------------------------------------------------------!
! FORMAL ARGUMENTS
!----------------------------------------------------------------------------------------------------------------------!
INTEGER,INTENT(IN) :: Level
!----------------------------------------------------------------------------------------------------------------------!
! LOCAL VARIABLES
!----------------------------------------------------------------------------------------------------------------------!
INTEGER :: iRefinedBox
INTEGER :: nRefinedBox
!----------------------------------------------------------------------------------------------------------------------!
INTEGER :: iNode
INTEGER :: ElemID
REAL    :: RefinedBox3D(1:8,1:3)
REAL    :: BaryCenterCoords(1:3)
!----------------------------------------------------------------------------------------------------------------------!
TYPE(tElem),POINTER :: aElem
!----------------------------------------------------------------------------------------------------------------------!

nRefinedBox = ParametersMeshRefinement%nRefinedBox
DO iRefinedBox=1,nRefinedBox
  IF (Level .GT. ParametersMeshRefinement%MaxBoxRefinementLevel(iRefinedBox)) THEN
    CYCLE
  END IF
  RefinedBox3D(1:8,1:3) = ParametersMeshRefinement%RefinedBoxCorner(iRefinedBox,1:8,1:3)
  aElem => ElemList%FirstElem
  DO WHILE (ASSOCIATED(aElem))
    ElemID = aElem%ElemID
    BaryCenterCoords(1:3) = 0.0
    DO iNode=1,aElem%nNodes
      BaryCenterCoords(1:3) = BaryCenterCoords(1:3) + aElem%Nodes(iNode)%Node%Coords(1:3)
    END DO
    BaryCenterCoords(1:3) = (1.0/REAL(aElem%nNodes))*BaryCenterCoords(1:3)
    IF (PointIsInsideCube3D(BaryCenterCoords,RefinedBox3D) .EQV. .TRUE.) THEN
      MeshData_ElementsToRefineFlag(ElemID) = .TRUE.
    END IF
    aElem => aElem%NextElem
  END DO
END DO

!----------------------------------------------------------------------------------------------------------------------!
END SUBROUTINE FlagElementsInsideStandardBox3D
!======================================================================================================================!
!
!
!
!======================================================================================================================!
SUBROUTINE FlagElementsInsideAdaptiveBox(Level)
!----------------------------------------------------------------------------------------------------------------------!
IMPLICIT NONE
!----------------------------------------------------------------------------------------------------------------------!
! FORMAL ARGUMENTS
!----------------------------------------------------------------------------------------------------------------------!
INTEGER,INTENT(IN) :: Level
!----------------------------------------------------------------------------------------------------------------------!
! LOCAL VARIABLES
!----------------------------------------------------------------------------------------------------------------------!
CHARACTER(LEN=256) :: ErrorMessage
CHARACTER(LEN=256) :: FunctionName = "FlagElementsInsideAdaptiveBox"
!----------------------------------------------------------------------------------------------------------------------!

SELECT CASE(PP_nDims)
  CASE(2)
    CALL FlagElementsInsideAdaptiveBox2D(Level)
  CASE(3)
    CALL FlagElementsInsideAdaptiveBox3D(Level)
  CASE DEFAULT
    ErrorMessage = "Invalid value for MeshDimension"
    CALL PrintError(__STAMP__,ErrorMessage,ModuleName,FunctionName)
END SELECT

!----------------------------------------------------------------------------------------------------------------------!
END SUBROUTINE FlagElementsInsideAdaptiveBox
!======================================================================================================================!
!
!
!
!======================================================================================================================!
SUBROUTINE FlagElementsInsideAdaptiveBox2D(Level)
!----------------------------------------------------------------------------------------------------------------------!
USE MOD_MeshMain_vars,ONLY: tElem
USE MOD_MeshMain_vars,ONLY: ElemList
USE MOD_MeshMain_vars,ONLY: MeshData_ElementsToRefineFlag
!----------------------------------------------------------------------------------------------------------------------!
USE MOD_MeshRefinement_vars,ONLY: ParametersMeshRefinement
!----------------------------------------------------------------------------------------------------------------------!
USE MOD_ComputationalGeometry,ONLY: PointIsInsideCube2D
!----------------------------------------------------------------------------------------------------------------------!
IMPLICIT NONE
!----------------------------------------------------------------------------------------------------------------------!
! FORMAL ARGUMENTS
!----------------------------------------------------------------------------------------------------------------------!
INTEGER,INTENT(IN) :: Level
!----------------------------------------------------------------------------------------------------------------------!
! LOCAL VARIABLES
!----------------------------------------------------------------------------------------------------------------------!
INTEGER :: iNode
INTEGER :: ElemID
!----------------------------------------------------------------------------------------------------------------------!
INTEGER :: iRefinedBox
INTEGER :: nRefinedBox
!----------------------------------------------------------------------------------------------------------------------!
REAL :: Delta
REAL :: AlphaX
REAL :: AlphaY
REAL :: dx
REAL :: dy
REAL :: AdaptiveBox2D(1:4,1:2)
REAL :: RefinedBox2D(1:4,1:2)
!----------------------------------------------------------------------------------------------------------------------!
TYPE(tElem),POINTER :: aElem
!----------------------------------------------------------------------------------------------------------------------!

nRefinedBox = ParametersMeshRefinement%nRefinedBox
DO iRefinedBox=1,nRefinedBox
  IF (Level .GT. ParametersMeshRefinement%MaxBoxRefinementLevel(iRefinedBox)) THEN
    CYCLE
  END IF

  RefinedBox2D(1:4,1:2) = ParametersMeshRefinement%RefinedBoxCorner(iRefinedBox,1:4,1:2)

  Delta  = ParametersMeshRefinement%RegionEnlargementFactor
  AlphaX = 2**(ParametersMeshRefinement%MaxBoxRefinementLevel(iRefinedBox)-Level+1)
  AlphaY = 2**(ParametersMeshRefinement%MaxBoxRefinementLevel(iRefinedBox)-Level+1)

  dx = Delta
  dy = Delta

  ! Face 4 (X-)
  AdaptiveBox2D(1,1) = RefinedBox2D(1,1) - 0.5*(AlphaX-1.0)*dx
  AdaptiveBox2D(4,1) = RefinedBox2D(4,1) - 0.5*(AlphaX-1.0)*dx
  ! Face 2 (X+)
  AdaptiveBox2D(2,1) = RefinedBox2D(2,1) + 0.5*(AlphaX-1.0)*dx
  AdaptiveBox2D(3,1) = RefinedBox2D(3,1) + 0.5*(AlphaX-1.0)*dx
  ! Face 1 (Y-)
  AdaptiveBox2D(1,2) = RefinedBox2D(1,2) - 0.5*(AlphaY-1.0)*dy
  AdaptiveBox2D(2,2) = RefinedBox2D(2,2) - 0.5*(AlphaY-1.0)*dy
  ! Face 2 (Y+)
  AdaptiveBox2D(3,2) = RefinedBox2D(3,2) + 0.5*(AlphaY-1.0)*dy
  AdaptiveBox2D(4,2) = RefinedBox2D(4,2) + 0.5*(AlphaY-1.0)*dy

  aElem => ElemList%FirstElem
  DO WHILE (ASSOCIATED(aElem))
    ElemID = aElem%ElemID
    DO iNode=1,aElem%nNodes
      IF (PointIsInsideCube2D(aElem%Nodes(iNode)%Node%Coords(1:2),AdaptiveBox2D) .EQV. .TRUE.) THEN
        MeshData_ElementsToRefineFlag(ElemID) = .TRUE.
        EXIT
      END IF
    END DO
    aElem => aElem%NextElem
  END DO
END DO

!----------------------------------------------------------------------------------------------------------------------!
END SUBROUTINE FlagElementsInsideAdaptiveBox2D
!======================================================================================================================!
!
!
!
!======================================================================================================================!
SUBROUTINE FlagElementsInsideAdaptiveBox3D(Level)
!----------------------------------------------------------------------------------------------------------------------!
USE MOD_MeshMain_vars,ONLY: tElem
USE MOD_MeshMain_vars,ONLY: ElemList
USE MOD_MeshMain_vars,ONLY: MeshData_ElementsToRefineFlag
!----------------------------------------------------------------------------------------------------------------------!
USE MOD_MeshRefinement_vars,  ONLY: ParametersMeshRefinement
!----------------------------------------------------------------------------------------------------------------------!
USE MOD_ComputationalGeometry,ONLY: PointIsInsideCube3D
!----------------------------------------------------------------------------------------------------------------------!
IMPLICIT NONE
!----------------------------------------------------------------------------------------------------------------------!
! FORMAL ARGUMENTS
!----------------------------------------------------------------------------------------------------------------------!
INTEGER,INTENT(IN) :: Level
!----------------------------------------------------------------------------------------------------------------------!
! LOCAL VARIABLES
!----------------------------------------------------------------------------------------------------------------------!
INTEGER :: iNode
INTEGER :: ElemID
!----------------------------------------------------------------------------------------------------------------------!
INTEGER :: iRefinedBox
INTEGER :: nRefinedBox
!----------------------------------------------------------------------------------------------------------------------!
REAL :: Delta
REAL :: AlphaX
REAL :: AlphaY
REAL :: AlphaZ
REAL :: dx
REAL :: dy
REAL :: dz
REAL :: AdaptiveBox3D(1:8,1:3)
REAL :: RefinedBox3D(1:8,1:3)
!----------------------------------------------------------------------------------------------------------------------!
TYPE(tElem),POINTER :: aElem
!----------------------------------------------------------------------------------------------------------------------!

nRefinedBox = ParametersMeshRefinement%nRefinedBox
DO iRefinedBox=1,nRefinedBox
  IF (Level .GT. ParametersMeshRefinement%MaxBoxRefinementLevel(iRefinedBox)) THEN
    CYCLE
  END IF

  RefinedBox3D(1:8,1:3) = ParametersMeshRefinement%RefinedBoxCorner(iRefinedBox,1:8,1:3)

  Delta  = ParametersMeshRefinement%RegionEnlargementFactor
  AlphaX = 2**(ParametersMeshRefinement%MaxBoxRefinementLevel(iRefinedBox)-Level+1)
  AlphaY = 2**(ParametersMeshRefinement%MaxBoxRefinementLevel(iRefinedBox)-Level+1)
  AlphaZ = 2**(ParametersMeshRefinement%MaxBoxRefinementLevel(iRefinedBox)-Level+1)

  dx = Delta
  dy = Delta
  dz = Delta

  ! Face 5 (X-)
  AdaptiveBox3D(1,1) = RefinedBox3D(1,1) - 0.5*(AlphaX-1.0)*dx
  AdaptiveBox3D(4,1) = RefinedBox3D(4,1) - 0.5*(AlphaX-1.0)*dx
  AdaptiveBox3D(5,1) = RefinedBox3D(5,1) - 0.5*(AlphaX-1.0)*dx
  AdaptiveBox3D(8,1) = RefinedBox3D(8,1) - 0.5*(AlphaX-1.0)*dx
  ! Face 3 (X+)
  AdaptiveBox3D(2,1) = RefinedBox3D(2,1) + 0.5*(AlphaX-1.0)*dx
  AdaptiveBox3D(3,1) = RefinedBox3D(3,1) + 0.5*(AlphaX-1.0)*dx
  AdaptiveBox3D(6,1) = RefinedBox3D(6,1) + 0.5*(AlphaX-1.0)*dx
  AdaptiveBox3D(7,1) = RefinedBox3D(7,1) + 0.5*(AlphaX-1.0)*dx
  ! Face 2 (Y-)
  AdaptiveBox3D(1,2) = RefinedBox3D(1,2) - 0.5*(AlphaY-1.0)*dy
  AdaptiveBox3D(2,2) = RefinedBox3D(2,2) - 0.5*(AlphaY-1.0)*dy
  AdaptiveBox3D(5,2) = RefinedBox3D(5,2) - 0.5*(AlphaY-1.0)*dy
  AdaptiveBox3D(6,2) = RefinedBox3D(6,2) - 0.5*(AlphaY-1.0)*dy
  ! Face 4 (Y+)
  AdaptiveBox3D(3,2) = RefinedBox3D(3,2) + 0.5*(AlphaY-1.0)*dy
  AdaptiveBox3D(4,2) = RefinedBox3D(4,2) + 0.5*(AlphaY-1.0)*dy
  AdaptiveBox3D(7,2) = RefinedBox3D(7,2) + 0.5*(AlphaY-1.0)*dy
  AdaptiveBox3D(8,2) = RefinedBox3D(8,2) + 0.5*(AlphaY-1.0)*dy
  ! Face 1 (Z-)
  AdaptiveBox3D(1,3) = RefinedBox3D(1,3) - 0.5*(AlphaZ-1.0)*dz
  AdaptiveBox3D(2,3) = RefinedBox3D(2,3) - 0.5*(AlphaZ-1.0)*dz
  AdaptiveBox3D(3,3) = RefinedBox3D(3,3) - 0.5*(AlphaZ-1.0)*dz
  AdaptiveBox3D(4,3) = RefinedBox3D(4,3) - 0.5*(AlphaZ-1.0)*dz
  ! Face 6 (Z+)
  AdaptiveBox3D(5,3) = RefinedBox3D(5,3) + 0.5*(AlphaZ-1.0)*dz
  AdaptiveBox3D(6,3) = RefinedBox3D(6,3) + 0.5*(AlphaZ-1.0)*dz
  AdaptiveBox3D(7,3) = RefinedBox3D(7,3) + 0.5*(AlphaZ-1.0)*dz
  AdaptiveBox3D(8,3) = RefinedBox3D(8,3) + 0.5*(AlphaZ-1.0)*dz

  aElem => ElemList%FirstElem
  DO WHILE (ASSOCIATED(aElem))
    ElemID = aElem%ElemID
    DO iNode=1,aElem%nNodes
      IF (PointIsInsideCube3D(aElem%Nodes(iNode)%Node%Coords(1:3),AdaptiveBox3D) .EQV. .TRUE.) THEN
        MeshData_ElementsToRefineFlag(ElemID) = .TRUE.
        EXIT
      END IF
    END DO
    aElem => aElem%NextElem
  END DO
END DO

!----------------------------------------------------------------------------------------------------------------------!
END SUBROUTINE FlagElementsInsideAdaptiveBox3D
!======================================================================================================================!
!
!
!
!======================================================================================================================!
SUBROUTINE FlagElementsAroundGeometryAndInsideBox(Level)
!----------------------------------------------------------------------------------------------------------------------!
USE MOD_MeshRefinement_vars,ONLY: FlagElementsInsideBox
USE MOD_MeshRefinement_vars,ONLY: FlagElementsAroundGeometry
!----------------------------------------------------------------------------------------------------------------------!
IMPLICIT NONE
!----------------------------------------------------------------------------------------------------------------------!
! FORMAL ARGUMENTS
!----------------------------------------------------------------------------------------------------------------------!
INTEGER,INTENT(IN) :: Level
!----------------------------------------------------------------------------------------------------------------------!
! LOCAL VARIABLES
!----------------------------------------------------------------------------------------------------------------------!

CALL FlagElementsInsideBox(Level)
CALL FlagElementsAroundGeometry(Level)

!----------------------------------------------------------------------------------------------------------------------!
END SUBROUTINE FlagElementsAroundGeometryAndInsideBox
!======================================================================================================================!
!
!
!
!======================================================================================================================!
SUBROUTINE FlagElementsWithPoints(Level)
!----------------------------------------------------------------------------------------------------------------------!
IMPLICIT NONE
!----------------------------------------------------------------------------------------------------------------------!
! FORMAL ARGUMENTS
!----------------------------------------------------------------------------------------------------------------------!
INTEGER,INTENT(IN) :: Level
!----------------------------------------------------------------------------------------------------------------------!
! LOCAL VARIABLES
!----------------------------------------------------------------------------------------------------------------------!
CHARACTER(LEN=256) :: ErrorMessage
CHARACTER(LEN=256) :: FunctionName = "FlagElementsWithPoints"
!----------------------------------------------------------------------------------------------------------------------!

SELECT CASE(PP_nDims)
  CASE(2)
    CALL FlagElementsWithPoints2D(Level)
  CASE(3)
    CALL FlagElementsWithPoints3D(Level)
  CASE DEFAULT
    ErrorMessage = "Invalid value for MeshDimension"
    CALL PrintError(__STAMP__,ErrorMessage,ModuleName,FunctionName)
END SELECT

!----------------------------------------------------------------------------------------------------------------------!
END SUBROUTINE FlagElementsWithPoints
!======================================================================================================================!
!
!
!
!======================================================================================================================!
SUBROUTINE FlagElementsWithPoints2D(Level)
!----------------------------------------------------------------------------------------------------------------------!
USE MOD_MeshMain_vars,ONLY: tElem
USE MOD_MeshMain_vars,ONLY: ElemList
USE MOD_MeshMain_vars,ONLY: MeshData_ElementsToRefineFlag
!----------------------------------------------------------------------------------------------------------------------!
IMPLICIT NONE
!----------------------------------------------------------------------------------------------------------------------!
! FORMAL ARGUMENTS
!----------------------------------------------------------------------------------------------------------------------!
INTEGER,INTENT(IN) :: Level
!----------------------------------------------------------------------------------------------------------------------!
! LOCAL VARIABLES
!----------------------------------------------------------------------------------------------------------------------!
INTEGER :: ElemID
!----------------------------------------------------------------------------------------------------------------------!
TYPE(tElem),POINTER :: aElem
!----------------------------------------------------------------------------------------------------------------------!

aElem => ElemList%FirstElem
DO WHILE (ASSOCIATED(aElem))
  ElemID = aElem%ElemID
  IF (aElem%nPoints .EQ. 0) THEN
    aElem => aElem%NextElem
    CYCLE
  END IF
  IF (aElem%nPoints .GE. 1) THEN
    MeshData_ElementsToRefineFlag(ElemID) = .TRUE.
    aElem => aElem%NextElem
  END IF
END DO

!----------------------------------------------------------------------------------------------------------------------!
END SUBROUTINE FlagElementsWithPoints2D
!======================================================================================================================!
!
!
!
!======================================================================================================================!
SUBROUTINE FlagElementsWithPoints3D(Level)
!----------------------------------------------------------------------------------------------------------------------!
USE MOD_MeshMain_vars,ONLY: tElem
USE MOD_MeshMain_vars,ONLY: ElemList
USE MOD_MeshMain_vars,ONLY: MeshData_ElementsToRefineFlag
!----------------------------------------------------------------------------------------------------------------------!
IMPLICIT NONE
!----------------------------------------------------------------------------------------------------------------------!
! FORMAL ARGUMENTS
!----------------------------------------------------------------------------------------------------------------------!
INTEGER,INTENT(IN) :: Level
!----------------------------------------------------------------------------------------------------------------------!
! LOCAL VARIABLES
!----------------------------------------------------------------------------------------------------------------------!
INTEGER :: ElemID
!----------------------------------------------------------------------------------------------------------------------!
TYPE(tElem),POINTER :: aElem
!----------------------------------------------------------------------------------------------------------------------!

aElem => ElemList%FirstElem
DO WHILE (ASSOCIATED(aElem))
  ElemID = aElem%ElemID
  IF (aElem%nPoints .EQ. 0) THEN
    aElem => aElem%NextElem
    CYCLE
  END IF
  IF (aElem%nPoints .GE. 1) THEN
    MeshData_ElementsToRefineFlag(ElemID) = .TRUE.
    aElem => aElem%NextElem
  END IF
END DO

!----------------------------------------------------------------------------------------------------------------------!
END SUBROUTINE FlagElementsWithPoints3D
!======================================================================================================================!
!
!
!
!======================================================================================================================!
SUBROUTINE FlagElementsOverlapFacets(Level)
!----------------------------------------------------------------------------------------------------------------------!
IMPLICIT NONE
!----------------------------------------------------------------------------------------------------------------------!
! FORMAL ARGUMENTS
!----------------------------------------------------------------------------------------------------------------------!
INTEGER,INTENT(IN) :: Level
!----------------------------------------------------------------------------------------------------------------------!
! LOCAL VARIABLES
!----------------------------------------------------------------------------------------------------------------------!
CHARACTER(LEN=256) :: ErrorMessage
CHARACTER(LEN=256) :: FunctionName = "FlagElementsOverlapFacets"
!----------------------------------------------------------------------------------------------------------------------!

SELECT CASE(PP_nDims)
  CASE(3)
    CALL FlagElementsOverlapFacets3D(Level)
  CASE DEFAULT
    ErrorMessage = "Invalid value for MeshDimension"
    CALL PrintError(__STAMP__,ErrorMessage,ModuleName,FunctionName)
END SELECT

!----------------------------------------------------------------------------------------------------------------------!
END SUBROUTINE FlagElementsOverlapFacets
!======================================================================================================================!
!
!
!
!======================================================================================================================!
SUBROUTINE FlagElementsOverlapFacets2D(Level)
!----------------------------------------------------------------------------------------------------------------------!
USE MOD_MeshMain_vars,ONLY: tElem
USE MOD_MeshMain_vars,ONLY: ElemList
USE MOD_MeshMain_vars,ONLY: MeshData_ElementsToRefineFlag
!----------------------------------------------------------------------------------------------------------------------!
IMPLICIT NONE
!----------------------------------------------------------------------------------------------------------------------!
! FORMAL ARGUMENTS
!----------------------------------------------------------------------------------------------------------------------!
INTEGER,INTENT(IN) :: Level
!----------------------------------------------------------------------------------------------------------------------!
! LOCAL VARIABLES
!----------------------------------------------------------------------------------------------------------------------!
TYPE(tElem),POINTER :: aElem
!----------------------------------------------------------------------------------------------------------------------!

aElem => ElemList%FirstElem
DO WHILE (ASSOCIATED(aElem))
  IF (aElem%nFacets .GE. 1) THEN
    MeshData_ElementsToRefineFlag(aElem%ElemID) = .TRUE.
  END IF
  aElem => aElem%NextElem
END DO

!----------------------------------------------------------------------------------------------------------------------!
END SUBROUTINE FlagElementsOverlapFacets2D
!======================================================================================================================!
!
!
!
!======================================================================================================================!
SUBROUTINE FlagElementsOverlapFacets3D(Level)
!----------------------------------------------------------------------------------------------------------------------!
USE MOD_MeshMain_vars,ONLY: tElem
USE MOD_MeshMain_vars,ONLY: ElemList
USE MOD_MeshMain_vars,ONLY: MeshData_ElementsToRefineFlag
!----------------------------------------------------------------------------------------------------------------------!
IMPLICIT NONE
!----------------------------------------------------------------------------------------------------------------------!
! FORMAL ARGUMENTS
!----------------------------------------------------------------------------------------------------------------------!
INTEGER,INTENT(IN) :: Level
!----------------------------------------------------------------------------------------------------------------------!
! LOCAL VARIABLES
!----------------------------------------------------------------------------------------------------------------------!
TYPE(tElem),POINTER :: aElem
!----------------------------------------------------------------------------------------------------------------------!

aElem => ElemList%FirstElem
DO WHILE (ASSOCIATED(aElem))
  IF (aElem%nFacets .GE. 1) THEN
    MeshData_ElementsToRefineFlag(aElem%ElemID) = .TRUE.
  END IF
  aElem => aElem%NextElem
END DO

!----------------------------------------------------------------------------------------------------------------------!
END SUBROUTINE FlagElementsOverlapFacets3D
!======================================================================================================================!
!
!
!
!----------------------------------------------------------------------------------------------------------------------!
END MODULE MOD_MeshRefinementFlagging
!======================================================================================================================!
