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
MODULE MOD_MeshGeometryDistribution
!----------------------------------------------------------------------------------------------------------------------!
USE MOD_GLOBAL_vars
!----------------------------------------------------------------------------------------------------------------------!
IMPLICIT NONE
!----------------------------------------------------------------------------------------------------------------------!
PRIVATE
!----------------------------------------------------------------------------------------------------------------------!
INTERFACE DistributePointsInElem
  MODULE PROCEDURE DistributePointsInElem
END INTERFACE

INTERFACE DistributeSTLPointsInElemList
  MODULE PROCEDURE DistributeSTLPointsInElemList
END INTERFACE

INTERFACE DistributeSTLFacetsInElem
  MODULE PROCEDURE DistributeSTLFacetsInElem
END INTERFACE

INTERFACE DistributeSTLFacetsInElemList
  MODULE PROCEDURE DistributeSTLFacetsInElemList
END INTERFACE
!----------------------------------------------------------------------------------------------------------------------!
PUBLIC :: DistributePointsInElem
PUBLIC :: DistributeSTLPointsInElemList
PUBLIC :: DistributeSTLFacetsInElem
PUBLIC :: DistributeSTLFacetsInElemList
!----------------------------------------------------------------------------------------------------------------------!
CHARACTER(LEN=256) :: ModuleName = "MeshGeometryDistribution"
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
SUBROUTINE DistributeSTLPointsInElemList()
!----------------------------------------------------------------------------------------------------------------------!
USE MOD_MeshMain_vars,ONLY: tElem
USE MOD_MeshMain_vars,ONLY: ElemList
!----------------------------------------------------------------------------------------------------------------------!
USE MOD_GeometryMain_vars,ONLY: GeometryPoints
!----------------------------------------------------------------------------------------------------------------------!
IMPLICIT NONE
!----------------------------------------------------------------------------------------------------------------------!
! FORMAL ARGUMENTS
!----------------------------------------------------------------------------------------------------------------------!
! LOCAL VARIABLES
!----------------------------------------------------------------------------------------------------------------------!
TYPE(tElem),POINTER :: aElem
!----------------------------------------------------------------------------------------------------------------------!
INTEGER            :: nTabIn
REAL               :: CalcTimeIni
REAL               :: CalcTimeEnd
CHARACTER(LEN=256) :: ElapsedTime
!----------------------------------------------------------------------------------------------------------------------!
CHARACTER(LEN=256) :: Header
!----------------------------------------------------------------------------------------------------------------------!

nTabIn = 2
Header = "DISTRIBUTING POINTS"
CALL PrintMessage(Header,nTabIn=nTabIn)

CalcTimeIni = RunningTime()

aElem => ElemList%FirstElem
DO WHILE (ASSOCIATED(aElem))
  CALL DistributePointsInElem(aElem,GeometryPoints)
  aElem => aElem%NextElem
END DO

CalcTimeEnd = RunningTime()
CALL ComputeRuntime(CalcTimeEnd-CalcTimeIni,ElapsedTime)
Header = "Elapsed Time"
CALL PrintAnalyze(Header,ElapsedTime)

!----------------------------------------------------------------------------------------------------------------------!
END SUBROUTINE DistributeSTLPointsInElemList
!======================================================================================================================!
!
!
!
!======================================================================================================================!
SUBROUTINE DistributePointsInElem(aElem,Points)
!----------------------------------------------------------------------------------------------------------------------!
USE MOD_MeshMain_vars,ONLY: tElem
!----------------------------------------------------------------------------------------------------------------------!
USE MOD_GeometryMain_vars,ONLY: tPointCoords
!----------------------------------------------------------------------------------------------------------------------!
USE MOD_ComputationalGeometry,ONLY: BoundingBox
USE MOD_ComputationalGeometry,ONLY: PointIsInsideCube2D
USE MOD_ComputationalGeometry,ONLY: PointIsInsideTriangle2D
!----------------------------------------------------------------------------------------------------------------------!
USE MOD_Mesh_CGNS_Definitions,ONLY: ELEMTYPE_TRI3
USE MOD_Mesh_CGNS_Definitions,ONLY: ELEMTYPE_QUAD4
!----------------------------------------------------------------------------------------------------------------------!
IMPLICIT NONE
!----------------------------------------------------------------------------------------------------------------------!
! FORMAL ARGUMENTS
!----------------------------------------------------------------------------------------------------------------------!
TYPE(tElem),POINTER,INTENT(INOUT)    :: aElem
TYPE(tPointCoords),TARGET,INTENT(IN) :: Points(:)
!----------------------------------------------------------------------------------------------------------------------!
! LOCAL VARIABLES
!----------------------------------------------------------------------------------------------------------------------!
INTEGER :: iNode
INTEGER :: iPoint
INTEGER :: nPoints
INTEGER :: nContainedPoints
!----------------------------------------------------------------------------------------------------------------------!
REAL    :: Box2D(1:4,1:2)
REAL    :: Triangle2D(1:3,1:2)
!----------------------------------------------------------------------------------------------------------------------!
REAL,ALLOCATABLE :: ElementPoints(:,:)
REAL,ALLOCATABLE :: ElementBoundingBox(:,:)
!----------------------------------------------------------------------------------------------------------------------!
CHARACTER(LEN=256) :: ErrorMessage
CHARACTER(LEN=256) :: FunctionName = "DistributePointsInElem"
!----------------------------------------------------------------------------------------------------------------------!

nPoints = aElem%nNodes

IF (ALLOCATED(ElementPoints)) THEN
  DEALLOCATE(ElementPoints)
END IF
ALLOCATE(ElementPoints(1:nPoints,1:PP_nDims))

IF (ALLOCATED(ElementBoundingBox)) THEN
  DEALLOCATE(ElementBoundingBox)
END IF
ALLOCATE(ElementBoundingBox(1:2,1:PP_nDims))

DO iPoint=1,nPoints
  ElementPoints(iPoint,1:PP_nDims) = aElem%Nodes(iPoint)%Node%Coords(1:PP_nDims)
END DO

ElementBoundingBox = BoundingBox(PP_nDims,ElementPoints)

nContainedPoints = 0
DO iPoint=1,SIZE(Points)
  SELECT CASE(PP_nDims)
    CASE(2)
      SELECT CASE(aElem%ElemType)
        CASE(ELEMTYPE_TRI3)
          DO iNode=1,aElem%nNodes
            Triangle2D(iNode,1:2) = aElem%Nodes(iNode)%Node%Coords(1:2)
          END DO
          IF (.NOT. PointIsInsideTriangle2D(Points(iPoint)%Coords(1:2),Triangle2D(1:3,1:2))) THEN
            CYCLE
          END IF
        CASE(ELEMTYPE_QUAD4)
          DO iNode=1,aElem%nNodes
            Box2D(iNode,1:2) = aElem%Nodes(iNode)%Node%Coords(1:2)
          END DO
          IF (.NOT. PointIsInsideCube2D(Points(iPoint)%Coords(1:2),Box2D(1:4,1:2))) THEN
            CYCLE
          END IF
      END SELECT
    CASE(3)
      ErrorMessage = "FUNCTION NOT IMPLEMENTED IN 3D"
      CALL PrintError(__STAMP__,ErrorMessage,ModuleName,FunctionName)
  END SELECT
  nContainedPoints = nContainedPoints+1
END DO

IF (nContainedPoints .EQ. 0) THEN
  aElem%nPoints = 0
ELSE
  aElem%nPoints = nContainedPoints
END IF

!----------------------------------------------------------------------------------------------------------------------!
END SUBROUTINE DistributePointsInElem
!======================================================================================================================!
!
!
!
!======================================================================================================================!
SUBROUTINE DistributeSTLFacetsInElemList(iLevel)
!----------------------------------------------------------------------------------------------------------------------!
USE MOD_DataStructures
!----------------------------------------------------------------------------------------------------------------------!
USE MOD_MeshMain_vars,ONLY: tElem
USE MOD_MeshMain_vars,ONLY: ElemList
!----------------------------------------------------------------------------------------------------------------------!
USE MOD_GeometryMain_vars,ONLY: GeometryFacets
USE MOD_GeometryMain_vars,ONLY: KDTree_FacetsBarycenters
!----------------------------------------------------------------------------------------------------------------------!
IMPLICIT NONE
!----------------------------------------------------------------------------------------------------------------------!
! FORMAL ARGUMENTS
!----------------------------------------------------------------------------------------------------------------------!
INTEGER,INTENT(IN) :: iLevel
!----------------------------------------------------------------------------------------------------------------------!
! LOCAL VARIABLES
!----------------------------------------------------------------------------------------------------------------------!
TYPE(tElem),POINTER :: aElem
!----------------------------------------------------------------------------------------------------------------------!
INTEGER            :: nTabIn
!----------------------------------------------------------------------------------------------------------------------!
REAL               :: CalcTimeIni
REAL               :: CalcTimeEnd
CHARACTER(LEN=256) :: ElapsedTime
CHARACTER(LEN=256) :: Header
!----------------------------------------------------------------------------------------------------------------------!

nTabIn = 2
Header = "DISTRIBUTING GEOMETRY"
CALL PrintMessage(Header,nTabIn=nTabIn)

CalcTimeIni = RunningTime()

! Distributing Facets in Mesh
aElem => ElemList%FirstElem
DO WHILE (ASSOCIATED(aElem))
  CALL DistributeSTLFacetsInElem(aElem,GeometryFacets,KDTree_FacetsBarycenters,iLevel)
  aElem => aElem%NextElem
END DO

CalcTimeEnd = RunningTime()
CALL ComputeRuntime(CalcTimeEnd-CalcTimeIni,ElapsedTime)
Header = "Elapsed Time"
CALL PrintAnalyze(Header,ElapsedTime,4)

!----------------------------------------------------------------------------------------------------------------------!
END SUBROUTINE DistributeSTLFacetsInElemList
!======================================================================================================================!
!
!
!
!======================================================================================================================!
SUBROUTINE DistributeSTLFacetsInElem(aElem,Facets,KDTree_FacetsBarycenters,iLevel)
!----------------------------------------------------------------------------------------------------------------------!
USE MOD_DataStructures
!----------------------------------------------------------------------------------------------------------------------!
USE MOD_MeshMain_vars,ONLY: tElem
!----------------------------------------------------------------------------------------------------------------------!
USE MOD_GeometryMain_vars,ONLY: tFacetCoords
!----------------------------------------------------------------------------------------------------------------------!
USE MOD_MeshRefinement_vars,ONLY: ParametersMeshRefinement
!----------------------------------------------------------------------------------------------------------------------!
USE MOD_ComputationalGeometry,ONLY: BoundingBox
USE MOD_ComputationalGeometry,ONLY: PointIsInsideCube2D
USE MOD_ComputationalGeometry,ONLY: PointIsInsideCube3D
USE MOD_ComputationalGeometry,ONLY: TriangleBoxOverlap
!----------------------------------------------------------------------------------------------------------------------!
IMPLICIT NONE
!----------------------------------------------------------------------------------------------------------------------!
! FORMAL ARGUMENTS
!----------------------------------------------------------------------------------------------------------------------!
TYPE(tElem),POINTER,INTENT(INOUT)    :: aElem
TYPE(tFacetCoords),TARGET,INTENT(IN) :: Facets(:)
TYPE(tKDTree),POINTER,INTENT(INOUT)  :: KDTree_FacetsBarycenters
INTEGER,INTENT(IN)                   :: iLevel
!----------------------------------------------------------------------------------------------------------------------!
TYPE(tKDTreeNeighbors),ALLOCATABLE :: SearchResults(:)
!----------------------------------------------------------------------------------------------------------------------!
! LOCAL VARIABLES
!----------------------------------------------------------------------------------------------------------------------!
INTEGER :: nContainedFacets
!----------------------------------------------------------------------------------------------------------------------!
INTEGER :: iNeighbor
INTEGER :: nNeighbors
INTEGER :: nNeighborsFound
!----------------------------------------------------------------------------------------------------------------------!
REAL :: Radius
REAL :: Radius2
REAL :: BoxSize
REAL :: BoxCenter(1:3)
REAL :: BoxHalfSize(1:3)
REAL :: TriVerts(1:3,1:3)
REAL :: QueryPoint(1:3)
!----------------------------------------------------------------------------------------------------------------------!
REAL :: alpha
REAL :: dx(1:PP_nDims)
!----------------------------------------------------------------------------------------------------------------------!
INTEGER :: iPoint
INTEGER :: nPoints
REAL,ALLOCATABLE :: Points(:,:)
REAL,ALLOCATABLE :: ElementBoundingBox(:,:)
!----------------------------------------------------------------------------------------------------------------------!
CHARACTER(LEN=256) :: ErrorMessage
CHARACTER(LEN=256) :: FunctionName = "DistributeSTLFacetsInElem"
!----------------------------------------------------------------------------------------------------------------------!

nPoints = aElem%nNodes

IF (ALLOCATED(Points)) THEN
  DEALLOCATE(Points)
END IF
ALLOCATE(Points(1:nPoints,1:PP_nDims))

IF (ALLOCATED(ElementBoundingBox)) THEN
  DEALLOCATE(ElementBoundingBox)
END IF
ALLOCATE(ElementBoundingBox(1:2,1:PP_nDims))

DO iPoint=1,nPoints
  Points(iPoint,1:PP_nDims) = aElem%Nodes(iPoint)%Node%Coords(1:PP_nDims)
END DO

ElementBoundingBox = BoundingBox(PP_nDims,Points)

alpha = ParametersMeshRefinement%ElementEnlargementFactor
dx(1:PP_nDims) = ABS(ElementBoundingBox(2,1:PP_nDims)-ElementBoundingBox(1,1:PP_nDims))
ElementBoundingBox(1,1:PP_nDims) = ElementBoundingBox(1,1:PP_nDims) - 0.5*(alpha-1.0)*dx(1:PP_nDims)
ElementBoundingBox(2,1:PP_nDims) = ElementBoundingBox(2,1:PP_nDims) + 0.5*(alpha-1.0)*dx(1:PP_nDims)

SELECT CASE(PP_nDims)
  CASE(2)
    ErrorMessage = "FUNCTION NOT IMPLEMENTED IN 2D"
    CALL PrintError(__STAMP__,ErrorMessage,ModuleName,FunctionName)
  CASE(3)
    BoxHalfSize(1:3) = 0.5*(ElementBoundingBox(2,1:PP_nDims)-ElementBoundingBox(1,1:PP_nDims))
    BoxCenter(1:3)   = BoxHalfSize(1:PP_nDims)+ElementBoundingBox(1,1:PP_nDims)
    QueryPoint(1:3)  = BoxCenter(1:PP_nDims)

    BoxSize = ParametersMeshRefinement%TessellationMaxEdgeLength
! ! !     BoxSize = (0.5)**(iLevel)
    Radius  = 10.0*BoxSize
    Radius2 = Radius**2
    nNeighbors = CountNearestNeighborsInsideBallAroundQueryPoint(KDTree_FacetsBarycenters,QueryPoint,Radius2)
    IF (ALLOCATED(SearchResults)) THEN
      DEALLOCATE(SearchResults)
    END IF
    ALLOCATE(SearchResults(1:nNeighbors))
    CALL FindNearestNeighborsInsideBallAroundQueryPoint(&
      KDTree_FacetsBarycenters,QueryPoint,Radius2,nNeighbors,nNeighborsFound,SearchResults)
    nContainedFacets = 0
    DO iNeighbor=1,nNeighbors
      TriVerts(1:3,1:3) = Facets(SearchResults(iNeighbor)%NeighborIndex)%VerticesCoords(1:3,1:3)
      IF (TriangleBoxOverlap(BoxCenter,BoxHalfSize,TriVerts) .EQV. .FALSE.) THEN
        CYCLE
      ELSE
        nContainedFacets = nContainedFacets+1
      END IF
    END DO
END SELECT

IF (nContainedFacets .EQ. 0) THEN
  aElem%nFacets = 0
ELSE
  aElem%nFacets = nContainedFacets
END IF

!----------------------------------------------------------------------------------------------------------------------!
END SUBROUTINE DistributeSTLFacetsInElem
!======================================================================================================================!
!
!
!
!======================================================================================================================!
SUBROUTINE DistributeSTLFacetsInElem_OLD(aElem,Facets,KDTree_FacetsBarycenters,iLevel)
!----------------------------------------------------------------------------------------------------------------------!
USE MOD_DataStructures
!----------------------------------------------------------------------------------------------------------------------!
USE MOD_MeshMain_vars,ONLY: tElem
!----------------------------------------------------------------------------------------------------------------------!
USE MOD_GeometryMain_vars,ONLY: tFacetCoords
!----------------------------------------------------------------------------------------------------------------------!
USE MOD_MeshRefinement_vars,ONLY: ParametersMeshRefinement
!----------------------------------------------------------------------------------------------------------------------!
USE MOD_ComputationalGeometry,ONLY: PointIsInsideCube2D
USE MOD_ComputationalGeometry,ONLY: PointIsInsideCube3D
USE MOD_ComputationalGeometry,ONLY: TriangleBoxOverlap
!----------------------------------------------------------------------------------------------------------------------!
IMPLICIT NONE
!----------------------------------------------------------------------------------------------------------------------!
! FORMAL ARGUMENTS
!----------------------------------------------------------------------------------------------------------------------!
TYPE(tElem),POINTER,INTENT(INOUT)    :: aElem
TYPE(tFacetCoords),TARGET,INTENT(IN) :: Facets(:)
TYPE(tKDTree),POINTER,INTENT(INOUT)  :: KDTree_FacetsBarycenters
INTEGER,INTENT(IN)                   :: iLevel
!----------------------------------------------------------------------------------------------------------------------!
TYPE(tKDTreeNeighbors),ALLOCATABLE :: SearchResults(:)
!----------------------------------------------------------------------------------------------------------------------!
! LOCAL VARIABLES
!----------------------------------------------------------------------------------------------------------------------!
INTEGER :: nContainedFacets
!----------------------------------------------------------------------------------------------------------------------!
REAL    :: Box2D(1:4,1:2)
REAL    :: Box3D(1:8,1:3)
INTEGER :: iNeighbor
INTEGER :: nNeighbors
INTEGER :: nNeighborsFound
!----------------------------------------------------------------------------------------------------------------------!
REAL :: Radius
REAL :: Radius2
REAL :: BoxSize
REAL :: BoxCenter(1:3)
REAL :: BoxHalfSize(1:3)
REAL :: TriVerts(1:3,1:3)
REAL :: QueryPoint(1:3)
!----------------------------------------------------------------------------------------------------------------------!
REAL :: alpha
REAL :: dx
REAL :: dy
REAL :: dz
!----------------------------------------------------------------------------------------------------------------------!

SELECT CASE(PP_nDims)
  CASE(2)
    Box2D(1,1:2) = aElem%Nodes(1)%Node%Coords(1:2)
    Box2D(2,1:2) = aElem%Nodes(2)%Node%Coords(1:2)
    Box2D(3,1:2) = aElem%Nodes(3)%Node%Coords(1:2)
    Box2D(4,1:2) = aElem%Nodes(4)%Node%Coords(1:2)
  CASE(3)
    Box3D(1,1:3) = aElem%Nodes(1)%Node%Coords(1:3)
    Box3D(2,1:3) = aElem%Nodes(2)%Node%Coords(1:3)
    Box3D(3,1:3) = aElem%Nodes(3)%Node%Coords(1:3)
    Box3D(4,1:3) = aElem%Nodes(4)%Node%Coords(1:3)
    Box3D(5,1:3) = aElem%Nodes(5)%Node%Coords(1:3)
    Box3D(6,1:3) = aElem%Nodes(6)%Node%Coords(1:3)
    Box3D(7,1:3) = aElem%Nodes(7)%Node%Coords(1:3)
    Box3D(8,1:3) = aElem%Nodes(8)%Node%Coords(1:3)
    !**********************************************************!
    alpha = ParametersMeshRefinement%ElementEnlargementFactor

    dx = ABS(Box3D(2,1)-Box3D(1,1))
    dy = ABS(Box3D(4,2)-Box3D(1,2))
    dz = ABS(Box3D(5,3)-Box3D(1,3))

    ! Face 5 (X-)
    Box3D(1,1) = Box3D(1,1) - 0.5*(alpha-1.0)*dx
    Box3D(4,1) = Box3D(4,1) - 0.5*(alpha-1.0)*dx
    Box3D(5,1) = Box3D(5,1) - 0.5*(alpha-1.0)*dx
    Box3D(8,1) = Box3D(8,1) - 0.5*(alpha-1.0)*dx
    ! Face 3 (X+)
    Box3D(2,1) = Box3D(2,1) + 0.5*(alpha-1.0)*dx
    Box3D(3,1) = Box3D(3,1) + 0.5*(alpha-1.0)*dx
    Box3D(6,1) = Box3D(6,1) + 0.5*(alpha-1.0)*dx
    Box3D(7,1) = Box3D(7,1) + 0.5*(alpha-1.0)*dx
    ! Face 2 (Y-)
    Box3D(1,2) = Box3D(1,2) - 0.5*(alpha-1.0)*dy
    Box3D(2,2) = Box3D(2,2) - 0.5*(alpha-1.0)*dy
    Box3D(5,2) = Box3D(5,2) - 0.5*(alpha-1.0)*dy
    Box3D(6,2) = Box3D(6,2) - 0.5*(alpha-1.0)*dy
    ! Face 4 (Y+)
    Box3D(3,2) = Box3D(3,2) + 0.5*(alpha-1.0)*dy
    Box3D(4,2) = Box3D(4,2) + 0.5*(alpha-1.0)*dy
    Box3D(7,2) = Box3D(7,2) + 0.5*(alpha-1.0)*dy
    Box3D(8,2) = Box3D(8,2) + 0.5*(alpha-1.0)*dy
    ! Face 1 (Z-)
    Box3D(1,3) = Box3D(1,3) - 0.5*(alpha-1.0)*dz
    Box3D(2,3) = Box3D(2,3) - 0.5*(alpha-1.0)*dz
    Box3D(3,3) = Box3D(3,3) - 0.5*(alpha-1.0)*dz
    Box3D(4,3) = Box3D(4,3) - 0.5*(alpha-1.0)*dz
    ! Face 6 (Z+)
    Box3D(5,3) = Box3D(5,3) + 0.5*(alpha-1.0)*dz
    Box3D(6,3) = Box3D(6,3) + 0.5*(alpha-1.0)*dz
    Box3D(7,3) = Box3D(7,3) + 0.5*(alpha-1.0)*dz
    Box3D(8,3) = Box3D(8,3) + 0.5*(alpha-1.0)*dz
    !**********************************************************!
END SELECT

SELECT CASE(PP_nDims)
  CASE(3)
    BoxHalfSize(1:3) = 0.5*((/Box3D(2,1),Box3D(4,2),Box3D(5,3)/)-(/Box3D(1,1),Box3D(1,2),Box3D(1,3)/))
    BoxCenter(1:3)   = BoxHalfSize(1:3)+(/Box3D(1,1),Box3D(1,2),Box3D(1,3)/)
    QueryPoint(1:3)  = BoxCenter(1:3)

    BoxSize = ParametersMeshRefinement%TessellationMaxEdgeLength
! ! !     BoxSize = (0.5)**(iLevel)
    Radius  = 10.0*BoxSize
    Radius2 = Radius**2
    nNeighbors = CountNearestNeighborsInsideBallAroundQueryPoint(KDTree_FacetsBarycenters,QueryPoint,Radius2)
    IF (ALLOCATED(SearchResults)) THEN
      DEALLOCATE(SearchResults)
    END IF
    ALLOCATE(SearchResults(1:nNeighbors))
    CALL FindNearestNeighborsInsideBallAroundQueryPoint(&
      KDTree_FacetsBarycenters,QueryPoint,Radius2,nNeighbors,nNeighborsFound,SearchResults)
    nContainedFacets = 0
    DO iNeighbor=1,nNeighbors
      TriVerts(1:3,1:3) = Facets(SearchResults(iNeighbor)%NeighborIndex)%VerticesCoords(1:3,1:3)
      IF (TriangleBoxOverlap(BoxCenter,BoxHalfSize,TriVerts) .EQV. .FALSE.) THEN
        CYCLE
      ELSE
        nContainedFacets = nContainedFacets+1
      END IF
    END DO
END SELECT

IF (nContainedFacets .EQ. 0) THEN
  aElem%nFacets = 0
ELSE
  aElem%nFacets = nContainedFacets
END IF

!----------------------------------------------------------------------------------------------------------------------!
END SUBROUTINE DistributeSTLFacetsInElem_OLD
!======================================================================================================================!
!
!
!
!----------------------------------------------------------------------------------------------------------------------!
END MODULE MOD_MeshGeometryDistribution
!======================================================================================================================!
