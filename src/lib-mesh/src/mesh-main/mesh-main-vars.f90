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
MODULE MOD_MeshMain_vars
!----------------------------------------------------------------------------------------------------------------------!
USE MOD_GLOBAL_vars
!----------------------------------------------------------------------------------------------------------------------!
USE MOD_DataStructures,ONLY: tBucket
!----------------------------------------------------------------------------------------------------------------------!
IMPLICIT NONE
!----------------------------------------------------------------------------------------------------------------------!
PUBLIC
!----------------------------------------------------------------------------------------------------------------------!
! GLOBAL VARIABLES
!----------------------------------------------------------------------------------------------------------------------!
!
!----------------------------------------------------------------------------------------------------------------------!
INTEGER :: PP_nElems
INTEGER :: PP_nNodes
INTEGER :: PP_N
INTEGER :: PP_NGeo
!----------------------------------------------------------------------------------------------------------------------!
TYPE tMeshInfo
  INTEGER            :: nDims
  INTEGER            :: NGeo
  INTEGER            :: nElems
  INTEGER            :: nNodes
  INTEGER            :: nFaces
  INTEGER            :: nEdges
  INTEGER            :: nBCFaces
  INTEGER            :: nOutVars
  INTEGER            :: MaxRefLevel
  CHARACTER(LEN=256) :: FileVersion
  CHARACTER(LEN=256) :: ProgramName
  CHARACTER(LEN=256) :: ProjectName
  CHARACTER(LEN=256) :: BaseFileName
  CHARACTER(LEN=256),ALLOCATABLE :: CoordNames(:)
  CHARACTER(LEN=256),ALLOCATABLE :: DataNames(:)
  CHARACTER(LEN=256),ALLOCATABLE :: OutputVars(:)
END TYPE
!----------------------------------------------------------------------------------------------------------------------!
TYPE tMeshArraysInfo
  INTEGER :: nDims
  INTEGER :: NGeo
  INTEGER :: nElems
  INTEGER :: nNodes
  INTEGER :: nFaces
  INTEGER :: nEdges
  INTEGER :: nBCFaces
  INTEGER :: nEdge2Edge
  INTEGER :: nQuad4Quad
END TYPE
!----------------------------------------------------------------------------------------------------------------------!
!
!----------------------------------------------------------------------------------------------------------------------!
! ! ! TYPE tPoint
! ! !   INTEGER :: PointID
! ! !   REAL    :: Coords(1:3)
! ! ! END TYPE
!----------------------------------------------------------------------------------------------------------------------!
TYPE tNode
  REAL    :: Coords(1:3)
  INTEGER :: NodeID
  INTEGER :: BCFlag
  INTEGER :: RefCount
  INTEGER :: tmp
END TYPE
!----------------------------------------------------------------------------------------------------------------------!
TYPE tNodePtr
  TYPE(tNode),POINTER :: Node
END TYPE
!----------------------------------------------------------------------------------------------------------------------!
! ! ! TYPE tFacet
! ! !   REAL :: VerticesCoords(1:3,1:3)
! ! ! END TYPE
!----------------------------------------------------------------------------------------------------------------------!
TYPE tElem
  INTEGER :: nNodes
  INTEGER :: ElemID
  INTEGER :: Flag
  INTEGER :: ElemType
  !=====================================!
  ! QUADTREES/OCTREES
  !=====================================!
  INTEGER :: Level
  INTEGER :: nFacets
  INTEGER :: nPoints
  INTEGER,ALLOCATABLE    :: Coords(:)
  TYPE(tElem),POINTER    :: PrevElem
  TYPE(tElem),POINTER    :: NextElem
  TYPE(tSide),POINTER    :: FirstSide
  TYPE(tNodePtr),POINTER :: Nodes(:)
END TYPE tElem
!----------------------------------------------------------------------------------------------------------------------!
TYPE tElemList
  TYPE(tElem),POINTER :: FirstElem => NULL()
  TYPE(tElem),POINTER :: LastElem  => NULL()
END TYPE tElemList
!----------------------------------------------------------------------------------------------------------------------!
TYPE tElemPtr
  TYPE(tElem),POINTER :: Elem
END TYPE tElemPtr
!----------------------------------------------------------------------------------------------------------------------!
TYPE tSide
  INTEGER                :: nNodes
  INTEGER                :: LocSide
  INTEGER                :: SideID
  TYPE(tNodePtr),POINTER :: Nodes(:)
  TYPE(tBC),POINTER      :: BC
  TYPE(tSide),POINTER    :: NextElemSide
END TYPE tSide
!----------------------------------------------------------------------------------------------------------------------!
TYPE tSidePtr
  TYPE(tSide),POINTER :: Side
END TYPE tSidePtr
!----------------------------------------------------------------------------------------------------------------------!
TYPE tFaceNodes
  INTEGER                  :: FaceID
  INTEGER,ALLOCATABLE      :: NodeID(:)
  TYPE(tFaceNodes),POINTER :: PrevFaceNodes
  TYPE(tFaceNodes),POINTER :: NextFaceNodes
END TYPE tFaceNodes
!----------------------------------------------------------------------------------------------------------------------!
TYPE tFaceNodesList
  TYPE(tFaceNodes),POINTER :: FirstFaceNodes => NULL()
  TYPE(tFaceNodes),POINTER :: LastFaceNodes  => NULL()
END TYPE tFaceNodesList
!----------------------------------------------------------------------------------------------------------------------!
TYPE tEdgeNodes
  INTEGER                  :: EdgeID
  INTEGER,ALLOCATABLE      :: NodeID(:)
  TYPE(tEdgeNodes),POINTER :: PrevEdgeNodes
  TYPE(tEdgeNodes),POINTER :: NextEdgeNodes
END TYPE tEdgeNodes
!----------------------------------------------------------------------------------------------------------------------!
TYPE tEdgeNodesList
  TYPE(tEdgeNodes),POINTER :: FirstEdgeNodes => NULL()
  TYPE(tEdgeNodes),POINTER :: LastEdgeNodes  => NULL()
END TYPE tEdgeNodesList
!----------------------------------------------------------------------------------------------------------------------!
TYPE tEdge
  TYPE(tNodePtr)         :: Nodes(1:2)
  TYPE(tNodePtr),POINTER :: CurvedNodes(:)
  TYPE(tEdge),POINTER    :: NextEdge
END TYPE tEdge
!----------------------------------------------------------------------------------------------------------------------!
TYPE tEdgePtr
  TYPE(tEdge),POINTER :: Edge
END TYPE tEdgePtr
!----------------------------------------------------------------------------------------------------------------------!
TYPE tBC
  INTEGER :: BCMark
END TYPE tBC
!----------------------------------------------------------------------------------------------------------------------!
!
!----------------------------------------------------------------------------------------------------------------------!
TYPE(tElemList)      :: ElemList
TYPE(tFaceNodesList) :: FaceNodesList
!----------------------------------------------------------------------------------------------------------------------!
TYPE(tMeshInfo)       :: MeshInfo
TYPE(tMeshArraysInfo) :: MeshArraysInfo
!----------------------------------------------------------------------------------------------------------------------!
TYPE(tBucket),ALLOCATABLE :: NodesToFacesMap_EDGE(:)
TYPE(tBucket),ALLOCATABLE :: NodesToFacesMap_TRI(:)
TYPE(tBucket),ALLOCATABLE :: NodesToFacesMap_QUAD(:)
!----------------------------------------------------------------------------------------------------------------------!
TYPE(tBucket),ALLOCATABLE :: NodesToEdgesMap(:)
TYPE(tBucket),ALLOCATABLE :: NodesToSharedEdgesMap(:)
!----------------------------------------------------------------------------------------------------------------------!
LOGICAL :: PerformMeshBlanking = .FALSE.
CHARACTER(LEN=256) :: MeshConstructionMethod
!----------------------------------------------------------------------------------------------------------------------!
CHARACTER(LEN=256) :: ExtrusionDirection
INTEGER,PARAMETER  :: ElemNodeIDSortingForExtrusion(1:8) = (/1,4,8,5,2,3,7,6/)
!----------------------------------------------------------------------------------------------------------------------!
INTEGER,ALLOCATABLE            :: MeshData_BCIndex(:)
INTEGER,ALLOCATABLE            :: MeshData_BoundaryType(:,:)
INTEGER,ALLOCATABLE            :: MeshData_BoundaryMark(:)
CHARACTER(LEN=256),ALLOCATABLE :: MeshData_BoundaryName(:)
!----------------------------------------------------------------------------------------------------------------------!
LOGICAL,ALLOCATABLE :: MeshData_ElementsToRefineFlag(:)
!----------------------------------------------------------------------------------------------------------------------!
REAL,ALLOCATABLE    :: MeshData_NodesCoordinates(:,:)
INTEGER,ALLOCATABLE :: MeshData_ElementsToElementType(:)
INTEGER,ALLOCATABLE :: MeshData_ElementsToNodes(:,:)
INTEGER,ALLOCATABLE :: MeshData_ElementsToFaces(:,:)
INTEGER,ALLOCATABLE :: MeshData_ElementsToLevel(:)
INTEGER,ALLOCATABLE :: MeshData_ElementsToFlag(:)
INTEGER,ALLOCATABLE :: MeshData_EdgesToNodes(:,:)
INTEGER,ALLOCATABLE :: MeshData_FacesToNodes(:,:)
INTEGER,ALLOCATABLE :: MeshData_FacesToEdges(:,:)
INTEGER,ALLOCATABLE :: MeshData_FacesElementType(:)
INTEGER,ALLOCATABLE :: MeshData_BCFacesToElementType(:)
INTEGER,ALLOCATABLE :: MeshData_BCFacesToNodes(:,:)
INTEGER,ALLOCATABLE :: MeshData_BCFacesToMark(:)
INTEGER,ALLOCATABLE :: MeshData_BCFacesToLevel(:)
!----------------------------------------------------------------------------------------------------------------------!
INTEGER,ALLOCATABLE :: MeshData_MasterSlavesToNodes_Edge2Edge(:,:)
INTEGER,ALLOCATABLE :: MeshData_MasterSlavesToNodes_Tri4Tri(:,:)
INTEGER,ALLOCATABLE :: MeshData_MasterSlavesToNodes_Quad2Quad(:,:)
INTEGER,ALLOCATABLE :: MeshData_MasterSlavesToNodes_Quad4Quad(:,:)
!----------------------------------------------------------------------------------------------------------------------!
INTEGER,ALLOCATABLE :: MeshData_MasterSlavesToElements_Edge2Edge(:,:)
INTEGER,ALLOCATABLE :: MeshData_MasterSlavesToElements_Tri4Tri(:,:)
INTEGER,ALLOCATABLE :: MeshData_MasterSlavesToElements_Quad2Quad(:,:)
INTEGER,ALLOCATABLE :: MeshData_MasterSlavesToElements_Quad4Quad(:,:)
!----------------------------------------------------------------------------------------------------------------------!
REAL,ALLOCATABLE    :: MeshData_ElementsToOutputVars(:,:,:)
REAL,ALLOCATABLE    :: MeshData_ElementsToNodesCoordinates(:,:,:)
REAL,ALLOCATABLE    :: MeshData_ElementsToNodesCoordinates2D(:,:,:,:)
REAL,ALLOCATABLE    :: MeshData_ElementsToNodesCoordinates3D(:,:,:,:,:)
!----------------------------------------------------------------------------------------------------------------------!
!
!----------------------------------------------------------------------------------------------------------------------!
END MODULE MOD_MeshMain_vars
!======================================================================================================================!
