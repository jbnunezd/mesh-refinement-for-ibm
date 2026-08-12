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
MODULE MOD_MeshBuiltIn_vars
!----------------------------------------------------------------------------------------------------------------------!
USE MOD_GLOBAL_vars
!----------------------------------------------------------------------------------------------------------------------!
IMPLICIT NONE
!----------------------------------------------------------------------------------------------------------------------!
PUBLIC
!----------------------------------------------------------------------------------------------------------------------!
! GLOBAL VARIABLES
!----------------------------------------------------------------------------------------------------------------------!
ABSTRACT INTERFACE
  SUBROUTINE BuildPhysicalDomain2D_INT(Nin,nElems,nBoxElems,BoxCorners,XRefDom2D,XPhysDom2D)
    IMPLICIT NONE
    INTEGER,INTENT(IN) :: Nin
    INTEGER,INTENT(IN) :: nElems
    INTEGER,INTENT(IN) :: nBoxElems(1:2)
    REAL,INTENT(IN)    :: BoxCorners(1:4,1:2)
    REAL,INTENT(IN)    :: XRefDom2D(1:2,0:Nin,0:Nin,1:nElems)
    REAL,INTENT(OUT)   :: XPhysDom2D(1:2,0:Nin,0:Nin,1:nElems)
  END SUBROUTINE BuildPhysicalDomain2D_INT
END INTERFACE
PROCEDURE(BuildPhysicalDomain2D_INT),POINTER :: BuildPhysicalDomain2D
!----------------------------------------------------------------------------------------------------------------------!
ABSTRACT INTERFACE
  SUBROUTINE BuildPhysicalDomain3D_INT(Nin,nElems,nBoxElems,BoxCorners,XRefDom3D,XPhysDom3D)
    IMPLICIT NONE
    INTEGER,INTENT(IN) :: Nin
    INTEGER,INTENT(IN) :: nElems
    INTEGER,INTENT(IN) :: nBoxElems(1:3)
    REAL,INTENT(IN)    :: BoxCorners(1:8,1:3)
    REAL,INTENT(IN)    :: XRefDom3D(1:3,0:Nin,0:Nin,0:Nin,1:nElems)
    REAL,INTENT(OUT)   :: XPhysDom3D(1:3,0:Nin,0:Nin,0:Nin,1:nElems)
  END SUBROUTINE BuildPhysicalDomain3D_INT
END INTERFACE
PROCEDURE(BuildPhysicalDomain3D_INT),POINTER :: BuildPhysicalDomain3D
!----------------------------------------------------------------------------------------------------------------------!
ABSTRACT INTERFACE
  SUBROUTINE Mapping2D_INT(WhichCurve,s,xc,x)
    IMPLICIT NONE
    INTEGER,INTENT(IN) :: WhichCurve
    REAL,INTENT(IN)    :: s
    REAL,INTENT(IN)    :: xc(1:4,1:2)
    REAL,INTENT(OUT)   :: x(1:2)
  END SUBROUTINE Mapping2D_INT
END INTERFACE
PROCEDURE(Mapping2D_INT),POINTER :: Mapping2D
!----------------------------------------------------------------------------------------------------------------------!
ABSTRACT INTERFACE
  SUBROUTINE Mapping3D_INT(WhichFace,s,xc,x)
    IMPLICIT NONE
    INTEGER,INTENT(IN) :: WhichFace
    REAL,INTENT(IN)    :: s(1:2)
    REAL,INTENT(IN)    :: xc(1:8,1:3)
    REAL,INTENT(OUT)   :: x(1:3)
  END SUBROUTINE Mapping3D_INT
END INTERFACE
PROCEDURE(Mapping3D_INT),POINTER :: Mapping3D
!----------------------------------------------------------------------------------------------------------------------!
ABSTRACT INTERFACE
  SUBROUTINE MeshStretching2D_INT(nElems,dx,dy)
    IMPLICIT NONE
    INTEGER,INTENT(IN) :: nElems(1:2)
    REAL,INTENT(OUT)   :: dx(1:nElems(1))
    REAL,INTENT(OUT)   :: dy(1:nElems(2))
  END SUBROUTINE MeshStretching2D_INT
END INTERFACE
PROCEDURE(MeshStretching2D_INT),POINTER :: MeshStretching2D
!----------------------------------------------------------------------------------------------------------------------!
ABSTRACT INTERFACE
  SUBROUTINE MeshStretching3D_INT(nElems,dx,dy,dz)
    IMPLICIT NONE
    INTEGER,INTENT(IN) :: nElems(1:3)
    REAL,INTENT(OUT)   :: dx(1:nElems(1))
    REAL,INTENT(OUT)   :: dy(1:nElems(2))
    REAL,INTENT(OUT)   :: dz(1:nElems(3))
  END SUBROUTINE MeshStretching3D_INT
END INTERFACE
PROCEDURE(MeshStretching3D_INT),POINTER :: MeshStretching3D
!----------------------------------------------------------------------------------------------------------------------!
INTEGER            :: nBoxElems2D(1:2)
INTEGER            :: nBoxElems3D(1:3)
REAL               :: BoxCorner2D(1:4,1:2)
REAL               :: BoxCorner3D(1:8,1:3)
REAL               :: RotationAngle2D
REAL               :: RotationAngle3D(1:3)
REAL,ALLOCATABLE   :: RotationMatrix2D(:,:)
REAL,ALLOCATABLE   :: RotationMatrix3D(:,:,:)
REAL,ALLOCATABLE   :: CGL_NGeo_Domain2D(:,:,:,:)
REAL,ALLOCATABLE   :: CGL_NGeo_Domain3D(:,:,:,:,:)
!----------------------------------------------------------------------------------------------------------------------!
TYPE tParametersMeshBuiltIn2D
  INTEGER :: nBoxElems2D(1:2)
  REAL    :: BoxCorner2D(1:4,1:2)
  REAL    :: RotationAngle2D
  REAL    :: MeshStretchingFactor2D(1:2)
  REAL    :: MeshDeformationFactor
  LOGICAL :: StretchMesh
  LOGICAL :: WarpMesh
  LOGICAL :: RotateMesh
  LOGICAL :: DebugMeshBuiltIn = .FALSE.
  INTEGER,ALLOCATABLE :: BoundaryMark(:)
  CHARACTER(LEN=256),ALLOCATABLE :: BoundaryName(:)
  CHARACTER(LEN=256) :: WhichMapping2D
  CHARACTER(LEN=256) :: WhichMeshType
  CHARACTER(LEN=256) :: WhichOutputBasis
  CHARACTER(LEN=256) :: WhichMeshStretching
END TYPE
!----------------------------------------------------------------------------------------------------------------------!
TYPE(tParametersMeshBuiltIn2D) :: ParametersMeshBuiltIn2D
!----------------------------------------------------------------------------------------------------------------------!
TYPE tParametersMeshBuiltIn3D
  INTEGER :: nBoxElems3D(1:3)
  REAL    :: BoxCorner3D(1:8,1:3)
  REAL    :: RotationAngle3D(1:3)
  REAL    :: MeshStretchingFactor3D(1:3)
  REAL    :: MeshDeformationFactor
  LOGICAL :: StretchMesh
  LOGICAL :: WarpMesh
  LOGICAL :: RotateMesh
  LOGICAL :: DebugMeshBuiltIn = .FALSE.
  INTEGER,ALLOCATABLE :: BoundaryMark(:)
  CHARACTER(LEN=256),ALLOCATABLE :: BoundaryName(:)
  CHARACTER(LEN=256) :: WhichMapping3D
  CHARACTER(LEN=256) :: WhichMeshType
  CHARACTER(LEN=256) :: WhichOutputBasis
  CHARACTER(LEN=256) :: WhichMeshStretching
END TYPE
!----------------------------------------------------------------------------------------------------------------------!
TYPE(tParametersMeshBuiltIn3D) :: ParametersMeshBuiltIn3D
!----------------------------------------------------------------------------------------------------------------------!
!
!----------------------------------------------------------------------------------------------------------------------!
! Chebyshev-Gauss-Lobatto (NGeo)
REAL,ALLOCATABLE :: CGL_xNodes_NGeo(:)
REAL,ALLOCATABLE :: CGL_xWeights_NGeo(:)
REAL,ALLOCATABLE :: CGL_xBaryWeights_NGeo(:)
REAL,ALLOCATABLE :: CGL_DMatrix_NGeo(:,:)

! Legendre-Gauss (N)
REAL,ALLOCATABLE :: LG_xNodes_N(:)
REAL,ALLOCATABLE :: LG_xWeights_N(:)
REAL,ALLOCATABLE :: LG_xBaryWeights_N(:)
REAL,ALLOCATABLE :: LG_DMatrix_N(:,:)

! Legendre-Gauss-Lobatto (N)
REAL,ALLOCATABLE :: LGL_xNodes_N(:)
REAL,ALLOCATABLE :: LGL_xWeights_N(:)
REAL,ALLOCATABLE :: LGL_xBaryWeights_N(:)
REAL,ALLOCATABLE :: LGL_DMatrix_N(:,:)

! Chebyshev-Gauss (N)
REAL,ALLOCATABLE :: CG_xNodes_N(:)
REAL,ALLOCATABLE :: CG_xWeights_N(:)
REAL,ALLOCATABLE :: CG_xBaryWeights_N(:)
REAL,ALLOCATABLE :: CG_DMatrix_N(:,:)

! Chebyshev-Gauss-Lobatto (N)
REAL,ALLOCATABLE :: CGL_xNodes_N(:)
REAL,ALLOCATABLE :: CGL_xWeights_N(:)
REAL,ALLOCATABLE :: CGL_xBaryWeights_N(:)
REAL,ALLOCATABLE :: CGL_DMatrix_N(:,:)

! Uniform (N)
REAL,ALLOCATABLE :: UNIFORM_xNodes_N(:)

! Chebyshev-Gauss-Lobatto (NGeo) to Chebyshev-Gauss-Lobatto (N)
REAL,ALLOCATABLE :: VDM_CGLNGeo_CGLN(:,:)
! Chebyshev-Gauss-Lobatto (NGeo) to Chebyshev-Gauss (N)
REAL,ALLOCATABLE :: VDM_CGLNGeo_CGN(:,:)
! Chebyshev-Gauss-Lobatto (NGeo) to Legendre-Gauss-Lobatto (N)
REAL,ALLOCATABLE :: VDM_CGLNGeo_LGLN(:,:)
! Chebyshev-Gauss-Lobatto (NGeo) to Legendre-Gauss (N)
REAL,ALLOCATABLE :: VDM_CGLNGeo_LGN(:,:)
! Chebyshev-Gauss-Lobatto (NGeo) to Uniform (N)
REAL,ALLOCATABLE :: VDM_CGLNGeo_UniformN(:,:)
! Chebyshev-Gauss-Lobatto (NGeo) to OutputBasis (N)
REAL,ALLOCATABLE :: VDM_CGLNGeo_OutputN(:,:)
!----------------------------------------------------------------------------------------------------------------------!
LOGICAL :: InitializeMeshBuiltInIsDone = .FALSE.
!----------------------------------------------------------------------------------------------------------------------!
END MODULE MOD_MeshBuiltIn_vars
!======================================================================================================================!
