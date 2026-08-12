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
MODULE MOD_MeshBuiltIn
!----------------------------------------------------------------------------------------------------------------------!
USE MOD_GLOBAL_vars
!----------------------------------------------------------------------------------------------------------------------!
IMPLICIT NONE
!----------------------------------------------------------------------------------------------------------------------!
PRIVATE
!----------------------------------------------------------------------------------------------------------------------!
INTERFACE InitializeMeshBuiltIn
  MODULE PROCEDURE InitializeMeshBuiltIn
END INTERFACE

INTERFACE MeshBuiltIn
  MODULE PROCEDURE MeshBuiltIn
END INTERFACE
!----------------------------------------------------------------------------------------------------------------------!
PUBLIC :: InitializeMeshBuiltIn
PUBLIC :: MeshBuiltIn
!----------------------------------------------------------------------------------------------------------------------!
CHARACTER(LEN=256) :: ModuleName = "MeshBuiltIn"
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
SUBROUTINE InitializeMeshBuiltIn()
!----------------------------------------------------------------------------------------------------------------------!
USE MOD_DataStructures
USE MOD_ConfigFilesTools
USE MOD_NumericsTools
!----------------------------------------------------------------------------------------------------------------------!
USE MOD_MeshMain_vars,ONLY: PP_NGeo
!----------------------------------------------------------------------------------------------------------------------!
USE MOD_MeshBuiltIn_vars
!----------------------------------------------------------------------------------------------------------------------!
IMPLICIT NONE
!----------------------------------------------------------------------------------------------------------------------!
! FORMAL ARGUMENTS
!----------------------------------------------------------------------------------------------------------------------!
! LOCAL VARIABLES
!----------------------------------------------------------------------------------------------------------------------!
CHARACTER(LEN=256) :: Header
CHARACTER(LEN=256) :: ErrorMessage
CHARACTER(LEN=256) :: FunctionName = "InitializeMeshBuiltIn"
!----------------------------------------------------------------------------------------------------------------------!

IF (InitializeMeshBuiltInIsDone) THEN
  SWRITE(UNIT_SCREEN,*) "InitializeMeshBuiltIn not ready to be called or already called."
  RETURN
END IF

Header = "INITIALIZING BUILT-IN MESH MODULE..."
CALL PrintHeader(Header)

PP_NGeo = GetInteger('NGeo','1')

CALL InitializeMeshBasis()

SELECT CASE(PP_nDims)
  CASE(2)
    CALL InitializeMeshBuiltIn2D()
  CASE(3)
    CALL InitializeMeshBuiltIn3D()
  CASE DEFAULT
    ErrorMessage = "Invalid value for MeshDimension"
    CALL PrintError(__STAMP__,ErrorMessage,ModuleName,FunctionName)
END SELECT

InitializeMeshBuiltInIsDone = .TRUE.

!----------------------------------------------------------------------------------------------------------------------!
END SUBROUTINE InitializeMeshBuiltIn
!======================================================================================================================!
!
!
!
!======================================================================================================================!
SUBROUTINE InitializeMeshBasis()
!----------------------------------------------------------------------------------------------------------------------!
USE MOD_DataStructures
USE MOD_ConfigFilesTools
USE MOD_NumericsTools
!----------------------------------------------------------------------------------------------------------------------!
USE MOD_MeshMain_vars,ONLY: PP_N
USE MOD_MeshMain_vars,ONLY: PP_NGeo
!----------------------------------------------------------------------------------------------------------------------!
USE MOD_MeshBuiltIn_vars
!----------------------------------------------------------------------------------------------------------------------!
IMPLICIT NONE
!----------------------------------------------------------------------------------------------------------------------!
! FORMAL ARGUMENTS
!----------------------------------------------------------------------------------------------------------------------!
! LOCAL VARIABLES
!----------------------------------------------------------------------------------------------------------------------!
INTEGER :: i
!----------------------------------------------------------------------------------------------------------------------!

PP_N = PP_NGeo

! Constructing xNodes, xWeights, xBaryWeights, and DMatrix

! Chebyshev-Gauss-Lobatto (PP_NGeo)
ALLOCATE(CGL_xNodes_NGeo(0:PP_NGeo))
ALLOCATE(CGL_xWeights_NGeo(0:PP_NGeo))
ALLOCATE(CGL_xBaryWeights_NGeo(0:PP_NGeo))
ALLOCATE(CGL_DMatrix_NGeo(0:PP_NGeo,0:PP_NGeo))
CALL ChebyshevGaussLobattoNodesAndWeights(PP_NGeo,CGL_xNodes_NGeo,CGL_xWeights_NGeo)
CALL BarycentricWeights(PP_NGeo,CGL_xNodes_NGeo,CGL_xBaryWeights_NGeo)
CALL PolynomialDerivativeMatrix(PP_NGeo,CGL_xNodes_NGeo,CGL_DMatrix_NGeo)

! Chebyshev-Gauss (PP_N)
ALLOCATE(CG_xNodes_N(0:PP_N))
ALLOCATE(CG_xWeights_N(0:PP_N))
ALLOCATE(CG_xBaryWeights_N(0:PP_N))
ALLOCATE(CG_DMatrix_N(0:PP_N,0:PP_N))
CALL ChebyshevGaussNodesAndWeights(PP_N,CG_xNodes_N,CG_xWeights_N)
CALL BarycentricWeights(PP_N,CG_xNodes_N,CG_xBaryWeights_N)
CALL PolynomialDerivativeMatrix(PP_N,CG_xNodes_N,CG_DMatrix_N)

! Chebyshev-Gauss-Lobatto (PP_N)
ALLOCATE(CGL_xNodes_N(0:PP_N))
ALLOCATE(CGL_xWeights_N(0:PP_N))
ALLOCATE(CGL_xBaryWeights_N(0:PP_N))
ALLOCATE(CGL_DMatrix_N(0:PP_N,0:PP_N))
CALL ChebyshevGaussLobattoNodesAndWeights(PP_N,CGL_xNodes_N,CGL_xWeights_N)
CALL BarycentricWeights(PP_N,CGL_xNodes_N,CGL_xBaryWeights_N)
CALL PolynomialDerivativeMatrix(PP_N,CGL_xNodes_N,CGL_DMatrix_N)

! Legendre-Gauss (PP_N)
ALLOCATE(LG_xNodes_N(0:PP_N))
ALLOCATE(LG_xWeights_N(0:PP_N))
ALLOCATE(LG_xBaryWeights_N(0:PP_N))
ALLOCATE(LG_DMatrix_N(0:PP_N,0:PP_N))
CALL LegendreGaussNodesAndWeights(PP_N,LG_xNodes_N,LG_xWeights_N)
CALL BarycentricWeights(PP_N,LG_xNodes_N,LG_xBaryWeights_N)
CALL PolynomialDerivativeMatrix(PP_N,LG_xNodes_N,LG_DMatrix_N)

! Legendre-Gauss-Lobatto (PP_N)
ALLOCATE(LGL_xNodes_N(0:PP_N))
ALLOCATE(LGL_xWeights_N(0:PP_N))
ALLOCATE(LGL_xBaryWeights_N(0:PP_N))
ALLOCATE(LGL_DMatrix_N(0:PP_N,0:PP_N))
CALL LegendreGaussLobattoNodesAndWeights(PP_N,LGL_xNodes_N,LGL_xWeights_N)
CALL BarycentricWeights(PP_N,LGL_xNodes_N,LGL_xBaryWeights_N)
CALL PolynomialDerivativeMatrix(PP_N,LGL_xNodes_N,LGL_DMatrix_N)

! Vandermonde Matrices

! Chebyshev-Gauss-Lobatto (PP_NGeo) to Chebyshev-Gauss-Lobatto (PP_N)
ALLOCATE(VDM_CGLNGeo_CGLN(0:PP_N,0:PP_NGeo))
CALL PolynomialInterpolationMatrix(&
  PP_NGeo,PP_N,CGL_xNodes_NGeo,CGL_xBaryWeights_NGeo,CGL_xNodes_N,VDM_CGLNGeo_CGLN)

! Chebyshev-Gauss-Lobatto (PP_NGeo) to Chebyshev-Gauss (PP_N)
ALLOCATE(VDM_CGLNGeo_CGN(0:PP_N,0:PP_NGeo))
CALL PolynomialInterpolationMatrix(&
  PP_NGeo,PP_N,CGL_xNodes_NGeo,CGL_xBaryWeights_NGeo,CG_xNodes_N,VDM_CGLNGeo_CGN)

! Chebyshev-Gauss-Lobatto (PP_NGeo) to Legendre-Gauss-Lobatto (PP_N)
ALLOCATE(VDM_CGLNGeo_LGLN(0:PP_N,0:PP_NGeo))
CALL PolynomialInterpolationMatrix(&
  PP_NGeo,PP_N,CGL_xNodes_NGeo,CGL_xBaryWeights_NGeo,LGL_xNodes_N,VDM_CGLNGeo_LGLN)

! Chebyshev-Gauss-Lobatto (PP_NGeo) to Legendre-Gauss (PP_N)
ALLOCATE(VDM_CGLNGeo_LGN(0:PP_N,0:PP_NGeo))
CALL PolynomialInterpolationMatrix(&
  PP_NGeo,PP_N,CGL_xNodes_NGeo,CGL_xBaryWeights_NGeo,LG_xNodes_N,VDM_CGLNGeo_LGN)

! Chebyshev-Gauss-Lobatto (PP_NGeo) to Equidistant (PP_N)
ALLOCATE(UNIFORM_xNodes_N(0:PP_N))
DO i=0,PP_N
  UNIFORM_xNodes_N(i) = -1.0+(2.0/REAL(PP_N))*REAL(i)
END DO
ALLOCATE(VDM_CGLNGeo_UniformN(0:PP_N,0:PP_NGeo))
CALL PolynomialInterpolationMatrix(&
  PP_NGeo,PP_N,CGL_xNodes_NGeo,CGL_xBaryWeights_NGeo,UNIFORM_xNodes_N,VDM_CGLNGeo_UniformN)

!----------------------------------------------------------------------------------------------------------------------!
END SUBROUTINE InitializeMeshBasis
!======================================================================================================================!
!
!
!
!======================================================================================================================!
SUBROUTINE InitializeMeshBuiltIn2D()
!----------------------------------------------------------------------------------------------------------------------!
USE MOD_DataStructures
USE MOD_ConfigFilesTools
USE MOD_NumericsTools
!----------------------------------------------------------------------------------------------------------------------!
USE MOD_MeshBuiltIn_vars
!----------------------------------------------------------------------------------------------------------------------!
USE MOD_MeshMain_vars,ONLY: PP_N
USE MOD_MeshMain_vars,ONLY: PP_NGeo
USE MOD_MeshMain_vars,ONLY: PP_nElems
USE MOD_MeshMain_vars,ONLY: PP_nNodes
USE MOD_MeshMain_vars,ONLY: PerformMeshBlanking
USE MOD_MeshMain_vars,ONLY: MeshData_BoundaryName
USE MOD_MeshMain_vars,ONLY: MeshData_BoundaryMark
USE MOD_MeshMain_vars,ONLY: MeshData_ElementsToNodesCoordinates2D
!----------------------------------------------------------------------------------------------------------------------!
USE MOD_MeshBuiltInMappings2D,ONLY: Mapping2D_FullDisk
USE MOD_MeshBuiltInMappings2D,ONLY: Mapping2D_HalfDisk
USE MOD_MeshBuiltInMappings2D,ONLY: Mapping2D_Nozzle
USE MOD_MeshBuiltInMappings2D,ONLY: Mapping2D_StraightQuad
USE MOD_MeshBuiltInMappings2D,ONLY: Mapping2D_SineBump
USE MOD_MeshBuiltInMappings2D,ONLY: Mapping2D_CurvedChannel
!----------------------------------------------------------------------------------------------------------------------!
USE MOD_MeshBuiltInStretchingFunctions2D,ONLY: MeshStretching2D_Uniform
USE MOD_MeshBuiltInStretchingFunctions2D,ONLY: MeshStretching2D_GeometricProgression
USE MOD_MeshBuiltInStretchingFunctions2D,ONLY: MeshStretching2D_TwoSidesBoundariesHyperbolic
USE MOD_MeshBuiltInStretchingFunctions2D,ONLY: MeshStretching2D_TwoSidesBoundariesPotential
!----------------------------------------------------------------------------------------------------------------------!
IMPLICIT NONE
!----------------------------------------------------------------------------------------------------------------------!
! FORMAL ARGUMENTS
!----------------------------------------------------------------------------------------------------------------------!
! LOCAL VARIABLES
!----------------------------------------------------------------------------------------------------------------------!
INTEGER :: i
INTEGER :: nBoxCorners
INTEGER :: nBoundaries
!----------------------------------------------------------------------------------------------------------------------!
CHARACTER(LEN=256) :: ErrorMessage
CHARACTER(LEN=256) :: FunctionName = "InitializeMeshBuiltIn2D"
!----------------------------------------------------------------------------------------------------------------------!

ParametersMeshBuiltIn2D%nBoxElems2D = GetIntegerArray('nBoxElems2D',2,'10,10')

nBoxCorners = CountStrings('BoxCorner',0)

SELECT CASE(nBoxCorners)
  CASE(4)
    DO i=1,nBoxCorners
      ParametersMeshBuiltIn2D%BoxCorner2D(i,:) = GetRealArray('BoxCorner',2)
    END DO
  CASE DEFAULT
    ErrorMessage = "Wrong number of BoxCorners"
    CALL PrintError(__STAMP__,ErrorMessage,ModuleName,FunctionName)
END SELECT

ParametersMeshBuiltIn2D%WhichMeshType         = GetString('WhichMeshType','cartesian-domain')
ParametersMeshBuiltIn2D%WhichMapping2D        = GetString('WhichMapping2D','straight-quad')
ParametersMeshBuiltIn2D%WhichOutputBasis      = GetString('WhichOutputBasis','chebyshev-gauss')
ParametersMeshBuiltIn2D%StretchMesh           = GetLogical('StretchMesh','.FALSE.')
ParametersMeshBuiltIn2D%WhichMeshStretching   = GetString('WhichMeshStretching')
ParametersMeshBuiltIn2D%WarpMesh              = GetLogical('WarpMesh','.FALSE.')
ParametersMeshBuiltIn2D%MeshDeformationFactor = GetReal('MeshDeformationFactor','0.01')
ParametersMeshBuiltIn2D%RotateMesh            = GetLogical('RotateMesh','.FALSE.')
ParametersMeshBuiltIn2D%DebugMeshBuiltIn      = GetLogical('DebugMeshBuiltIn','.FALSE.')
ParametersMeshBuiltIn2D%RotationAngle2D       = GetReal('RotationAngle2D','0.0')

PerformMeshBlanking = GetLogical('PerformMeshBlanking','.FALSE.')

! Boundary Conditions

IF (PerformMeshBlanking .EQV. .FALSE.) THEN
  nBoundaries = CountStrings('BoundaryMark',0)
  IF (nBoundaries .EQ. 4) THEN
    ALLOCATE(ParametersMeshBuiltIn2D%BoundaryName(1:nBoundaries))
    ALLOCATE(ParametersMeshBuiltIn2D%BoundaryMark(1:nBoundaries))
    DO i=1,nBoundaries
      ParametersMeshBuiltIn2D%BoundaryName(i) = GetString('BoundaryName')
      ParametersMeshBuiltIn2D%BoundaryMark(i) = GetInteger('BoundaryMark')
    END DO
  ELSE
    ErrorMessage = "Wrong number of BoundaryMark and BoundaryName (Blanking OFF)"
    CALL PrintError(__STAMP__,ErrorMessage,ModuleName,FunctionName)
  END IF
ELSE
  nBoundaries = CountStrings('BoundaryMark',0)
  IF (nBoundaries .EQ. 5) THEN
    ALLOCATE(ParametersMeshBuiltIn2D%BoundaryName(1:nBoundaries))
    ALLOCATE(ParametersMeshBuiltIn2D%BoundaryMark(1:nBoundaries))
    DO i=1,nBoundaries
      ParametersMeshBuiltIn2D%BoundaryName(i) = GetString('BoundaryName')
      ParametersMeshBuiltIn2D%BoundaryMark(i) = GetInteger('BoundaryMark')
    END DO
  ELSE
    ErrorMessage = "Wrong number of BoundaryMark and BoundaryName (Blanking ON)"
    CALL PrintError(__STAMP__,ErrorMessage,ModuleName,FunctionName)
  END IF
END IF

ALLOCATE(MeshData_BoundaryName(1:nBoundaries))
ALLOCATE(MeshData_BoundaryMark(1:nBoundaries))

MeshData_BoundaryName = ParametersMeshBuiltIn2D%BoundaryName
MeshData_BoundaryMark = ParametersMeshBuiltIn2D%BoundaryMark

SELECT CASE(LowerCase(ParametersMeshBuiltIn2D%WhichMeshType))
  CASE('cartesian-domain')
    BuildPhysicalDomain2D => BuildPhysicalDomain2D_CartesianDomain
  CASE('bilinear-domain')
    BuildPhysicalDomain2D => BuildPhysicalDomain2D_BilinearDomain
  CASE('curved-domain')
    BuildPhysicalDomain2D => BuildPhysicalDomain2D_CurvedDomain
  CASE DEFAULT
    ErrorMessage = "Mesh Type for BuildPhysicalDomain2D not implemented"
    CALL PrintError(__STAMP__,ErrorMessage,ModuleName,FunctionName)
END SELECT

SELECT CASE(LowerCase(ParametersMeshBuiltIn2D%WhichMapping2D))
  CASE('straight-quad')
    Mapping2D => Mapping2D_StraightQuad
  CASE('full-disk')
    Mapping2D => Mapping2D_FullDisk
  CASE('half-disk')
    Mapping2D => Mapping2D_HalfDisk
  CASE('nozzle')
    Mapping2D => Mapping2D_Nozzle
  CASE('sine-bump')
    Mapping2D => Mapping2D_SineBump
  CASE('curved-channel')
    Mapping2D => Mapping2D_CurvedChannel
  CASE DEFAULT
    ErrorMessage = "Analytical Mapping not implemented"
    CALL PrintError(__STAMP__,ErrorMessage,ModuleName,FunctionName)
END SELECT

SELECT CASE(LowerCase(ParametersMeshBuiltIn2D%WhichMeshStretching))
  CASE('uniform')
    MeshStretching2D => MeshStretching2D_Uniform
  CASE('geometric-progression')
    MeshStretching2D => MeshStretching2D_GeometricProgression
  CASE('two-boundaries-hyperbolic')
    MeshStretching2D => MeshStretching2D_TwoSidesBoundariesHyperbolic
  CASE('two-boundaries-potential')
    MeshStretching2D => MeshStretching2D_TwoSidesBoundariesPotential
  CASE DEFAULT
    ErrorMessage = "Mesh Type for BuildPhysicalDomain not implemented"
    CALL PrintError(__STAMP__,ErrorMessage,ModuleName,FunctionName)
END SELECT
IF (ParametersMeshBuiltIn2D%StretchMesh .EQV. .TRUE.) THEN
  ParametersMeshBuiltIn2D%MeshStretchingFactor2D = GetRealArray('MeshStretchingFactor2D',2,'1.0,1.0')
END IF

ALLOCATE(VDM_CGLNGeo_OutputN(0:PP_N,0:PP_NGeo))
SELECT CASE(LowerCase(ParametersMeshBuiltIn2D%WhichOutputBasis))
  CASE('chebyshev-gauss')
    VDM_CGLNGeo_OutputN = VDM_CGLNGeo_CGN
  CASE('chebyshev-gauss-lobatto')
    VDM_CGLNGeo_OutputN = VDM_CGLNGeo_CGLN
  CASE('legendre-gauss')
    VDM_CGLNGeo_OutputN = VDM_CGLNGeo_LGN
  CASE('legendre-gauss-lobatto')
    VDM_CGLNGeo_OutputN = VDM_CGLNGeo_LGLN
  CASE('uniform')
    VDM_CGLNGeo_OutputN = VDM_CGLNGeo_UniformN
  CASE DEFAULT
    ErrorMessage = "Mesh Output Basis not implemented"
    CALL PrintError(__STAMP__,ErrorMessage,ModuleName,FunctionName)
END SELECT

! Mesh Variables
RotationAngle2D = ParametersMeshBuiltIn2D%RotationAngle2D
nBoxElems2D     = ParametersMeshBuiltIn2D%nBoxElems2D
BoxCorner2D     = ParametersMeshBuiltIn2D%BoxCorner2D
PP_nElems       = nBoxElems2D(1)*nBoxElems2D(2)
PP_nNodes       = PP_nElems*(PP_NGeo+1)**2

ALLOCATE(CGL_NGeo_Domain2D(1:2,0:PP_NGeo,0:PP_NGeo,1:PP_nElems))
ALLOCATE(RotationMatrix2D(1:2,1:2))

ALLOCATE(MeshData_ElementsToNodesCoordinates2D(1:2,0:PP_N,0:PP_N,1:PP_nElems))

RotationAngle2D  = ACOS(-1.0)*RotationAngle2D/180.0
RotationMatrix2D = 0.0

RotationMatrix2D(1,1) = COS(RotationAngle2D)
RotationMatrix2D(1,2) =-SIN(RotationAngle2D)
RotationMatrix2D(2,1) = SIN(RotationAngle2D)
RotationMatrix2D(2,2) = COS(RotationAngle2D)

!----------------------------------------------------------------------------------------------------------------------!
END SUBROUTINE InitializeMeshBuiltIn2D
!======================================================================================================================!
!
!
!
!======================================================================================================================!
SUBROUTINE InitializeMeshBuiltIn3D()
!----------------------------------------------------------------------------------------------------------------------!
USE MOD_DataStructures
USE MOD_ConfigFilesTools
USE MOD_NumericsTools
!----------------------------------------------------------------------------------------------------------------------!
USE MOD_MeshBuiltIn_vars
!----------------------------------------------------------------------------------------------------------------------!
USE MOD_MeshMain_vars,ONLY: PP_N
USE MOD_MeshMain_vars,ONLY: PP_NGeo
USE MOD_MeshMain_vars,ONLY: PP_nElems
USE MOD_MeshMain_vars,ONLY: PP_nNodes
USE MOD_MeshMain_vars,ONLY: PerformMeshBlanking
USE MOD_MeshMain_vars,ONLY: MeshData_BoundaryMark
USE MOD_MeshMain_vars,ONLY: MeshData_BoundaryName
USE MOD_MeshMain_vars,ONLY: MeshData_ElementsToNodesCoordinates3D
!----------------------------------------------------------------------------------------------------------------------!
USE MOD_MeshBuiltInMappings3D,ONLY: Mapping3D_StraightHexa
USE MOD_MeshBuiltInMappings3D,ONLY: Mapping3D_FullCylinder
USE MOD_MeshBuiltInMappings3D,ONLY: Mapping3D_HalfCylinder
!----------------------------------------------------------------------------------------------------------------------!
USE MOD_MeshBuiltInStretchingFunctions3D,ONLY: MeshStretching3D_Uniform
USE MOD_MeshBuiltInStretchingFunctions3D,ONLY: MeshStretching3D_GeometricProgression
USE MOD_MeshBuiltInStretchingFunctions3D,ONLY: MeshStretching3D_ThreeSidesBoundariesHyperbolic
USE MOD_MeshBuiltInStretchingFunctions3D,ONLY: MeshStretching3D_ThreeSidesBoundariesPotential
!----------------------------------------------------------------------------------------------------------------------!
IMPLICIT NONE
!----------------------------------------------------------------------------------------------------------------------!
! FORMAL ARGUMENTS
!----------------------------------------------------------------------------------------------------------------------!
! LOCAL VARIABLES
!----------------------------------------------------------------------------------------------------------------------!
INTEGER :: i
INTEGER :: nBoxCorners
INTEGER :: nBoundaries
!----------------------------------------------------------------------------------------------------------------------!
CHARACTER(LEN=256) :: ErrorMessage
CHARACTER(LEN=256) :: FunctionName = "InitializeMeshBuiltIn3D"
!----------------------------------------------------------------------------------------------------------------------!

ParametersMeshBuiltIn3D%nBoxElems3D = GetIntegerArray('nBoxElems3D',3,'10,10,10')

nBoxCorners = CountStrings('BoxCorner',0)

SELECT CASE(nBoxCorners)
  CASE(8)
    DO i=1,nBoxCorners
      ParametersMeshBuiltIn3D%BoxCorner3D(i,:) = GetRealArray('BoxCorner',3)
    END DO
  CASE DEFAULT
    ErrorMessage = "Wrong number of BoxCorners"
    CALL PrintError(__STAMP__,ErrorMessage,ModuleName,FunctionName)
END SELECT

ParametersMeshBuiltIn3D%WhichMeshType           = GetString('WhichMeshType','cartesian-domain')
ParametersMeshBuiltIn3D%WhichMapping3D          = GetString('WhichMapping3D','straight-quad')
ParametersMeshBuiltIn3D%WhichOutputBasis        = GetString('WhichOutputBasis','chebyshev-gauss')
ParametersMeshBuiltIn3D%StretchMesh             = GetLogical('StretchMesh','.FALSE.')
ParametersMeshBuiltIn3D%WhichMeshStretching     = GetString('WhichMeshStretching')
ParametersMeshBuiltIn3D%WarpMesh                = GetLogical('WarpMesh','.FALSE.')
ParametersMeshBuiltIn3D%MeshDeformationFactor   = GetReal('MeshDeformationFactor','0.01')
ParametersMeshBuiltIn3D%RotateMesh              = GetLogical('RotateMesh','.FALSE.')
ParametersMeshBuiltIn3D%DebugMeshBuiltIn        = GetLogical('DebugMeshBuiltIn','.FALSE.')
ParametersMeshBuiltIn3D%RotationAngle3D         = GetRealArray('RotationAngle3D',3,'0.0,0.0,0.0')

PerformMeshBlanking = GetLogical('PerformMeshBlanking','.FALSE.')

! Boundary Conditions
IF (PerformMeshBlanking .EQV. .FALSE.) THEN
  nBoundaries = CountStrings('BoundaryMark',0)
  IF ((nBoundaries .EQ. 6) .OR. (nBoundaries .EQ. 7)) THEN
    ALLOCATE(ParametersMeshBuiltIn3D%BoundaryName(1:nBoundaries))
    ALLOCATE(ParametersMeshBuiltIn3D%BoundaryMark(1:nBoundaries))
    DO i=1,6
      ParametersMeshBuiltIn3D%BoundaryName(i) = GetString('BoundaryName')
      ParametersMeshBuiltIn3D%BoundaryMark(i) = GetInteger('BoundaryMark')
    END DO
  ELSE
    ErrorMessage = "Wrong number of BoundaryMark and BoundaryName (Blanking OFF)"
    CALL PrintError(__STAMP__,ErrorMessage,ModuleName,FunctionName)
  END IF
ELSE
  nBoundaries = CountStrings('BoundaryMark',0)
  IF (nBoundaries .EQ. 7) THEN
    ALLOCATE(ParametersMeshBuiltIn3D%BoundaryName(1:nBoundaries))
    ALLOCATE(ParametersMeshBuiltIn3D%BoundaryMark(1:nBoundaries))
    DO i=1,nBoundaries
      ParametersMeshBuiltIn3D%BoundaryName(i) = GetString('BoundaryName')
      ParametersMeshBuiltIn3D%BoundaryMark(i) = GetInteger('BoundaryMark')
    END DO
  ELSE
    ErrorMessage = "Wrong number of BoundaryMark and BoundaryName (Blanking ON)"
    CALL PrintError(__STAMP__,ErrorMessage,ModuleName,FunctionName)
  END IF
END IF

ALLOCATE(MeshData_BoundaryName(1:nBoundaries))
ALLOCATE(MeshData_BoundaryMark(1:nBoundaries))

MeshData_BoundaryName(1:6) = ParametersMeshBuiltIn3D%BoundaryName(1:6)
MeshData_BoundaryMark(1:6) = ParametersMeshBuiltIn3D%BoundaryMark(1:6)

SELECT CASE(LowerCase(ParametersMeshBuiltIn3D%WhichMeshType))
  CASE('cartesian-domain')
    BuildPhysicalDomain3D => BuildPhysicalDomain3D_CartesianDomain
  CASE('trilinear-domain')
    BuildPhysicalDomain3D => BuildPhysicalDomain3D_TrilinearDomain
  CASE('curved-domain')
    BuildPhysicalDomain3D => BuildPhysicalDomain3D_CurvedDomain
  CASE DEFAULT
    ErrorMessage = "Mesh Type for BuildPhysicalDomain3D not implemented"
    CALL PrintError(__STAMP__,ErrorMessage,ModuleName,FunctionName)
END SELECT

SELECT CASE(LowerCase(ParametersMeshBuiltIn3D%WhichMapping3D))
  CASE('straight-hexa')
    Mapping3D => Mapping3D_StraightHexa
  CASE('full-cylinder')
    Mapping3D => Mapping3D_FullCylinder
  CASE('half-cylinder')
    Mapping3D => Mapping3D_HalfCylinder
  CASE DEFAULT
    ErrorMessage = "Analytical Mapping not implemented"
    CALL PrintError(__STAMP__,ErrorMessage,ModuleName,FunctionName)
END SELECT

SELECT CASE(LowerCase(ParametersMeshBuiltIn3D%WhichMeshStretching))
  CASE('uniform')
    MeshStretching3D => MeshStretching3D_Uniform
  CASE('geometric-progression')
    MeshStretching3D => MeshStretching3D_GeometricProgression
  CASE('three-boundaries-hyperbolic')
    MeshStretching3D => MeshStretching3D_ThreeSidesBoundariesHyperbolic
  CASE('three-boundaries-potential')
    MeshStretching3D => MeshStretching3D_ThreeSidesBoundariesPotential
  CASE DEFAULT
    ErrorMessage = "Mesh Type for BuildPhysicalDomain not implemented"
    CALL PrintError(__STAMP__,ErrorMessage,ModuleName,FunctionName)
END SELECT
IF (ParametersMeshBuiltIn3D%StretchMesh .EQV. .TRUE.) THEN
  ParametersMeshBuiltIn3D%MeshStretchingFactor3D = GetRealArray('MeshStretchingFactor3D',3,'1.0,1.0,1.0')
END IF

ALLOCATE(VDM_CGLNGeo_OutputN(0:PP_N,0:PP_NGeo))
SELECT CASE(LowerCase(ParametersMeshBuiltIn3D%WhichOutputBasis))
  CASE('chebyshev-gauss')
    VDM_CGLNGeo_OutputN = VDM_CGLNGeo_CGN
  CASE('chebyshev-gauss-lobatto')
    VDM_CGLNGeo_OutputN = VDM_CGLNGeo_CGLN
  CASE('legendre-gauss')
    VDM_CGLNGeo_OutputN = VDM_CGLNGeo_LGN
  CASE('legendre-gauss-lobatto')
    VDM_CGLNGeo_OutputN = VDM_CGLNGeo_LGLN
  CASE('uniform')
    VDM_CGLNGeo_OutputN = VDM_CGLNGeo_UniformN
  CASE DEFAULT
    ErrorMessage = "Mesh Output Basis not implemented"
    CALL PrintError(__STAMP__,ErrorMessage,ModuleName,FunctionName)
END SELECT

! Mesh Variables
RotationAngle3D = ParametersMeshBuiltIn3D%RotationAngle3D
nBoxElems3D     = ParametersMeshBuiltIn3D%nBoxElems3D
BoxCorner3D     = ParametersMeshBuiltIn3D%BoxCorner3D
PP_nElems       = nBoxElems3D(1)*nBoxElems3D(2)*nBoxElems3D(3)
PP_nNodes       = PP_nElems*(PP_NGeo+1)**3

ALLOCATE(RotationMatrix3D(1:3,1:3,1:3))
ALLOCATE(CGL_NGeo_Domain3D(1:3,0:PP_NGeo,0:PP_NGeo,0:PP_NGeo,1:PP_nElems))
ALLOCATE(MeshData_ElementsToNodesCoordinates3D(1:3,0:PP_N,0:PP_N,0:PP_N,1:PP_nElems))

RotationAngle3D(1:3) = ACOS(-1.0)*RotationAngle3D/180.0
RotationMatrix3D = 0.0

RotationMatrix3D(1,1,1) = 1.0
RotationMatrix3D(2,2,1) = COS(RotationAngle3D(1))
RotationMatrix3D(2,3,1) =-SIN(RotationAngle3D(1))
RotationMatrix3D(3,2,1) = SIN(RotationAngle3D(1))
RotationMatrix3D(3,3,1) = COS(RotationAngle3D(1))

RotationMatrix3D(2,2,2) = 1.0
RotationMatrix3D(1,1,2) = COS(RotationAngle3D(2))
RotationMatrix3D(1,3,2) = SIN(RotationAngle3D(2))
RotationMatrix3D(3,1,2) =-SIN(RotationAngle3D(2))
RotationMatrix3D(3,3,2) = COS(RotationAngle3D(2))

RotationMatrix3D(3,3,3) = 1.0
RotationMatrix3D(1,1,3) = COS(RotationAngle3D(3))
RotationMatrix3D(1,2,3) =-SIN(RotationAngle3D(3))
RotationMatrix3D(2,1,3) = SIN(RotationAngle3D(3))
RotationMatrix3D(2,2,3) = COS(RotationAngle3D(3))

!----------------------------------------------------------------------------------------------------------------------!
END SUBROUTINE InitializeMeshBuiltIn3D
!======================================================================================================================!
!
!
!
!======================================================================================================================!
SUBROUTINE MeshBuiltIn()
!----------------------------------------------------------------------------------------------------------------------!
IMPLICIT NONE
!----------------------------------------------------------------------------------------------------------------------!
! FORMAL ARGUMENTS
!----------------------------------------------------------------------------------------------------------------------!
! LOCAL VARIABLES
!----------------------------------------------------------------------------------------------------------------------!
CHARACTER(LEN=256) :: ErrorMessage
CHARACTER(LEN=256) :: FunctionName = "MeshBuiltIn"
!----------------------------------------------------------------------------------------------------------------------!

SELECT CASE(PP_nDims)
  CASE(2)
    CALL MeshBuiltIn2D()
  CASE(3)
    CALL MeshBuiltIn3D()
  CASE DEFAULT
    ErrorMessage = "Invalid value for MeshDimension"
    CALL PrintError(__STAMP__,ErrorMessage,ModuleName,FunctionName)
END SELECT

!----------------------------------------------------------------------------------------------------------------------!
END SUBROUTINE MeshBuiltIn
!======================================================================================================================!
!
!
!
!======================================================================================================================!
SUBROUTINE MeshBuiltIn2D()
!----------------------------------------------------------------------------------------------------------------------!
USE MOD_MeshMain_vars,ONLY: PP_NGeo
USE MOD_MeshMain_vars,ONLY: PP_nElems
USE MOD_MeshMain_vars,ONLY: PP_nNodes
USE MOD_MeshMain_vars,ONLY: MeshInfo
USE MOD_MeshMain_vars,ONLY: MeshData_ElementsToElementType
USE MOD_MeshMain_vars,ONLY: MeshData_ElementsToNodesCoordinates2D
!----------------------------------------------------------------------------------------------------------------------!
USE MOD_MeshBuiltIn_vars,ONLY: BoxCorner2D
USE MOD_MeshBuiltIn_vars,ONLY: nBoxElems2D
USE MOD_MeshBuiltIn_vars,ONLY: CGL_xNodes_NGeo
USE MOD_MeshBuiltIn_vars,ONLY: CGL_NGeo_Domain2D
USE MOD_MeshBuiltIn_vars,ONLY: BuildPhysicalDomain2D
!----------------------------------------------------------------------------------------------------------------------!
USE MOD_Mesh_CGNS_Definitions, ONLY: ELEMTYPE_QUAD4
!----------------------------------------------------------------------------------------------------------------------!
IMPLICIT NONE
!----------------------------------------------------------------------------------------------------------------------!
! FORMAL ARGUMENTS
!----------------------------------------------------------------------------------------------------------------------!
! LOCAL VARIABLES
!----------------------------------------------------------------------------------------------------------------------!
INTEGER :: nElems
INTEGER :: nDataVars
INTEGER :: nOutputVars
!----------------------------------------------------------------------------------------------------------------------!
CHARACTER(LEN=256),ALLOCATABLE :: CoordNames(:)
CHARACTER(LEN=256),ALLOCATABLE :: DataNames(:)
CHARACTER(LEN=256),ALLOCATABLE :: OutputVars(:)
!----------------------------------------------------------------------------------------------------------------------!

CALL BuildReferenceDomain2D(&
  Nin       = PP_NGeo,&
  nElems    = PP_nElems,&
  nBoxElems = nBoxElems2D,&
  XRefDom1D = CGL_xNodes_NGeo,&
  XRefDom2D = CGL_NGeo_Domain2D)

CALL BuildPhysicalDomain2D(&
  Nin        = PP_NGeo,&
  nElems     = PP_nElems,&
  nBoxElems  = nBoxElems2D,&
  BoxCorners = BoxCorner2D,&
  XRefDom2D  = CGL_NGeo_Domain2D,&
  XPhysDom2D = MeshData_ElementsToNodesCoordinates2D)

IF (ALLOCATED(CGL_NGeo_Domain2D) .EQV. .TRUE.) THEN
  DEALLOCATE(CGL_NGeo_Domain2D)
END IF

!--------------------------------------------------!
! Setting ElementsType
!--------------------------------------------------!
nElems = SIZE(MeshData_ElementsToNodesCoordinates2D,4)
IF (ALLOCATED(MeshData_ElementsToElementType) .EQV. .TRUE.) THEN
  DEALLOCATE(MeshData_ElementsToElementType)
END IF
ALLOCATE(MeshData_ElementsToElementType(1:nElems))

MeshData_ElementsToElementType(1:nElems) = ELEMTYPE_QUAD4

IF (ALLOCATED(CoordNames) .EQV. .TRUE.) THEN
  DEALLOCATE(CoordNames)
END IF
ALLOCATE(CoordNames(1:PP_nDims))
CoordNames(1) = "CoordinateX"
CoordNames(2) = "CoordinateY"

nDataVars   = 2
IF (ALLOCATED(DataNames) .EQV. .TRUE.) THEN
  DEALLOCATE(DataNames)
END IF
ALLOCATE(DataNames(1:nDataVars))
DataNames(1)  = "Level"
DataNames(2)  = "Flag"

nOutputVars = PP_nDims+nDataVars
IF (ALLOCATED(OutputVars) .EQV. .TRUE.) THEN
  DEALLOCATE(OutputVars)
END IF
ALLOCATE(OutputVars(1:nOutputVars))

OutputVars(1:PP_nDims)             = CoordNames(1:PP_nDims)
OutputVars(PP_nDims+1:nOutputVars) = DataNames(1:nDataVars)

IF (ALLOCATED(MeshInfo%CoordNames) .EQV. .TRUE.) THEN
  DEALLOCATE(MeshInfo%CoordNames)
END IF
ALLOCATE(MeshInfo%CoordNames(1:PP_nDims))

IF (ALLOCATED(MeshInfo%DataNames) .EQV. .TRUE.) THEN
  DEALLOCATE(MeshInfo%DataNames)
END IF
ALLOCATE(MeshInfo%DataNames(1:nDataVars))

IF (ALLOCATED(MeshInfo%OutputVars) .EQV. .TRUE.) THEN
  DEALLOCATE(MeshInfo%OutputVars)
END IF
ALLOCATE(MeshInfo%OutputVars(1:nOutputVars))

! Mesh Info for exporting
MeshInfo%nDims        = PP_nDims
MeshInfo%NGeo         = PP_NGeo
MeshInfo%nElems       = PP_nElems
MeshInfo%nNodes       = PP_nNodes
MeshInfo%nOutVars     = nOutputVars
MeshInfo%MaxRefLevel  = 0
MeshInfo%DataNames    = DataNames
MeshInfo%CoordNames   = CoordNames
MeshInfo%OutputVars   = OutputVars
MeshInfo%FileVersion  = TRIM(FileVersion)
MeshInfo%ProgramName  = TRIM(ProgramName)
MeshInfo%ProjectName  = TRIM(ProjectName)
MeshInfo%BaseFileName = TRIM(ProjectName)//"_MESH"

!----------------------------------------------------------------------------------------------------------------------!
END SUBROUTINE MeshBuiltIn2D
!======================================================================================================================!
!
!
!
!======================================================================================================================!
SUBROUTINE MeshBuiltIn3D()
!----------------------------------------------------------------------------------------------------------------------!
USE MOD_MeshMain_vars,ONLY: PP_NGeo
USE MOD_MeshMain_vars,ONLY: PP_nElems
USE MOD_MeshMain_vars,ONLY: PP_nNodes
USE MOD_MeshMain_vars,ONLY: MeshInfo
USE MOD_MeshMain_vars,ONLY: MeshData_ElementsToElementType
USE MOD_MeshMain_vars,ONLY: MeshData_ElementsToNodesCoordinates3D
!----------------------------------------------------------------------------------------------------------------------!
USE MOD_MeshBuiltIn_vars,ONLY: BoxCorner3D
USE MOD_MeshBuiltIn_vars,ONLY: nBoxElems3D
USE MOD_MeshBuiltIn_vars,ONLY: CGL_xNodes_NGeo
USE MOD_MeshBuiltIn_vars,ONLY: CGL_NGeo_Domain3D
USE MOD_MeshBuiltIn_vars,ONLY: BuildPhysicalDomain3D
!----------------------------------------------------------------------------------------------------------------------!
USE MOD_Mesh_CGNS_Definitions, ONLY: ELEMTYPE_HEXA8
!----------------------------------------------------------------------------------------------------------------------!
IMPLICIT NONE
!----------------------------------------------------------------------------------------------------------------------!
! FORMAL ARGUMENTS
!----------------------------------------------------------------------------------------------------------------------!
! LOCAL VARIABLES
!----------------------------------------------------------------------------------------------------------------------!
INTEGER :: nElems
INTEGER :: nDataVars
INTEGER :: nOutputVars
!----------------------------------------------------------------------------------------------------------------------!
CHARACTER(LEN=256),ALLOCATABLE :: CoordNames(:)
CHARACTER(LEN=256),ALLOCATABLE :: DataNames(:)
CHARACTER(LEN=256),ALLOCATABLE :: OutputVars(:)
!----------------------------------------------------------------------------------------------------------------------!

CALL BuildReferenceDomain3D(&
  Nin       = PP_NGeo,&
  nElems    = PP_nElems,&
  nBoxElems = nBoxElems3D,&
  XRefDom1D = CGL_xNodes_NGeo,&
  XRefDom3D = CGL_NGeo_Domain3D)

CALL BuildPhysicalDomain3D(&
  Nin        = PP_NGeo,&
  nElems     = PP_nElems,&
  nBoxElems  = nBoxElems3D,&
  BoxCorners = BoxCorner3D,&
  XRefDom3D  = CGL_NGeo_Domain3D,&
  XPhysDom3D = MeshData_ElementsToNodesCoordinates3D)

IF (ALLOCATED(CGL_NGeo_Domain3D) .EQV. .TRUE.) THEN
  DEALLOCATE(CGL_NGeo_Domain3D)
END IF

!--------------------------------------------------!
! Setting ElementsType
!--------------------------------------------------!
nElems = SIZE(MeshData_ElementsToNodesCoordinates3D,5)
IF (ALLOCATED(MeshData_ElementsToElementType) .EQV. .TRUE.) THEN
  DEALLOCATE(MeshData_ElementsToElementType)
END IF
ALLOCATE(MeshData_ElementsToElementType(1:nElems))

MeshData_ElementsToElementType(1:nElems) = ELEMTYPE_HEXA8

IF (ALLOCATED(CoordNames) .EQV. .TRUE.) THEN
  DEALLOCATE(CoordNames)
END IF
ALLOCATE(CoordNames(1:PP_nDims))
CoordNames(1) = "CoordinateX"
CoordNames(2) = "CoordinateY"
CoordNames(3) = "CoordinateZ"

nDataVars   = 2
IF (ALLOCATED(DataNames) .EQV. .TRUE.) THEN
  DEALLOCATE(DataNames)
END IF
ALLOCATE(DataNames(1:nDataVars))
DataNames(1)  = "Level"
DataNames(2)  = "Flag"

nOutputVars = PP_nDims+nDataVars
IF (ALLOCATED(OutputVars) .EQV. .TRUE.) THEN
  DEALLOCATE(OutputVars)
END IF
ALLOCATE(OutputVars(1:nOutputVars))

OutputVars(1:PP_nDims)             = CoordNames(1:PP_nDims)
OutputVars(PP_nDims+1:nOutputVars) = DataNames(1:nDataVars)

IF (ALLOCATED(MeshInfo%CoordNames) .EQV. .TRUE.) THEN
  DEALLOCATE(MeshInfo%CoordNames)
END IF
ALLOCATE(MeshInfo%CoordNames(1:PP_nDims))

IF (ALLOCATED(MeshInfo%DataNames) .EQV. .TRUE.) THEN
  DEALLOCATE(MeshInfo%DataNames)
END IF
ALLOCATE(MeshInfo%DataNames(1:nDataVars))

IF (ALLOCATED(MeshInfo%OutputVars) .EQV. .TRUE.) THEN
  DEALLOCATE(MeshInfo%OutputVars)
END IF
ALLOCATE(MeshInfo%OutputVars(1:nOutputVars))

! Mesh Info for exporting
MeshInfo%nDims        = PP_nDims
MeshInfo%NGeo         = PP_NGeo
MeshInfo%nElems       = PP_nElems
MeshInfo%nNodes       = PP_nNodes
MeshInfo%nOutVars     = nOutputVars
MeshInfo%MaxRefLevel  = 0
MeshInfo%DataNames    = DataNames
MeshInfo%CoordNames   = CoordNames
MeshInfo%OutputVars   = OutputVars
MeshInfo%FileVersion  = TRIM(FileVersion)
MeshInfo%ProgramName  = TRIM(ProgramName)
MeshInfo%ProjectName  = TRIM(ProjectName)
MeshInfo%BaseFileName = TRIM(ProjectName)//"_MESH"

!----------------------------------------------------------------------------------------------------------------------!
END SUBROUTINE MeshBuiltIn3D
!======================================================================================================================!
!
!
!
!======================================================================================================================!
SUBROUTINE BuildReferenceDomain2D(Nin,nElems,nBoxElems,XRefDom1D,XRefDom2D)
!----------------------------------------------------------------------------------------------------------------------!
USE MOD_MeshBuiltIn_vars,ONLY: MeshStretching2D
!----------------------------------------------------------------------------------------------------------------------!
IMPLICIT NONE
!----------------------------------------------------------------------------------------------------------------------!
! FORMAL ARGUMENTS
!----------------------------------------------------------------------------------------------------------------------!
INTEGER,INTENT(IN) :: Nin
INTEGER,INTENT(IN) :: nElems
INTEGER,INTENT(IN) :: nBoxElems(1:2)
REAL,INTENT(IN)    :: XRefDom1D(0:Nin)
REAL,INTENT(OUT)   :: XRefDom2D(1:2,0:Nin,0:Nin,1:nElems)
!----------------------------------------------------------------------------------------------------------------------!
! LOCAL VARIABLES
!----------------------------------------------------------------------------------------------------------------------!
INTEGER :: i, j
INTEGER :: ii, jj
INTEGER :: iElem
REAL    :: x1(1:2)
REAL    :: dx(1:nBoxElems(1))
REAL    :: dy(1:nBoxElems(2))
!----------------------------------------------------------------------------------------------------------------------!

CALL MeshStretching2D(nBoxElems,dx,dy)

iElem = 0
x1(2) = -1.0
DO jj=1,nBoxElems(2)
  x1(1) = -1.0
  DO ii=1,nBoxElems(1)
    iElem = iElem + 1
    DO j=0,Nin
      DO i=0,Nin
        XRefDom2D(1,i,j,iElem) = x1(1) + 0.5*(1.0+XRefDom1D(i))*dx(ii)
        XRefDom2D(2,i,j,iElem) = x1(2) + 0.5*(1.0+XRefDom1D(j))*dy(jj)
      END DO
    END DO
    x1(1) = x1(1) + dx(ii)
  END DO
  x1(2) = x1(2) + dy(jj)
END DO

!----------------------------------------------------------------------------------------------------------------------!
END SUBROUTINE BuildReferenceDomain2D
!======================================================================================================================!
!
!
!
!======================================================================================================================!
SUBROUTINE BuildReferenceDomain3D(Nin,nElems,nBoxElems,XRefDom1D,XRefDom3D)
!----------------------------------------------------------------------------------------------------------------------!
USE MOD_MeshBuiltIn_vars,ONLY: MeshStretching3D
!----------------------------------------------------------------------------------------------------------------------!
IMPLICIT NONE
!----------------------------------------------------------------------------------------------------------------------!
! FORMAL ARGUMENTS
!----------------------------------------------------------------------------------------------------------------------!
INTEGER,INTENT(IN) :: Nin
INTEGER,INTENT(IN) :: nElems
INTEGER,INTENT(IN) :: nBoxElems(1:3)
REAL,INTENT(IN)    :: XRefDom1D(0:Nin)
REAL,INTENT(OUT)   :: XRefDom3D(1:3,0:Nin,0:Nin,0:Nin,1:nElems)
!----------------------------------------------------------------------------------------------------------------------!
! LOCAL VARIABLES
!----------------------------------------------------------------------------------------------------------------------!
INTEGER :: i, j, k
INTEGER :: ii, jj, kk
INTEGER :: iElem
REAL    :: x1(1:3)
REAL    :: dx(1:nBoxElems(1))
REAL    :: dy(1:nBoxElems(2))
REAL    :: dz(1:nBoxElems(3))
!----------------------------------------------------------------------------------------------------------------------!

CALL MeshStretching3D(nBoxElems,dx,dy,dz)

iElem = 0
x1(3) = -1.0
DO kk=1,nBoxElems(3)
  x1(2) = -1.0
  DO jj=1,nBoxElems(2)
    x1(1) = -1.0
    DO ii=1,nBoxElems(1)
      iElem = iElem + 1
      DO k=0,Nin
        DO j=0,Nin
          DO i=0,Nin
            XRefDom3D(1,i,j,k,iElem) = x1(1) + 0.5*(1.0+XRefDom1D(i))*dx(ii)
            XRefDom3D(2,i,j,k,iElem) = x1(2) + 0.5*(1.0+XRefDom1D(j))*dy(jj)
            XRefDom3D(3,i,j,k,iElem) = x1(3) + 0.5*(1.0+XRefDom1D(k))*dz(kk)
          END DO
        END DO
      END DO
      x1(1) = x1(1) + dx(ii)
    END DO
    x1(2) = x1(2) + dy(jj)
  END DO
  x1(3) = x1(3) + dz(kk)
END DO

!----------------------------------------------------------------------------------------------------------------------!
END SUBROUTINE BuildReferenceDomain3D
!======================================================================================================================!
!
!
!
!======================================================================================================================!
SUBROUTINE BuildPhysicalDomain2D_CartesianDomain(Nin,nElems,nBoxElems,BoxCorners,XRefDom2D,XPhysDom2D)
!----------------------------------------------------------------------------------------------------------------------!
USE MOD_MeshMain_vars,ONLY: PP_N
USE MOD_MeshMain_vars,ONLY: PP_NGeo
!----------------------------------------------------------------------------------------------------------------------!
USE MOD_MeshBuiltIn_vars,ONLY: VDM_CGLNGeo_OutputN
USE MOD_MeshBuiltIn_vars,ONLY: Mapping2D
USE MOD_MeshBuiltIn_vars,ONLY: RotationMatrix2D
!----------------------------------------------------------------------------------------------------------------------!
USE MOD_NumericsTools,ONLY: InterpolateToNewPoints2D
!----------------------------------------------------------------------------------------------------------------------!
IMPLICIT NONE
!----------------------------------------------------------------------------------------------------------------------!
! FORMAL ARGUMENTS
!----------------------------------------------------------------------------------------------------------------------!
INTEGER,INTENT(IN) :: Nin
INTEGER,INTENT(IN) :: nElems
INTEGER,INTENT(IN) :: nBoxElems(1:2)
REAL,INTENT(IN)    :: BoxCorners(1:4,1:2)
REAL,INTENT(IN)    :: XRefDom2D(1:2,0:Nin,0:Nin,1:nElems)
REAL,INTENT(OUT)   :: XPhysDom2D(1:2,0:Nin,0:Nin,1:nElems)
!----------------------------------------------------------------------------------------------------------------------!
! LOCAL VARIABLES
!----------------------------------------------------------------------------------------------------------------------!
INTEGER :: iElem
INTEGER :: i, j
INTEGER :: ii, jj
REAL    :: Lx, Ly
REAL    :: r, s
REAL    :: xc(1:4,1:2)
!----------------------------------------------------------------------------------------------------------------------!

xc(1:4,1:2) = BoxCorners(1:4,1:2)

Lx = ABS(BoxCorners(3,1)-BoxCorners(1,1))
Ly = ABS(BoxCorners(3,2)-BoxCorners(1,2))

iElem = 0
DO jj=1,nBoxElems(2)
  DO ii=1,nBoxElems(1)
    iElem = iElem + 1
    DO j=0,Nin
      DO i=0,Nin
        r = XRefDom2D(1,i,j,iElem)
        s = XRefDom2D(2,i,j,iElem)
        XPhysDom2D(1,i,j,iElem) = BoxCorners(1,1) + 0.5*(1.0+r)*Lx
        XPhysDom2D(2,i,j,iElem) = BoxCorners(1,2) + 0.5*(1.0+s)*Ly
        CALL DeformMesh2D(xc,XPhysDom2D(1:2,i,j,iElem))
        CALL RotateMesh2D(RotationMatrix2D,XPhysDom2D(1:2,i,j,iElem))
      END DO
    END DO
    CALL InterpolateToNewPoints2D(&
      PP_nDims,PP_NGeo,PP_N,VDM_CGLNGeo_OutputN,XPhysDom2D(:,:,:,iElem),XPhysDom2D(:,:,:,iElem))
  END DO
END DO

!----------------------------------------------------------------------------------------------------------------------!
END SUBROUTINE BuildPhysicalDomain2D_CartesianDomain
!======================================================================================================================!
!
!
!
!======================================================================================================================!
SUBROUTINE BuildPhysicalDomain2D_BilinearDomain(Nin,nElems,nBoxElems,BoxCorners,XRefDom2D,XPhysDom2D)
!----------------------------------------------------------------------------------------------------------------------!
USE MOD_MeshMain_vars,ONLY: PP_N
USE MOD_MeshMain_vars,ONLY: PP_NGeo
!----------------------------------------------------------------------------------------------------------------------!
USE MOD_MeshBuiltIn_vars,ONLY: VDM_CGLNGeo_OutputN
USE MOD_MeshBuiltIn_vars,ONLY: Mapping2D
USE MOD_MeshBuiltIn_vars,ONLY: RotationMatrix2D
!----------------------------------------------------------------------------------------------------------------------!
USE MOD_NumericsTools,ONLY: InterpolateToNewPoints2D
!----------------------------------------------------------------------------------------------------------------------!
IMPLICIT NONE
!----------------------------------------------------------------------------------------------------------------------!
! FORMAL ARGUMENTS
!----------------------------------------------------------------------------------------------------------------------!
INTEGER,INTENT(IN) :: Nin
INTEGER,INTENT(IN) :: nElems
INTEGER,INTENT(IN) :: nBoxElems(1:2)
REAL,INTENT(IN)    :: BoxCorners(1:4,1:2)
REAL,INTENT(IN)    :: XRefDom2D(1:2,0:Nin,0:Nin,1:nElems)
REAL,INTENT(OUT)   :: XPhysDom2D(1:2,0:Nin,0:Nin,1:nElems)
!----------------------------------------------------------------------------------------------------------------------!
! LOCAL VARIABLES
!----------------------------------------------------------------------------------------------------------------------!
INTEGER :: iElem
INTEGER :: i, j
INTEGER :: ii, jj
REAL    :: r, s
REAL    :: xc(1:4,1:2)
!----------------------------------------------------------------------------------------------------------------------!

xc(1:4,1:2) = BoxCorners(1:4,1:2)

iElem = 0
DO jj=1,nBoxElems(2)
  DO ii=1,nBoxElems(1)
    iElem = iElem + 1
    DO j=0,Nin
      DO i=0,Nin
        r = XRefDom2D(1,i,j,iElem)
        s = XRefDom2D(2,i,j,iElem)
        XPhysDom2D(1:2,i,j,iElem) = 0.25*((1.0-r)*(1.0-s)*xc(1,1:2) + (1.0+r)*(1.0-s)*xc(2,1:2) + &
                                          (1.0+r)*(1.0+s)*xc(3,1:2) + (1.0-r)*(1.0+s)*xc(4,1:2))
        CALL DeformMesh2D(xc,XPhysDom2D(1:2,i,j,iElem))
        CALL RotateMesh2D(RotationMatrix2D,XPhysDom2D(1:2,i,j,iElem))
      END DO
    END DO
    CALL InterpolateToNewPoints2D(&
      PP_nDims,PP_NGeo,PP_N,VDM_CGLNGeo_OutputN,XPhysDom2D(:,:,:,iElem),XPhysDom2D(:,:,:,iElem))
  END DO
END DO

!----------------------------------------------------------------------------------------------------------------------!
END SUBROUTINE BuildPhysicalDomain2D_BilinearDomain
!======================================================================================================================!
!
!
!
!======================================================================================================================!
SUBROUTINE BuildPhysicalDomain2D_CurvedDomain(Nin,nElems,nBoxElems,BoxCorners,XRefDom2D,XPhysDom2D)
!----------------------------------------------------------------------------------------------------------------------!
USE MOD_MeshMain_vars,ONLY: PP_N
USE MOD_MeshMain_vars,ONLY: PP_NGeo
!----------------------------------------------------------------------------------------------------------------------!
USE MOD_MeshBuiltIn_vars,ONLY: VDM_CGLNGeo_OutputN
USE MOD_MeshBuiltIn_vars,ONLY: Mapping2D
USE MOD_MeshBuiltIn_vars,ONLY: RotationMatrix2D
!----------------------------------------------------------------------------------------------------------------------!
USE MOD_NumericsTools,ONLY: InterpolateToNewPoints2D
!----------------------------------------------------------------------------------------------------------------------!
IMPLICIT NONE
!----------------------------------------------------------------------------------------------------------------------!
! FORMAL ARGUMENTS
!----------------------------------------------------------------------------------------------------------------------!
INTEGER,INTENT(IN) :: Nin
INTEGER,INTENT(IN) :: nElems
INTEGER,INTENT(IN) :: nBoxElems(1:2)
REAL,INTENT(IN)    :: BoxCorners(1:4,1:2)
REAL,INTENT(IN)    :: XRefDom2D(1:2,0:Nin,0:Nin,1:nElems)
REAL,INTENT(OUT)   :: XPhysDom2D(1:2,0:Nin,0:Nin,1:nElems)
!----------------------------------------------------------------------------------------------------------------------!
! LOCAL VARIABLES
!----------------------------------------------------------------------------------------------------------------------!
INTEGER :: iElem
INTEGER :: i, j
INTEGER :: ii, jj
REAL    :: r, s
REAL    :: xc(1:4,1:2)
REAL    :: xp(1:4,1:2)
!----------------------------------------------------------------------------------------------------------------------!

!-----------------------------------------------------!
! CGNS notation of the Hexahedron:
!-----------------------------------------------------!
!      P8------P7
!     /|      /|
!    P5------P6|
!    | P4----|-P3
!    |/      |/
!    P1------P2
!-----------------------------------------------------!
! Faces                 Edges            Edges
! F1  -> P1,P4,P3,P2    E1  -> P1,P2     E7  -> P3,P7
! F2  -> P1,P2,P6,P5    E2  -> P2,P3     E8  -> P4,P8
! F3  -> P2,P3,P7,P6    E3  -> P3,P4     E9  -> P5,P6
! F4  -> P3,P4,P8,P7    E4  -> P4,P1     E10 -> P6,P7
! F5  -> P1,P5,P8,P4    E5  -> P1,P5     E11 -> P7,P8
! F6  -> P5,P6,P7,P8    E6  -> P2,P6     E12 -> P8,P5
!-----------------------------------------------------!

CALL Mapping2D(1,-1.0,BoxCorners(1:4,1:2),xc(1,1:2))
CALL Mapping2D(2,-1.0,BoxCorners(1:4,1:2),xc(2,1:2))
CALL Mapping2D(3,+1.0,BoxCorners(1:4,1:2),xc(3,1:2))
CALL Mapping2D(4,+1.0,BoxCorners(1:4,1:2),xc(4,1:2))

iElem = 0
DO jj=1,nBoxElems(2)
  DO ii=1,nBoxElems(1)
    iElem = iElem + 1
    DO j=0,Nin
      DO i=0,Nin
        r = XRefDom2D(1,i,j,iElem)
        s = XRefDom2D(2,i,j,iElem)
        CALL Mapping2D(1,r,xc(1:4,1:2),xp(1,1:2))
        CALL Mapping2D(2,s,xc(1:4,1:2),xp(2,1:2))
        CALL Mapping2D(3,r,xc(1:4,1:2),xp(3,1:2))
        CALL Mapping2D(4,s,xc(1:4,1:2),xp(4,1:2))
        XPhysDom2D(1:2,i,j,iElem) = 0.50*(         (1.0-r)*xp(4,1:2) + (1.0+r)*xp(2,1:2)  + &
                                                   (1.0-s)*xp(1,1:2) + (1.0+s)*xp(3,1:2)) - &
                                    0.25*((1.0-r)*((1.0-s)*xc(1,1:2) + (1.0+s)*xc(4,1:2)) + &
                                          (1.0+r)*((1.0-s)*xc(2,1:2) + (1.0+s)*xc(3,1:2)))
        CALL DeformMesh2D(xc,XPhysDom2D(1:2,i,j,iElem))
        CALL RotateMesh2D(RotationMatrix2D,XPhysDom2D(1:2,i,j,iElem))
      END DO
    END DO
    CALL InterpolateToNewPoints2D(&
      PP_nDims,PP_NGeo,PP_N,VDM_CGLNGeo_OutputN,XPhysDom2D(:,:,:,iElem),XPhysDom2D(:,:,:,iElem))
  END DO
END DO

!----------------------------------------------------------------------------------------------------------------------!
END SUBROUTINE BuildPhysicalDomain2D_CurvedDomain
!======================================================================================================================!
!
!
!
!======================================================================================================================!
SUBROUTINE BuildPhysicalDomain3D_CartesianDomain(Nin,nElems,nBoxElems,BoxCorners,XRefDom3D,XPhysDom3D)
!----------------------------------------------------------------------------------------------------------------------!
USE MOD_MeshMain_vars,ONLY: PP_N
USE MOD_MeshMain_vars,ONLY: PP_NGeo
!----------------------------------------------------------------------------------------------------------------------!
USE MOD_MeshBuiltIn_vars,ONLY: VDM_CGLNGeo_OutputN
USE MOD_MeshBuiltIn_vars,ONLY: Mapping3D
USE MOD_MeshBuiltIn_vars,ONLY: RotationMatrix3D
!----------------------------------------------------------------------------------------------------------------------!
USE MOD_NumericsTools,ONLY: InterpolateToNewPoints3D
!----------------------------------------------------------------------------------------------------------------------!
IMPLICIT NONE
!----------------------------------------------------------------------------------------------------------------------!
! FORMAL ARGUMENTS
!----------------------------------------------------------------------------------------------------------------------!
INTEGER,INTENT(IN) :: Nin
INTEGER,INTENT(IN) :: nElems
INTEGER,INTENT(IN) :: nBoxElems(1:3)
REAL,INTENT(IN)    :: BoxCorners(1:8,1:3)
REAL,INTENT(IN)    :: XRefDom3D(1:3,0:Nin,0:Nin,0:Nin,1:nElems)
REAL,INTENT(OUT)   :: XPhysDom3D(1:3,0:Nin,0:Nin,0:Nin,1:nElems)
!----------------------------------------------------------------------------------------------------------------------!
! LOCAL VARIABLES
!----------------------------------------------------------------------------------------------------------------------!
INTEGER :: iElem
INTEGER :: i, j, k
INTEGER :: ii, jj, kk
REAL    :: Lx, Ly, Lz
REAL    :: r, s, t
REAL    :: xc(1:8,1:3)
!----------------------------------------------------------------------------------------------------------------------!

xc(1:8,1:3) = BoxCorners(1:8,1:3)

Lx = ABS(BoxCorners(2,1)-BoxCorners(1,1))
Ly = ABS(BoxCorners(4,2)-BoxCorners(1,2))
Lz = ABS(BoxCorners(5,3)-BoxCorners(1,3))

iElem = 0
DO kk=1,nBoxElems(3)
  DO jj=1,nBoxElems(2)
    DO ii=1,nBoxElems(1)
      iElem = iElem + 1
      DO k=0,Nin
        DO j=0,Nin
          DO i=0,Nin
            r = XRefDom3D(1,i,j,k,iElem)
            s = XRefDom3D(2,i,j,k,iElem)
            t = XRefDom3D(3,i,j,k,iElem)
            XPhysDom3D(1,i,j,k,iElem) = BoxCorners(1,1) + 0.5*(1.0+r)*Lx
            XPhysDom3D(2,i,j,k,iElem) = BoxCorners(1,2) + 0.5*(1.0+s)*Ly
            XPhysDom3D(3,i,j,k,iElem) = BoxCorners(1,3) + 0.5*(1.0+t)*Lz
            CALL DeformMesh3D(xc,XPhysDom3D(1:3,i,j,k,iElem))
            CALL RotateMesh3D(RotationMatrix3D,XPhysDom3D(1:3,i,j,k,iElem))
          END DO
        END DO
      END DO
      CALL InterpolateToNewPoints3D(&
        PP_nDims,PP_NGeo,PP_N,VDM_CGLNGeo_OutputN,XPhysDom3D(:,:,:,:,iElem),XPhysDom3D(:,:,:,:,iElem))
    END DO
  END DO
END DO

!----------------------------------------------------------------------------------------------------------------------!
END SUBROUTINE BuildPhysicalDomain3D_CartesianDomain
!======================================================================================================================!
!
!
!
!======================================================================================================================!
SUBROUTINE BuildPhysicalDomain3D_TrilinearDomain(Nin,nElems,nBoxElems,BoxCorners,XRefDom3D,XPhysDom3D)
!----------------------------------------------------------------------------------------------------------------------!
USE MOD_MeshMain_vars,ONLY: PP_N
USE MOD_MeshMain_vars,ONLY: PP_NGeo
!----------------------------------------------------------------------------------------------------------------------!
USE MOD_MeshBuiltIn_vars,ONLY: VDM_CGLNGeo_OutputN
USE MOD_MeshBuiltIn_vars,ONLY: Mapping3D
USE MOD_MeshBuiltIn_vars,ONLY: RotationMatrix3D
!----------------------------------------------------------------------------------------------------------------------!
USE MOD_NumericsTools,ONLY: InterpolateToNewPoints3D
!----------------------------------------------------------------------------------------------------------------------!
IMPLICIT NONE
!----------------------------------------------------------------------------------------------------------------------!
! FORMAL ARGUMENTS
!----------------------------------------------------------------------------------------------------------------------!
INTEGER,INTENT(IN) :: Nin
INTEGER,INTENT(IN) :: nElems
INTEGER,INTENT(IN) :: nBoxElems(1:3)
REAL,INTENT(IN)    :: BoxCorners(1:8,1:3)
REAL,INTENT(IN)    :: XRefDom3D(1:3,0:Nin,0:Nin,0:Nin,1:nElems)
REAL,INTENT(OUT)   :: XPhysDom3D(1:3,0:Nin,0:Nin,0:Nin,1:nElems)
!----------------------------------------------------------------------------------------------------------------------!
! LOCAL VARIABLES
!----------------------------------------------------------------------------------------------------------------------!
INTEGER :: iElem
INTEGER :: i, j, k
INTEGER :: ii, jj, kk
REAL    :: r, s, t
REAL    :: xc(1:8,1:3)
!----------------------------------------------------------------------------------------------------------------------!

xc(1:8,1:3) = BoxCorners(1:8,1:3)

iElem = 0
DO kk=1,nBoxElems(3)
  DO jj=1,nBoxElems(2)
    DO ii=1,nBoxElems(1)
      iElem = iElem + 1
      DO k=0,Nin
        DO j=0,Nin
          DO i=0,Nin
            r = XRefDom3D(1,i,j,k,iElem)
            s = XRefDom3D(2,i,j,k,iElem)
            t = XRefDom3D(3,i,j,k,iElem)
            XPhysDom3D(1:3,i,j,k,iElem) = 0.125*( &
              (1.0-r)*(1.0-s)*(1.0-t)*xc(1,1:3) + &
              (1.0+r)*(1.0-s)*(1.0-t)*xc(2,1:3) + &
              (1.0+r)*(1.0+s)*(1.0-t)*xc(3,1:3) + &
              (1.0-r)*(1.0+s)*(1.0-t)*xc(4,1:3) + &
              (1.0-r)*(1.0-s)*(1.0+t)*xc(5,1:3) + &
              (1.0+r)*(1.0-s)*(1.0+t)*xc(6,1:3) + &
              (1.0+r)*(1.0+s)*(1.0+t)*xc(7,1:3) + &
              (1.0-r)*(1.0+s)*(1.0+t)*xc(8,1:3) )
            CALL DeformMesh3D(xc,XPhysDom3D(1:3,i,j,k,iElem))
            CALL RotateMesh3D(RotationMatrix3D,XPhysDom3D(1:3,i,j,k,iElem))
          END DO
        END DO
      END DO
      CALL InterpolateToNewPoints3D(&
        PP_nDims,PP_NGeo,PP_N,VDM_CGLNGeo_OutputN,XPhysDom3D(:,:,:,:,iElem),XPhysDom3D(:,:,:,:,iElem))
    END DO
  END DO
END DO

!----------------------------------------------------------------------------------------------------------------------!
END SUBROUTINE BuildPhysicalDomain3D_TrilinearDomain
!======================================================================================================================!
!
!
!
!======================================================================================================================!
SUBROUTINE BuildPhysicalDomain3D_CurvedDomain(Nin,nElems,nBoxElems,BoxCorners,XRefDom3D,XPhysDom3D)
!----------------------------------------------------------------------------------------------------------------------!
USE MOD_MeshMain_vars,ONLY: PP_N
USE MOD_MeshMain_vars,ONLY: PP_NGeo
!----------------------------------------------------------------------------------------------------------------------!
USE MOD_MeshBuiltIn_vars,ONLY: VDM_CGLNGeo_OutputN
USE MOD_MeshBuiltIn_vars,ONLY: Mapping3D
USE MOD_MeshBuiltIn_vars,ONLY: RotationMatrix3D
!----------------------------------------------------------------------------------------------------------------------!
USE MOD_NumericsTools,ONLY: InterpolateToNewPoints3D
!----------------------------------------------------------------------------------------------------------------------!
IMPLICIT NONE
!----------------------------------------------------------------------------------------------------------------------!
! FORMAL ARGUMENTS
!----------------------------------------------------------------------------------------------------------------------!
INTEGER,INTENT(IN) :: Nin
INTEGER,INTENT(IN) :: nElems
INTEGER,INTENT(IN) :: nBoxElems(1:3)
REAL,INTENT(IN)    :: BoxCorners(1:8,1:3)
REAL,INTENT(IN)    :: XRefDom3D(1:3,0:Nin,0:Nin,0:Nin,1:nElems)
REAL,INTENT(OUT)   :: XPhysDom3D(1:3,0:Nin,0:Nin,0:Nin,1:nElems)
!----------------------------------------------------------------------------------------------------------------------!
! LOCAL VARIABLES
!----------------------------------------------------------------------------------------------------------------------!
INTEGER :: iElem
INTEGER :: i, j, k
INTEGER :: ii, jj, kk
REAL    :: r, s, t
REAL    :: xc(1:8,1:3)
REAL    :: fp(1:8,1:3)
REAL    :: ep(1:12,1:3)
!----------------------------------------------------------------------------------------------------------------------!

!-----------------------------------------------------!
! CGNS notation of the Hexahedron:
!-----------------------------------------------------!
!      P8------P7
!     /|      /|
!    P5------P6|
!    | P4----|-P3
!    |/      |/
!    P1------P2
!-----------------------------------------------------!
! Faces                 Edges            Edges
! F1  -> P1,P4,P3,P2    E1  -> P1,P2     E7  -> P3,P7
! F2  -> P1,P2,P6,P5    E2  -> P2,P3     E8  -> P4,P8
! F3  -> P2,P3,P7,P6    E3  -> P3,P4     E9  -> P5,P6
! F4  -> P3,P4,P8,P7    E4  -> P4,P1     E10 -> P6,P7
! F5  -> P1,P5,P8,P4    E5  -> P1,P5     E11 -> P7,P8
! F6  -> P5,P6,P7,P8    E6  -> P2,P6     E12 -> P8,P5
!-----------------------------------------------------!

CALL Mapping3D(1,(/-1.0,-1.0/),BoxCorners(1:8,1:3),xc(1,1:3))
CALL Mapping3D(1,(/+1.0,-1.0/),BoxCorners(1:8,1:3),xc(2,1:3))
CALL Mapping3D(1,(/+1.0,+1.0/),BoxCorners(1:8,1:3),xc(3,1:3))
CALL Mapping3D(1,(/-1.0,+1.0/),BoxCorners(1:8,1:3),xc(4,1:3))
CALL Mapping3D(6,(/-1.0,-1.0/),BoxCorners(1:8,1:3),xc(5,1:3))
CALL Mapping3D(6,(/+1.0,-1.0/),BoxCorners(1:8,1:3),xc(6,1:3))
CALL Mapping3D(6,(/+1.0,+1.0/),BoxCorners(1:8,1:3),xc(7,1:3))
CALL Mapping3D(6,(/-1.0,+1.0/),BoxCorners(1:8,1:3),xc(8,1:3))

iElem = 0
DO kk=1,nBoxElems(3)
  DO jj=1,nBoxElems(2)
    DO ii=1,nBoxElems(1)
      iElem = iElem + 1
      DO k=0,Nin
        DO j=0,Nin
          DO i=0,Nin
            r = XRefDom3D(1,i,j,k,iElem)
            s = XRefDom3D(2,i,j,k,iElem)
            t = XRefDom3D(3,i,j,k,iElem)
            XPhysDom3D(1:3,i,j,k,iElem) = 0.125*(&
              (1.0-r)*(1.0-s)*(1.0-t)*xc(1,1:3) + &
              (1.0+r)*(1.0-s)*(1.0-t)*xc(2,1:3) + &
              (1.0+r)*(1.0+s)*(1.0-t)*xc(3,1:3) + &
              (1.0-r)*(1.0+s)*(1.0-t)*xc(4,1:3) + &
              (1.0-r)*(1.0-s)*(1.0+t)*xc(5,1:3) + &
              (1.0+r)*(1.0-s)*(1.0+t)*xc(6,1:3) + &
              (1.0+r)*(1.0+s)*(1.0+t)*xc(7,1:3) + &
              (1.0-r)*(1.0+s)*(1.0+t)*xc(8,1:3))
            CALL Mapping3D(1,(/r,s/),BoxCorners(1:8,1:3),fp(1,1:3))
            CALL Mapping3D(2,(/r,t/),BoxCorners(1:8,1:3),fp(2,1:3))
            CALL Mapping3D(3,(/s,t/),BoxCorners(1:8,1:3),fp(3,1:3))
            CALL Mapping3D(4,(/r,t/),BoxCorners(1:8,1:3),fp(4,1:3))
            CALL Mapping3D(5,(/s,t/),BoxCorners(1:8,1:3),fp(5,1:3))
            CALL Mapping3D(6,(/r,s/),BoxCorners(1:8,1:3),fp(6,1:3))
            XPhysDom3D(1:3,i,j,k,iElem) = XPhysDom3D(1:3,i,j,k,iElem) + 0.5*(&
              (1.0-t)*fp(1,1:3) + &
              (1.0-s)*fp(2,1:3) + &
              (1.0+r)*fp(3,1:3) + &
              (1.0+s)*fp(4,1:3) + &
              (1.0-r)*fp(5,1:3) + &
              (1.0+t)*fp(6,1:3))
            CALL Mapping3D(1,(/r,-1.0/),BoxCorners(1:8,1:3),ep( 1,1:3))
            CALL Mapping3D(1,(/+1.0,s/),BoxCorners(1:8,1:3),ep( 2,1:3))
            CALL Mapping3D(1,(/r,+1.0/),BoxCorners(1:8,1:3),ep( 3,1:3))
            CALL Mapping3D(1,(/-1.0,s/),BoxCorners(1:8,1:3),ep( 4,1:3))
            CALL Mapping3D(2,(/-1.0,t/),BoxCorners(1:8,1:3),ep( 5,1:3))
            CALL Mapping3D(2,(/+1.0,t/),BoxCorners(1:8,1:3),ep( 6,1:3))
            CALL Mapping3D(4,(/+1.0,t/),BoxCorners(1:8,1:3),ep( 7,1:3))
            CALL Mapping3D(4,(/-1.0,t/),BoxCorners(1:8,1:3),ep( 8,1:3))
            CALL Mapping3D(6,(/r,-1.0/),BoxCorners(1:8,1:3),ep( 9,1:3))
            CALL Mapping3D(6,(/+1.0,s/),BoxCorners(1:8,1:3),ep(10,1:3))
            CALL Mapping3D(6,(/r,+1.0/),BoxCorners(1:8,1:3),ep(11,1:3))
            CALL Mapping3D(6,(/-1.0,s/),BoxCorners(1:8,1:3),ep(12,1:3))
            XPhysDom3D(1:3,i,j,k,iElem) = XPhysDom3D(1:3,i,j,k,iElem) - 0.25*(&
              (1.0-s)*(1.0-t)*ep( 1,1:3) + &
              (1.0+r)*(1.0-t)*ep( 2,1:3) + &
              (1.0+s)*(1.0-t)*ep( 3,1:3) + &
              (1.0-r)*(1.0-t)*ep( 4,1:3) + &
              (1.0-r)*(1.0-s)*ep( 5,1:3) + &
              (1.0+r)*(1.0-s)*ep( 6,1:3) + &
              (1.0+r)*(1.0+s)*ep( 7,1:3) + &
              (1.0-r)*(1.0+s)*ep( 8,1:3) + &
              (1.0-s)*(1.0+t)*ep( 9,1:3) + &
              (1.0+r)*(1.0+t)*ep(10,1:3) + &
              (1.0+s)*(1.0+t)*ep(11,1:3) + &
              (1.0-r)*(1.0+t)*ep(12,1:3))
            CALL DeformMesh3D(xc,XPhysDom3D(1:3,i,j,k,iElem))
            CALL RotateMesh3D(RotationMatrix3D,XPhysDom3D(1:3,i,j,k,iElem))
          END DO
        END DO
      END DO
      CALL InterpolateToNewPoints3D(&
        PP_nDims,PP_NGeo,PP_N,VDM_CGLNGeo_OutputN,XPhysDom3D(:,:,:,:,iElem),XPhysDom3D(:,:,:,:,iElem))
    END DO
  END DO
END DO

!----------------------------------------------------------------------------------------------------------------------!
END SUBROUTINE BuildPhysicalDomain3D_CurvedDomain
!======================================================================================================================!
!
!
!
!======================================================================================================================!
SUBROUTINE DeformMesh2D(xc,x)
!----------------------------------------------------------------------------------------------------------------------!
USE MOD_MeshBuiltIn_vars,ONLY: ParametersMeshBuiltIn2D
!----------------------------------------------------------------------------------------------------------------------!
IMPLICIT NONE
!----------------------------------------------------------------------------------------------------------------------!
! FORMAL ARGUMENTS
!----------------------------------------------------------------------------------------------------------------------!
REAL,INTENT(IN)    :: xc(1:4,1:2)
REAL,INTENT(INOUT) :: x(1:2)
!----------------------------------------------------------------------------------------------------------------------!
! LOCAL VARIABLES
!----------------------------------------------------------------------------------------------------------------------!
REAL :: Lx, Ly
REAL :: xm, ym
REAL :: alpha, PI
REAL :: r, s, t
!----------------------------------------------------------------------------------------------------------------------!

!********************************************************************************!
! References:
!   BIBKEY : chan2019a
!   AUTHORS: Jesse Chan, David Del Rey Fernandez, and Mark Carpenter.
!   TITLE  : Efficient Entropy Stable Gauss Collocation Methods
!   JOURNAL: SIAM J. Sci. Comput. Vol. 41, No. 5, pp. A2938-A2966
!********************************************************************************!

IF (ParametersMeshBuiltIn2D%WarpMesh .EQV. .FALSE.) THEN
  RETURN
END IF

Lx = ABS(xc(2,1)-xc(1,1))
Ly = ABS(xc(3,2)-xc(2,2))

xm = 0.5*(xc(1,1)+xc(2,1))
ym = 0.5*(xc(2,2)+xc(3,2))

alpha = ParametersMeshBuiltIn2D%MeshDeformationFactor
PI    = ACOS(-1.0)

r = x(1)
s = x(2)
x(1) = r + alpha*Lx*COS(1.0*PI*(r-xm)/Lx)*COS(4.0*PI*(s-ym)/Ly)
t = x(1)
x(2) = s + alpha*Ly*COS(3.0*PI*(t-xm)/Lx)*COS(1.0*PI*(s-ym)/Ly)

!----------------------------------------------------------------------------------------------------------------------!
END SUBROUTINE DeformMesh2D
!======================================================================================================================!
!
!
!
!======================================================================================================================!
SUBROUTINE DeformMesh3D(xc,x)
!----------------------------------------------------------------------------------------------------------------------!
USE MOD_MeshBuiltIn_vars,ONLY: ParametersMeshBuiltIn3D
!----------------------------------------------------------------------------------------------------------------------!
IMPLICIT NONE
!----------------------------------------------------------------------------------------------------------------------!
! FORMAL ARGUMENTS
!----------------------------------------------------------------------------------------------------------------------!
REAL,INTENT(IN)    :: xc(1:8,1:3)
REAL,INTENT(INOUT) :: x(1:3)
!----------------------------------------------------------------------------------------------------------------------!
! LOCAL VARIABLES
!----------------------------------------------------------------------------------------------------------------------!
REAL    :: Lx, Ly, Lz
REAL    :: xm, ym, zm
REAL    :: alpha, PI
REAL    :: r, s, t, u, v
!----------------------------------------------------------------------------------------------------------------------!

!********************************************************************************!
! References:
!   BIBKEY : chan2019a
!   AUTHORS: Jesse Chan, David Del Rey Fernandez, and Mark Carpenter.
!   TITLE  : Efficient Entropy Stable Gauss Collocation Methods
!   JOURNAL: SIAM J. Sci. Comput. Vol. 41, No. 5, pp. A2938-A2966
!********************************************************************************!

IF (ParametersMeshBuiltIn3D%WarpMesh .EQV. .FALSE.) THEN
  RETURN
END IF

Lx = ABS(xc(2,1)-xc(1,1))
Ly = ABS(xc(4,2)-xc(1,2))
Lz = ABS(xc(5,3)-xc(1,3))

xm = 0.5*(xc(1,1)+xc(2,1))
ym = 0.5*(xc(1,2)+xc(4,2))
zm = 0.5*(xc(1,3)+xc(5,3))

alpha = ParametersMeshBuiltIn3D%MeshDeformationFactor
PI    = ACOS(-1.0)

r = x(1)
s = x(2)
t = x(3)
x(1) = r + alpha*Lx*COS(1.0*PI*(r-xm)/Lx)*COS(4.0*PI*(s-ym)/Ly)*COS(1.0*PI*(t-zm)/Lz)
u = x(1)
x(2) = s + alpha*Ly*COS(3.0*PI*(u-xm)/Lx)*COS(1.0*PI*(s-ym)/Ly)*COS(1.0*PI*(t-zm)/Lz)
v = x(2)
x(3) = t + alpha*Lz*COS(1.0*PI*(u-xm)/Lx)*COS(2.0*PI*(v-ym)/Ly)*COS(1.0*PI*(t-zm)/Lz)

!----------------------------------------------------------------------------------------------------------------------!
END SUBROUTINE DeformMesh3D
!======================================================================================================================!
!
!
!
!======================================================================================================================!
SUBROUTINE RotateMesh2D(RotationMatrix2D,x)
!----------------------------------------------------------------------------------------------------------------------!
USE MOD_MeshBuiltIn_vars,ONLY: ParametersMeshBuiltIn2D
!----------------------------------------------------------------------------------------------------------------------!
IMPLICIT NONE
!----------------------------------------------------------------------------------------------------------------------!
! FORMAL ARGUMENTS
!----------------------------------------------------------------------------------------------------------------------!
REAL,INTENT(IN)    :: RotationMatrix2D(1:2,1:2)
REAL,INTENT(INOUT) :: x(1:2)
!----------------------------------------------------------------------------------------------------------------------!
! LOCAL VARIABLES
!----------------------------------------------------------------------------------------------------------------------!

IF (ParametersMeshBuiltIn2D%RotateMesh .EQV. .FALSE.) THEN
  RETURN
END IF

x(1:2) = MATMUL(RotationMatrix2D(1:2,1:2),x(1:2))

!----------------------------------------------------------------------------------------------------------------------!
END SUBROUTINE RotateMesh2D
!======================================================================================================================!
!
!
!
!======================================================================================================================!
SUBROUTINE RotateMesh3D(RotationMatrix3D,x)
!----------------------------------------------------------------------------------------------------------------------!
USE MOD_MeshBuiltIn_vars,ONLY: ParametersMeshBuiltIn3D
!----------------------------------------------------------------------------------------------------------------------!
IMPLICIT NONE
!----------------------------------------------------------------------------------------------------------------------!
! FORMAL ARGUMENTS
!----------------------------------------------------------------------------------------------------------------------!
REAL,INTENT(IN)    :: RotationMatrix3D(1:3,1:3,1:3)
REAL,INTENT(INOUT) :: x(1:3)
!----------------------------------------------------------------------------------------------------------------------!
! LOCAL VARIABLES
!----------------------------------------------------------------------------------------------------------------------!

IF (ParametersMeshBuiltIn3D%RotateMesh .EQV. .FALSE.) THEN
  RETURN
END IF

x(1:3) = MATMUL(RotationMatrix3D(1:3,1:3,1),x(1:3))
x(1:3) = MATMUL(RotationMatrix3D(1:3,1:3,2),x(1:3))
x(1:3) = MATMUL(RotationMatrix3D(1:3,1:3,3),x(1:3))

!----------------------------------------------------------------------------------------------------------------------!
END SUBROUTINE RotateMesh3D
!======================================================================================================================!
!
!
!
!----------------------------------------------------------------------------------------------------------------------!
END MODULE MOD_MeshBuiltIn
!======================================================================================================================!
