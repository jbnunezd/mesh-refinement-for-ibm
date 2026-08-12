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
MODULE MOD_MeshBuiltInStretchingFunctions3D
!----------------------------------------------------------------------------------------------------------------------!
USE MOD_GLOBAL_vars
!----------------------------------------------------------------------------------------------------------------------!
IMPLICIT NONE
!----------------------------------------------------------------------------------------------------------------------!
PUBLIC
!----------------------------------------------------------------------------------------------------------------------!
INTERFACE MeshStretching3D_Uniform
  MODULE PROCEDURE MeshStretching3D_Uniform
END INTERFACE

INTERFACE MeshStretching3D_GeometricProgression
  MODULE PROCEDURE MeshStretching3D_GeometricProgression
END INTERFACE

INTERFACE MeshStretching3D_ThreeSidesBoundariesHyperbolic
  MODULE PROCEDURE MeshStretching3D_ThreeSidesBoundariesHyperbolic
END INTERFACE

INTERFACE MeshStretching3D_ThreeSidesBoundariesPotential
  MODULE PROCEDURE MeshStretching3D_ThreeSidesBoundariesPotential
END INTERFACE
!----------------------------------------------------------------------------------------------------------------------!
PUBLIC :: MeshStretching3D_Uniform
PUBLIC :: MeshStretching3D_GeometricProgression
PUBLIC :: MeshStretching3D_ThreeSidesBoundariesHyperbolic
PUBLIC :: MeshStretching3D_ThreeSidesBoundariesPotential
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
SUBROUTINE MeshStretching3D_Uniform(nElems,dx,dy,dz)
!----------------------------------------------------------------------------------------------------------------------!
IMPLICIT NONE
!----------------------------------------------------------------------------------------------------------------------!
! FORMAL ARGUMENTS
!----------------------------------------------------------------------------------------------------------------------!
INTEGER,INTENT(IN) :: nElems(1:3)
REAL,INTENT(OUT)   :: dx(1:nElems(1))
REAL,INTENT(OUT)   :: dy(1:nElems(2))
REAL,INTENT(OUT)   :: dz(1:nElems(3))
!----------------------------------------------------------------------------------------------------------------------!
! LOCAL VARIABLES
!----------------------------------------------------------------------------------------------------------------------!

dx(1:nElems(1)) = 1.0
dy(1:nElems(2)) = 1.0
dz(1:nElems(3)) = 1.0

dx = dx*(2.0/SUM(dx(1:nElems(1))))
dy = dy*(2.0/SUM(dy(1:nElems(2))))
dz = dz*(2.0/SUM(dz(1:nElems(3))))

!----------------------------------------------------------------------------------------------------------------------!
END SUBROUTINE MeshStretching3D_Uniform
!======================================================================================================================!
!
!
!
!======================================================================================================================!
SUBROUTINE MeshStretching3D_GeometricProgression(nElems,dx,dy,dz)
!----------------------------------------------------------------------------------------------------------------------!
USE MOD_MeshBuiltIn_vars,ONLY: ParametersMeshBuiltIn3D
!----------------------------------------------------------------------------------------------------------------------!
IMPLICIT NONE
!----------------------------------------------------------------------------------------------------------------------!
! FORMAL ARGUMENTS
!----------------------------------------------------------------------------------------------------------------------!
INTEGER,INTENT(IN) :: nElems(1:3)
REAL,INTENT(OUT)   :: dx(1:nElems(1))
REAL,INTENT(OUT)   :: dy(1:nElems(2))
REAL,INTENT(OUT)   :: dz(1:nElems(3))
!----------------------------------------------------------------------------------------------------------------------!
! LOCAL VARIABLES
!----------------------------------------------------------------------------------------------------------------------!
INTEGER :: ii
REAL    :: alpha(1:3)
!----------------------------------------------------------------------------------------------------------------------!

alpha(1:3) = ParametersMeshBuiltIn3D%MeshStretchingFactor3D(1:3)

dx(1) = 1.0
dy(1) = 1.0
dz(1) = 1.0

DO ii=2,nElems(1)
  dx(ii) = dx(ii-1)*alpha(1)
END DO
DO ii=2,nElems(2)
  dy(ii) = dy(ii-1)*alpha(2)
END DO
DO ii=2,nElems(3)
  dz(ii) = dz(ii-1)*alpha(3)
END DO

dx = dx*(2.0/SUM(dx(1:nElems(1))))
dy = dy*(2.0/SUM(dy(1:nElems(2))))
dz = dz*(2.0/SUM(dz(1:nElems(3))))

!----------------------------------------------------------------------------------------------------------------------!
END SUBROUTINE MeshStretching3D_GeometricProgression
!======================================================================================================================!
!
!
!
!======================================================================================================================!
SUBROUTINE MeshStretching3D_ThreeSidesBoundariesHyperbolic(nElems,dx,dy,dz)
!----------------------------------------------------------------------------------------------------------------------!
USE MOD_MeshBuiltIn_vars,ONLY: ParametersMeshBuiltIn3D
!----------------------------------------------------------------------------------------------------------------------!
IMPLICIT NONE
!----------------------------------------------------------------------------------------------------------------------!
! FORMAL ARGUMENTS
!----------------------------------------------------------------------------------------------------------------------!
INTEGER,INTENT(IN) :: nElems(1:3)
REAL,INTENT(OUT)   :: dx(1:nElems(1))
REAL,INTENT(OUT)   :: dy(1:nElems(2))
REAL,INTENT(OUT)   :: dz(1:nElems(3))
!----------------------------------------------------------------------------------------------------------------------!
! LOCAL VARIABLES
!----------------------------------------------------------------------------------------------------------------------!
INTEGER :: ii
REAL    :: ds
REAL    :: s
REAL    :: alpha(1:3)
!----------------------------------------------------------------------------------------------------------------------!

alpha(1:3) = ParametersMeshBuiltIn3D%MeshStretchingFactor3D(1:3)

IF (nElems(1) .EQ. 1) THEN
  ds = 2.0
  dx = ds
ELSE
  ds = 2.0/REAL(nElems(1)-1.0)
  s = -1.0
  DO ii=1,nElems(1)
    dx(ii) = ds*ABS(1.0-TANH(alpha(1)*ABS(s)))
    s = s+ds
  END DO
END IF

IF (nElems(2) .EQ. 1) THEN
  ds = 2.0
  dy = ds
ELSE
  ds = 2.0/REAL(nElems(2)-1.0)
  s = -1.0
  DO ii=1,nElems(2)
    dy(ii) = ds*ABS(1.0-TANH(alpha(2)*ABS(s)))
    s = s+ds
  END DO
END IF

IF (nElems(3) .EQ. 1) THEN
  ds = 2.0
  dz = ds
ELSE
  ds = 2.0/REAL(nElems(3)-1.0)
  s = -1.0
  DO ii=1,nElems(3)
    dz(ii) = ds*ABS(1.0-TANH(alpha(3)*ABS(s)))
    s = s+ds
  END DO
END IF

dx = dx*(2.0/SUM(dx(1:nElems(1))))
dy = dy*(2.0/SUM(dy(1:nElems(2))))
dz = dz*(2.0/SUM(dz(1:nElems(3))))

!----------------------------------------------------------------------------------------------------------------------!
END SUBROUTINE MeshStretching3D_ThreeSidesBoundariesHyperbolic
!======================================================================================================================!
!
!
!
!======================================================================================================================!
SUBROUTINE MeshStretching3D_ThreeSidesBoundariesPotential(nElems,dx,dy,dz)
!----------------------------------------------------------------------------------------------------------------------!
USE MOD_MeshBuiltIn_vars,ONLY: ParametersMeshBuiltIn3D
!----------------------------------------------------------------------------------------------------------------------!
IMPLICIT NONE
!----------------------------------------------------------------------------------------------------------------------!
! FORMAL ARGUMENTS
!----------------------------------------------------------------------------------------------------------------------!
INTEGER,INTENT(IN) :: nElems(1:3)
REAL,INTENT(OUT)   :: dx(1:nElems(1))
REAL,INTENT(OUT)   :: dy(1:nElems(2))
REAL,INTENT(OUT)   :: dz(1:nElems(3))
!----------------------------------------------------------------------------------------------------------------------!
! LOCAL VARIABLES
!----------------------------------------------------------------------------------------------------------------------!
INTEGER :: ii
REAL    :: ds
REAL    :: s
REAL    :: alpha(1:3)
!----------------------------------------------------------------------------------------------------------------------!

alpha(1:3) = ParametersMeshBuiltIn3D%MeshStretchingFactor3D(1:3)

IF (nElems(1) .EQ. 1) THEN
  ds = 2.0
  dx = ds
ELSE
  ds = 2.0/REAL(nElems(1)-1.0)
  s = -1.0
  DO ii=1,nElems(1)
    dx(ii) = ds*(1.0/(1.0+(alpha(1)*ABS(s))**6))
    s = s+ds
  END DO
END IF

IF (nElems(2) .EQ. 1) THEN
  ds = 2.0
  dy = ds
ELSE
  ds = 2.0/REAL(nElems(2)-1.0)
  s = -1.0
  DO ii=1,nElems(2)
    dy(ii) = ds*(1.0/(1.0+(alpha(2)*ABS(s))**6))
    s = s+ds
  END DO
END IF

IF (nElems(3) .EQ. 1) THEN
  ds = 2.0
  dz = ds
ELSE
  ds = 2.0/REAL(nElems(3)-1.0)
  s = -1.0
  DO ii=1,nElems(3)
    dz(ii) = ds*(1.0/(1.0+(alpha(3)*ABS(s))**6))
    s = s+ds
  END DO
END IF

dx = dx*(2.0/SUM(dx(1:nElems(1))))
dy = dy*(2.0/SUM(dy(1:nElems(2))))
dz = dz*(2.0/SUM(dz(1:nElems(3))))

!----------------------------------------------------------------------------------------------------------------------!
END SUBROUTINE MeshStretching3D_ThreeSidesBoundariesPotential
!======================================================================================================================!
!
!
!
!----------------------------------------------------------------------------------------------------------------------!
END MODULE MOD_MeshBuiltInStretchingFunctions3D
!======================================================================================================================!
