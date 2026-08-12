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
MODULE MOD_GeometryBuiltIn_vars
!----------------------------------------------------------------------------------------------------------------------!
USE MOD_GLOBAL_vars
!----------------------------------------------------------------------------------------------------------------------!
IMPLICIT NONE
!----------------------------------------------------------------------------------------------------------------------!
PUBLIC
!----------------------------------------------------------------------------------------------------------------------!
! GLOBAL VARIABLES
!----------------------------------------------------------------------------------------------------------------------!
TYPE tParametersGeometryBuiltIn
  CHARACTER(LEN=256) :: WhichGeometryBuiltIn
  LOGICAL            :: DebugGeometryBuiltIn= .FALSE.
END TYPE
!----------------------------------------------------------------------------------------------------------------------!
ABSTRACT INTERFACE
  SUBROUTINE GeometryMapping2D_INT(s,xc,x)
    IMPLICIT NONE
    REAL,INTENT(IN)    :: s
    REAL,INTENT(IN)    :: xc(1:2)
    REAL,INTENT(OUT)   :: x(1:2)
  END SUBROUTINE GeometryMapping2D_INT
END INTERFACE
PROCEDURE(GeometryMapping2D_INT),POINTER :: GeometryMapping2D
!----------------------------------------------------------------------------------------------------------------------!
ABSTRACT INTERFACE
  SUBROUTINE GeometryMapping3D_INT(s,xc,x)
    IMPLICIT NONE
    REAL,INTENT(IN)    :: s(1:2)
    REAL,INTENT(IN)    :: xc(1:3)
    REAL,INTENT(OUT)   :: x(1:3)
  END SUBROUTINE GeometryMapping3D_INT
END INTERFACE
PROCEDURE(GeometryMapping3D_INT),POINTER :: GeometryMapping3D
!----------------------------------------------------------------------------------------------------------------------!
TYPE(tParametersGeometryBuiltIn) :: ParametersGeometryBuiltIn
!----------------------------------------------------------------------------------------------------------------------!
LOGICAL :: InitializeGeometryBuiltInIsDone = .FALSE.
!----------------------------------------------------------------------------------------------------------------------!
!
!----------------------------------------------------------------------------------------------------------------------!
END MODULE MOD_GeometryBuiltIn_vars
!======================================================================================================================!
