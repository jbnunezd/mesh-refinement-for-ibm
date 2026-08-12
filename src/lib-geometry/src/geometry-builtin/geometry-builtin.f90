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
MODULE MOD_GeometryBuiltIn
!----------------------------------------------------------------------------------------------------------------------!
USE MOD_GLOBAL_vars
!----------------------------------------------------------------------------------------------------------------------!
IMPLICIT NONE
!----------------------------------------------------------------------------------------------------------------------!
PRIVATE
!----------------------------------------------------------------------------------------------------------------------!
INTERFACE InitializeGeometryBuiltIn
  MODULE PROCEDURE InitializeGeometryBuiltIn
END INTERFACE

INTERFACE GeometryBuiltIn
  MODULE PROCEDURE GeometryBuiltIn
END INTERFACE

INTERFACE ExtractPointsFromGeometryBuiltIn
  MODULE PROCEDURE ExtractPointsFromGeometryBuiltIn
END INTERFACE
!----------------------------------------------------------------------------------------------------------------------!
PUBLIC :: InitializeGeometryBuiltIn
PUBLIC :: GeometryBuiltIn
PUBLIC :: ExtractPointsFromGeometryBuiltIn
!----------------------------------------------------------------------------------------------------------------------!
CHARACTER(LEN=256) :: ModuleName = "GeometryBuiltIn"
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
SUBROUTINE InitializeGeometryBuiltIn()
!----------------------------------------------------------------------------------------------------------------------!
USE MOD_ConfigFilesTools
!----------------------------------------------------------------------------------------------------------------------!
USE MOD_GeometryBuiltIn_vars,ONLY: ParametersGeometryBuiltIn
USE MOD_GeometryBuiltIn_vars,ONLY: InitializeGeometryBuiltInIsDone
!----------------------------------------------------------------------------------------------------------------------!
IMPLICIT NONE
!----------------------------------------------------------------------------------------------------------------------!
! FORMAL ARGUMENTS
!----------------------------------------------------------------------------------------------------------------------!
! LOCAL VARIABLES
!----------------------------------------------------------------------------------------------------------------------!
CHARACTER(LEN=256) :: Header
CHARACTER(LEN=256) :: ErrorMessage
CHARACTER(LEN=256) :: FunctionName = "InitializeGeometryBuiltIn"
!----------------------------------------------------------------------------------------------------------------------!

IF (InitializeGeometryBuiltInIsDone) THEN
  SWRITE(UNIT_SCREEN,*) "InitializeGeometry not ready to be called or already called."
  RETURN
END IF

Header = "INITIALIZING GEOMETRY BUILTIN MODULE..."
CALL PrintHeader(Header)

ParametersGeometryBuiltIn%WhichGeometryBuiltIn = GetString('WhichGeometryBuiltIn')
ParametersGeometryBuiltIn%DebugGeometryBuiltIn = GetLogical('DebugGeometryBuiltIn','.FALSE.')

SELECT CASE(PP_nDims)
  CASE(2)
    CALL InitializeGeometryBuiltIn2D()
  CASE(3)
    CALL InitializeGeometryBuiltIn3D()
  CASE DEFAULT
    ErrorMessage = "InitializeGeometryBuiltIn: Invalid value for MeshDimension"
    CALL PrintError(__STAMP__,ErrorMessage,ModuleName,FunctionName)
END SELECT

InitializeGeometryBuiltInIsDone = .TRUE.

!----------------------------------------------------------------------------------------------------------------------!
END SUBROUTINE InitializeGeometryBuiltIn
!======================================================================================================================!
!
!
!
!======================================================================================================================!
SUBROUTINE InitializeGeometryBuiltIn2D()
!----------------------------------------------------------------------------------------------------------------------!
USE MOD_DataStructures,ONLY: LowerCase
!----------------------------------------------------------------------------------------------------------------------!
USE MOD_GeometryBuiltIn_vars,ONLY: GeometryMapping2D
USE MOD_GeometryBuiltIn_vars,ONLY: ParametersGeometryBuiltIn
!----------------------------------------------------------------------------------------------------------------------!
USE MOD_GeometryBuiltInMappings2D,ONLY: GeometryMapping2D_Circle
USE MOD_GeometryBuiltInMappings2D,ONLY: GeometryMapping2D_Ellipse
!----------------------------------------------------------------------------------------------------------------------!
IMPLICIT NONE
!----------------------------------------------------------------------------------------------------------------------!
! FORMAL ARGUMENTS
!----------------------------------------------------------------------------------------------------------------------!
! LOCAL VARIABLES
!----------------------------------------------------------------------------------------------------------------------!
CHARACTER(LEN=256) :: ErrorMessage
CHARACTER(LEN=256) :: FunctionName = "InitializeGeometryBuiltIn2D"
!----------------------------------------------------------------------------------------------------------------------!

SELECT CASE(LowerCase(ParametersGeometryBuiltIn%WhichGeometryBuiltIn))
  CASE('circle')
    GeometryMapping2D => GeometryMapping2D_Circle
  CASE('ellipse')
    GeometryMapping2D => GeometryMapping2D_Ellipse
  CASE DEFAULT
    ErrorMessage = "Analytical Mapping not implemented"
    CALL PrintError(__STAMP__,ErrorMessage,ModuleName,FunctionName)
END SELECT

!----------------------------------------------------------------------------------------------------------------------!
END SUBROUTINE InitializeGeometryBuiltIn2D
!======================================================================================================================!
!
!
!
!======================================================================================================================!
SUBROUTINE InitializeGeometryBuiltIn3D()
!----------------------------------------------------------------------------------------------------------------------!
USE MOD_DataStructures,ONLY: LowerCase
!----------------------------------------------------------------------------------------------------------------------!
USE MOD_GeometryBuiltIn_vars,ONLY: GeometryMapping3D
USE MOD_GeometryBuiltIn_vars,ONLY: ParametersGeometryBuiltIn
!----------------------------------------------------------------------------------------------------------------------!
USE MOD_GeometryBuiltInMappings3D,ONLY: GeometryMapping3D_Point
USE MOD_GeometryBuiltInMappings3D,ONLY: GeometryMapping3D_Sphere
!----------------------------------------------------------------------------------------------------------------------!
IMPLICIT NONE
!----------------------------------------------------------------------------------------------------------------------!
! FORMAL ARGUMENTS
!----------------------------------------------------------------------------------------------------------------------!
! LOCAL VARIABLES
!----------------------------------------------------------------------------------------------------------------------!
CHARACTER(LEN=256) :: ErrorMessage
CHARACTER(LEN=256) :: FunctionName = "InitializeGeometryBuiltIn3D"
!----------------------------------------------------------------------------------------------------------------------!

SELECT CASE(LowerCase(ParametersGeometryBuiltIn%WhichGeometryBuiltIn))
  CASE('point')
    GeometryMapping3D => GeometryMapping3D_Point
  CASE('sphere')
    GeometryMapping3D => GeometryMapping3D_Sphere
  CASE DEFAULT
    ErrorMessage = "Analytical Mapping not implemented"
    CALL PrintError(__STAMP__,ErrorMessage,ModuleName,FunctionName)
END SELECT

!----------------------------------------------------------------------------------------------------------------------!
END SUBROUTINE InitializeGeometryBuiltIn3D
!======================================================================================================================!
!
!
!
!======================================================================================================================!
SUBROUTINE GeometryBuiltIn()
!----------------------------------------------------------------------------------------------------------------------!
IMPLICIT NONE
!----------------------------------------------------------------------------------------------------------------------!
! FORMAL ARGUMENTS
!----------------------------------------------------------------------------------------------------------------------!
! LOCAL VARIABLES
!----------------------------------------------------------------------------------------------------------------------!
REAL               :: CalcTimeIni
REAL               :: CalcTimeEnd
CHARACTER(LEN=256) :: ElapsedTime
!----------------------------------------------------------------------------------------------------------------------!
CHARACTER(LEN=256) :: Header
CHARACTER(LEN=256) :: ErrorMessage
CHARACTER(LEN=256) :: FunctionName = "GeometryBuiltIn"
!----------------------------------------------------------------------------------------------------------------------!

!------------------------------------------------------------!
! CREATING GEOMETRY
!------------------------------------------------------------!
SWRITE(UNIT_SCREEN,*)
Header = "CREATING GEOMETRY..."
CALL PrintMessage(Header)

CalcTimeIni = RunningTime()

SELECT CASE(PP_nDims)
  CASE(2)
    CALL GeometryBuiltIn2D()
  CASE(3)
    CALL GeometryBuiltIn3D()
  CASE DEFAULT
    ErrorMessage = "Invalid value for MeshDimension"
    CALL PrintError(__STAMP__,ErrorMessage,ModuleName,FunctionName)
END SELECT

CALL ExtractPointsFromGeometryBuiltIn()

CalcTimeEnd = RunningTime()
CALL ComputeRuntime(CalcTimeEnd-CalcTimeIni,ElapsedTime)
CALL PrintAnalyze("Elapsed Time",ElapsedTime,nTabIn=2)

!----------------------------------------------------------------------------------------------------------------------!
END SUBROUTINE GeometryBuiltIn
!======================================================================================================================!
!
!
!
!======================================================================================================================!
SUBROUTINE GeometryBuiltIn2D()
!----------------------------------------------------------------------------------------------------------------------!
USE MOD_GeometryMain_vars,ONLY: GeometryData_PointsCoordinates2D
!----------------------------------------------------------------------------------------------------------------------!
USE MOD_GeometryBuiltIn_vars,ONLY: GeometryMapping2D
!----------------------------------------------------------------------------------------------------------------------!
IMPLICIT NONE
!----------------------------------------------------------------------------------------------------------------------!
! FORMAL ARGUMENTS
!----------------------------------------------------------------------------------------------------------------------!
! LOCAL VARIABLES
!----------------------------------------------------------------------------------------------------------------------!
INTEGER :: ii
INTEGER :: iPoint
INTEGER :: nPoints
REAL    :: sIni
REAL    :: sEnd
REAL    :: s
REAL    :: ds
REAL    :: xc(1:2)
!----------------------------------------------------------------------------------------------------------------------!

nPoints = 1000
IF (.NOT. ALLOCATED(GeometryData_PointsCoordinates2D)) THEN
  ALLOCATE(GeometryData_PointsCoordinates2D(1:2,1:nPoints))
END IF

sIni = 0.0
sEnd = 2.0*ACOS(-1.0)
ds   = ABS(sEnd-sIni)/REAL(nPoints)

! ! ! xc(1) = 0.0
! ! ! xc(2) = 0.0
xc(1) = 2.0
xc(2) = 1.5

iPoint = 0
DO ii=1,nPoints
  iPoint = iPoint+1
  s = sIni + REAL(ii)*ds
  CALL GeometryMapping2D(s,xc,GeometryData_PointsCoordinates2D(1:2,iPoint))
END DO

!----------------------------------------------------------------------------------------------------------------------!
END SUBROUTINE GeometryBuiltIn2D
!======================================================================================================================!
!
!
!
!======================================================================================================================!
SUBROUTINE GeometryBuiltIn3D()
!----------------------------------------------------------------------------------------------------------------------!
USE MOD_GeometryMain_vars,ONLY: GeometryData_PointsCoordinates3D
!----------------------------------------------------------------------------------------------------------------------!
USE MOD_GeometryBuiltIn_vars,ONLY: GeometryMapping3D
!----------------------------------------------------------------------------------------------------------------------!
IMPLICIT NONE
!----------------------------------------------------------------------------------------------------------------------!
! FORMAL ARGUMENTS
!----------------------------------------------------------------------------------------------------------------------!
! LOCAL VARIABLES
!----------------------------------------------------------------------------------------------------------------------!
INTEGER :: ii
INTEGER :: jj
INTEGER :: iPoint
INTEGER :: nPoints
REAL    :: sIni(1:2)
REAL    :: sEnd(1:2)
REAL    :: s(1:2)
REAL    :: ds(1:2)
REAL    :: xc(1:3)
!----------------------------------------------------------------------------------------------------------------------!

nPoints = 100
IF (.NOT. ALLOCATED(GeometryData_PointsCoordinates3D)) THEN
  ALLOCATE(GeometryData_PointsCoordinates3D(1:3,1:nPoints*nPoints))
END IF

sIni(1) = 0.0
sIni(2) = 0.0
sEnd(1) = 2.0*ACOS(-1.0)
sEnd(2) = 1.0*ACOS(-1.0)
ds(1) = ABS(sEnd(1)-sIni(1))/REAL(nPoints)
ds(2) = ABS(sEnd(2)-sIni(2))/REAL(nPoints)

xc(1) = 0.0
xc(2) = 0.0
xc(3) = 0.0

iPoint = 0
DO jj=1,nPoints
  DO ii=1,nPoints
    iPoint = iPoint+1
    s(1) = sIni(1) + REAL(ii)*ds(1)
    s(2) = sIni(2) + REAL(jj)*ds(2)
    CALL GeometryMapping3D(s,xc,GeometryData_PointsCoordinates3D(1:3,iPoint))
  END DO
END DO

!----------------------------------------------------------------------------------------------------------------------!
END SUBROUTINE GeometryBuiltIn3D
!======================================================================================================================!
!
!
!
!======================================================================================================================!
SUBROUTINE ExtractPointsFromGeometryBuiltIn()
!----------------------------------------------------------------------------------------------------------------------!
USE MOD_GeometryMain_vars,ONLY: GeometryPoints
USE MOD_GeometryMain_vars,ONLY: GeometryData_PointsCoordinates2D
USE MOD_GeometryMain_vars,ONLY: GeometryData_PointsCoordinates3D
!----------------------------------------------------------------------------------------------------------------------!
IMPLICIT NONE
!----------------------------------------------------------------------------------------------------------------------!
! FORMAL ARGUMENTS
!----------------------------------------------------------------------------------------------------------------------!
! LOCAL VARIABLES
!----------------------------------------------------------------------------------------------------------------------!
INTEGER :: iPoint
INTEGER :: nPoints
!----------------------------------------------------------------------------------------------------------------------!
CHARACTER(LEN=256) :: ErrorMessage
CHARACTER(LEN=256) :: FunctionName = "ExtractPointsFromGeometryBuiltIn"
!----------------------------------------------------------------------------------------------------------------------!

SELECT CASE(PP_nDims)
  CASE(2)
    nPoints = SIZE(GeometryData_PointsCoordinates2D,2)
    IF (.NOT. ALLOCATED(GeometryPoints)) THEN
      ALLOCATE(GeometryPoints(1:nPoints))
    END IF
    DO iPoint=1,nPoints
      GeometryPoints(iPoint)%PointID     = iPoint
      GeometryPoints(iPoint)%Coords(1:2) = GeometryData_PointsCoordinates2D(1:2,iPoint)
      GeometryPoints(iPoint)%Coords(3)   = 0.0
    END DO
  CASE(3)
    nPoints = SIZE(GeometryData_PointsCoordinates3D,2)
    IF (.NOT. ALLOCATED(GeometryPoints)) THEN
      ALLOCATE(GeometryPoints(1:nPoints))
    END IF
    DO iPoint=1,nPoints
      GeometryPoints(iPoint)%PointID     = iPoint
      GeometryPoints(iPoint)%Coords(1:3) = GeometryData_PointsCoordinates3D(1:3,iPoint)
    END DO
  CASE DEFAULT
    ErrorMessage = "ExtractPointsFromGeometryBuiltIn: Invalid value for MeshDimension"
    CALL PrintError(__STAMP__,ErrorMessage,ModuleName,FunctionName)
END SELECT

!----------------------------------------------------------------------------------------------------------------------!
END SUBROUTINE ExtractPointsFromGeometryBuiltIn
!======================================================================================================================!
!
!
!
!----------------------------------------------------------------------------------------------------------------------!
END MODULE MOD_GeometryBuiltIn
!======================================================================================================================!
