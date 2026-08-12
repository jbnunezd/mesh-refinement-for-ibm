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
MODULE MOD_MeshExport
!----------------------------------------------------------------------------------------------------------------------!
USE MOD_GLOBAL_vars
!----------------------------------------------------------------------------------------------------------------------!
IMPLICIT NONE
!----------------------------------------------------------------------------------------------------------------------!
PRIVATE
!----------------------------------------------------------------------------------------------------------------------!
INTERFACE InitializeMeshExport
  MODULE PROCEDURE InitializeMeshExport
END INTERFACE

INTERFACE MeshExport
  MODULE PROCEDURE MeshExport
END INTERFACE
!----------------------------------------------------------------------------------------------------------------------!
PUBLIC :: InitializeMeshExport
PUBLIC :: MeshExport
!----------------------------------------------------------------------------------------------------------------------!
CHARACTER(LEN=256) :: ModuleName = "MeshExport"
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
SUBROUTINE InitializeMeshExport()
!----------------------------------------------------------------------------------------------------------------------!
USE MOD_ConfigFilesTools
!----------------------------------------------------------------------------------------------------------------------!
USE MOD_DataStructures,ONLY: SortArray
USE MOD_DataStructures,ONLY: LowerCase
!----------------------------------------------------------------------------------------------------------------------!
USE MOD_MeshExport_vars,ONLY: MeshExportNames
USE MOD_MeshExport_vars,ONLY: MeshExportFormat
USE MOD_MeshExport_vars,ONLY: MeshExportFormatIndex
USE MOD_MeshExport_vars,ONLY: ParametersMeshExport
USE MOD_MeshExport_vars,ONLY: InitializeMeshExportIsDone
!----------------------------------------------------------------------------------------------------------------------!
IMPLICIT NONE
!----------------------------------------------------------------------------------------------------------------------!
! FORMAL ARGUMENTS
!----------------------------------------------------------------------------------------------------------------------!
! LOCAL VARIABLES
!----------------------------------------------------------------------------------------------------------------------!
INTEGER             :: iVar
INTEGER             :: jVar
INTEGER             :: nVars
INTEGER             :: iFormat
INTEGER             :: temp
INTEGER             :: nMeshExportFormat
INTEGER             :: nMeshExportFormatIn
INTEGER,ALLOCATABLE :: MeshExportFormatIndexTemp(:)
INTEGER,ALLOCATABLE :: index_vector(:)
LOGICAL,ALLOCATABLE :: Mask(:)
!----------------------------------------------------------------------------------------------------------------------!
CHARACTER(LEN=256) :: Header
!----------------------------------------------------------------------------------------------------------------------!

IF (InitializeMeshExportIsDone .EQV. .TRUE.) THEN
  SWRITE(UNIT_SCREEN,*) "InitializeMeshExport not ready to be called or already called."
  RETURN
END IF

Header = "INITIALIZING MESH-EXPORT MODULE..."
CALL PrintHeader(Header)

ParametersMeshExport%ExportMesh = GetLogical('ExportMesh','.FALSE.')

nVars = 3
ALLOCATE(MeshExportNames(1:nVars))
MeshExportNames(1) = "tecplot-ascii"
MeshExportNames(2) = "hdf5-coda"
MeshExportNames(3) = "gmsh"

IF (ParametersMeshExport%ExportMesh .EQV. .TRUE.) THEN
  nMeshExportFormatIn = CountStrings('MeshExportFormat',0)
  IF ((nMeshExportFormatIn .GT. 0)) THEN
    IF (.NOT. ALLOCATED(MeshExportFormat)) THEN
      ALLOCATE(MeshExportFormat(1:nMeshExportFormatIn))
    END IF
    DO iVar=1,nMeshExportFormatIn
      MeshExportFormat(iVar) = GetString('MeshExportFormat')
    END DO
    IF (.NOT. ALLOCATED(MeshExportFormatIndex)) THEN
      ALLOCATE(MeshExportFormatIndex(1:nMeshExportFormatIn))
    END IF

    jVar = 0
    MeshExportFormatIndex = 0
    DO iVar=1,nMeshExportFormatIn
      DO iFormat=1,nVars
        IF (TRIM(MeshExportFormat(iVar)) .EQ. TRIM(MeshExportNames(iFormat))) THEN
          jVar = jVar + 1
          MeshExportFormatIndex(jVar) = iFormat
        END IF
      END DO
    END DO

    IF (nMeshExportFormatIn .EQ. 1) THEN
      IF (MeshExportFormatIndex(1) .EQ. 0) THEN
        nMeshExportFormat = 0
      ELSE
        nMeshExportFormat = 1
      END IF
    END IF

    IF (nMeshExportFormatIn .GT. 1) THEN
      nMeshExportFormat = 0
      ALLOCATE(Mask(1:nMeshExportFormatIn))
      Mask = .TRUE.
      DO iVar=nMeshExportFormatIn,2,-1
        Mask(iVar) = .NOT.(ANY(MeshExportFormatIndex(1:iVar-1) .EQ. MeshExportFormatIndex(iVar)))
      END DO
      DO iVar=1,nMeshExportFormatIn
        IF (MeshExportFormatIndex(iVar) .EQ. 0) THEN
          Mask(iVar) = .FALSE.
        END IF
      END DO
      temp = SIZE(PACK([(iVar,iVar=1,nMeshExportFormatIn)],Mask))
      ALLOCATE(index_vector(temp))
      index_vector = PACK([(iVar,iVar=1,nMeshExportFormatIn)],Mask)
      temp = SIZE(MeshExportFormatIndex(index_vector))
      ALLOCATE(MeshExportFormatIndexTemp(temp))
      MeshExportFormatIndexTemp = MeshExportFormatIndex(index_vector)
      nMeshExportFormat = SIZE(MeshExportFormatIndexTemp)
      DO iVar=1,SIZE(MeshExportFormatIndexTemp)
        IF (MeshExportFormatIndexTemp(iVar) .EQ. 0) THEN
          nMeshExportFormat = nMeshExportFormat - 1
        END IF
      END DO
    END IF

    IF (nMeshExportFormat .EQ. 0) THEN
      nMeshExportFormat = nVars
      DEALLOCATE(MeshExportFormatIndex)
      ALLOCATE(MeshExportFormatIndex(nMeshExportFormat))
      MeshExportFormatIndex = 0
      DO iVar=1,nMeshExportFormat
        MeshExportFormatIndex(iVar) = iVar
      END DO
    ELSEIF ((nMeshExportFormat .GT. 0) .AND. (nMeshExportFormat .LT. nMeshExportFormatIn)) THEN
      DEALLOCATE(MeshExportFormatIndex)
      ALLOCATE(MeshExportFormatIndex(nMeshExportFormat))
      MeshExportFormatIndex = MeshExportFormatIndexTemp
      IF (nMeshExportFormat .GT. 1) THEN
        CALL SortArray(MeshExportFormatIndex)
      END IF
    ELSEIF ((nMeshExportFormat .GT. 0) .AND. (nMeshExportFormat .EQ. nMeshExportFormatIn)) THEN
      IF (nMeshExportFormat .GT. 1) THEN
        CALL SortArray(MeshExportFormatIndex)
      END IF
    END IF
  ELSE
    IF (.NOT. ALLOCATED(MeshExportFormatIndex)) THEN
      ALLOCATE(MeshExportFormatIndex(nMeshExportFormat))
    END IF
    MeshExportFormatIndex = 0
    DO iVar=1,nMeshExportFormat
      MeshExportFormatIndex(iVar) = iVar
    END DO
  END IF
END IF

InitializeMeshExportIsDone = .TRUE.

!----------------------------------------------------------------------------------------------------------------------!
END SUBROUTINE InitializeMeshExport
!======================================================================================================================!
!
!
!
!======================================================================================================================!
SUBROUTINE MeshExport()
!----------------------------------------------------------------------------------------------------------------------!
USE MOD_MeshExport_vars,ONLY: ParametersMeshExport
USE MOD_MeshExport_vars,ONLY: MeshExportNames
USE MOD_MeshExport_vars,ONLY: MeshExportFormatIndex
!----------------------------------------------------------------------------------------------------------------------!
USE MOD_DataStructures,ONLY: LowerCase
!----------------------------------------------------------------------------------------------------------------------!
USE MOD_MeshMain_vars,ONLY: MeshInfo
USE MOD_MeshMain_vars,ONLY: MeshData_BCFacesToNodes
USE MOD_MeshMain_vars,ONLY: MeshData_BCFacesToMark
USE MOD_MeshMain_vars,ONLY: MeshData_BCFacesToLevel
USE MOD_MeshMain_vars,ONLY: MeshData_BCFacesToElementType
USE MOD_MeshMain_vars,ONLY: MeshData_BoundaryMark
USE MOD_MeshMain_vars,ONLY: MeshData_BoundaryName
USE MOD_MeshMain_vars,ONLY: MeshData_ElementsToNodes
USE MOD_MeshMain_vars,ONLY: MeshData_ElementsToFlag
USE MOD_MeshMain_vars,ONLY: MeshData_ElementsToLevel
USE MOD_MeshMain_vars,ONLY: MeshData_ElementsToElementType
USE MOD_MeshMain_vars,ONLY: MeshData_NodesCoordinates
!----------------------------------------------------------------------------------------------------------------------!
USE MOD_MeshMain_vars,ONLY: MeshData_MasterSlavesToNodes_Tri4Tri
USE MOD_MeshMain_vars,ONLY: MeshData_MasterSlavesToNodes_Quad2Quad
USE MOD_MeshMain_vars,ONLY: MeshData_MasterSlavesToNodes_Quad4Quad
!----------------------------------------------------------------------------------------------------------------------!
USE MOD_DataExport_GMSH,   ONLY: DataExport_GMSH
USE MOD_DataExport_HDF5,   ONLY: DataExport_HDF5_CODA
USE MOD_DataExport_TECPLOT,ONLY: DataExport_TECPLOT_MESH_ASCII
!----------------------------------------------------------------------------------------------------------------------!
IMPLICIT NONE
!----------------------------------------------------------------------------------------------------------------------!
! FORMAL ARGUMENTS
!----------------------------------------------------------------------------------------------------------------------!
! LOCAL VARIABLES
!----------------------------------------------------------------------------------------------------------------------!
INTEGER :: iFormat
!----------------------------------------------------------------------------------------------------------------------!
CHARACTER(LEN=256) :: OutputFormat
!----------------------------------------------------------------------------------------------------------------------!
CHARACTER(LEN=256) :: ErrorMessage
CHARACTER(LEN=256) :: FunctionName = "MeshExport"
!----------------------------------------------------------------------------------------------------------------------!
INTEGER :: nTabIn
!----------------------------------------------------------------------------------------------------------------------!
REAL                :: CalcTimeIni
REAL                :: CalcTimeEnd
CHARACTER(LEN=256)  :: ElapsedTime
CHARACTER(LEN=256)  :: Header
!----------------------------------------------------------------------------------------------------------------------!

IF (ParametersMeshExport%ExportMesh .EQV. .FALSE.) THEN
  RETURN
END IF

nTabIn = 2
Header = "EXPORTING MESH"
CALL PrintMessage(Header,nTabIn=nTabIn)

CalcTimeIni = RunningTime()

DO iFormat=1,SIZE(MeshExportFormatIndex)
  OutputFormat = MeshExportNames(MeshExportFormatIndex(iFormat))
  SELECT CASE(LowerCase(TRIM(OutputFormat)))
    CASE('tecplot-ascii')
      CALL DataExport_TECPLOT_MESH_ASCII(&
        FileName              = MeshInfo%BaseFileName,&
        ProjectName           = MeshInfo%ProjectName,&
        ProgramName           = MeshInfo%ProgramName,&
        FileVersion           = MeshInfo%FileVersion,&
        nDims                 = MeshInfo%nDims,&
        NGeo                  = MeshInfo%NGeo,&
        OutputTime            = REAL(MeshInfo%MaxRefLevel),&
        VarNames              = MeshInfo%OutputVars,&
        ElementsToElementType = MeshData_ElementsToElementType,&
        ElementsToNodes       = MeshData_ElementsToNodes,&
        ElementsToLevel       = MeshData_ElementsToLevel,&
        ElementsToFlag        = MeshData_ElementsToFlag,&
        NodesCoordinates      = MeshData_NodesCoordinates,&
        BCFacesToNodes        = MeshData_BCFacesToNodes,&
        BCFacesToLevel        = MeshData_BCFacesToLevel,&
        BCFacesToElementType  = MeshData_BCFacesToElementType,&
        BCFacesToMark         = MeshData_BCFacesToMark,&
        BoundaryMark          = MeshData_BoundaryMark,&
        BoundaryName          = MeshData_BoundaryName)
    CASE('hdf5-coda')
      CALL DataExport_HDF5_CODA(&
        FileName                      = MeshInfo%BaseFileName,&
        ProjectName                   = MeshInfo%ProjectName,&
        ProgramName                   = MeshInfo%ProgramName,&
        FileVersion                   = MeshInfo%FileVersion,&
        nDims                         = MeshInfo%nDims,&
        NGeo                          = MeshInfo%NGeo,&
        OutputTime                    = REAL(MeshInfo%MaxRefLevel),&
        VarNames                      = MeshInfo%CoordNames,&
        ElementsToElementType         = MeshData_ElementsToElementType,&
        ElementsToNodes               = MeshData_ElementsToNodes,&
        NodesCoordinates              = MeshData_NodesCoordinates,&
        BCFacesToNodes                = MeshData_BCFacesToNodes,&
        BCFacesToElementType          = MeshData_BCFacesToElementType,&
        BCFacesToMark                 = MeshData_BCFacesToMark,&
        BoundaryMark                  = MeshData_BoundaryMark,&
        BoundaryName                  = MeshData_BoundaryName,&
        MasterSlavesToNodes_Tri4Tri   = MeshData_MasterSlavesToNodes_Tri4Tri,&
        MasterSlavesToNodes_Quad2Quad = MeshData_MasterSlavesToNodes_Quad2Quad,&
        MasterSlavesToNodes_Quad4Quad = MeshData_MasterSlavesToNodes_Quad4Quad)
    CASE('gmsh')
      CALL DataExport_GMSH(&
        FileName           = MeshInfo%BaseFileName,&
        ProjectName        = MeshInfo%ProjectName,&
        ProgramName        = MeshInfo%ProgramName,&
        FileVersion        = MeshInfo%FileVersion,&
        nDims              = MeshInfo%nDims,&
        ElementsType       = MeshData_ElementsToElementType,&
        ElementsToNodes    = MeshData_ElementsToNodes,&
        NodesCoordinates   = MeshData_NodesCoordinates,&
        BCFacesToNodes     = MeshData_BCFacesToNodes,&
        BCFacesToMark      = MeshData_BCFacesToMark,&
        BCFacesElementType = MeshData_BCFacesToElementType,&
        BoundaryMark       = MeshData_BoundaryMark,&
        BoundaryName       = MeshData_BoundaryName)
    CASE DEFAULT
    ErrorMessage = "Unknown output format"
    CALL PrintError(__STAMP__,ErrorMessage,ModuleName,FunctionName)
  END SELECT
END DO

CalcTimeEnd = RunningTime()
CALL ComputeRuntime(CalcTimeEnd-CalcTimeIni,ElapsedTime)
Header = "Elapsed Time"
CALL PrintAnalyze(Header,ElapsedTime,nTabIn=4)

!----------------------------------------------------------------------------------------------------------------------!
END SUBROUTINE MeshExport
!======================================================================================================================!
!
!
!
!----------------------------------------------------------------------------------------------------------------------!
END MODULE MOD_MeshExport
!======================================================================================================================!
