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
MODULE MOD_MeshAdaptation
!----------------------------------------------------------------------------------------------------------------------!
USE MOD_GLOBAL_vars
!----------------------------------------------------------------------------------------------------------------------!
IMPLICIT NONE
!----------------------------------------------------------------------------------------------------------------------!
PRIVATE
!----------------------------------------------------------------------------------------------------------------------!
INTERFACE InitializeMeshAdaptation
  MODULE PROCEDURE InitializeMeshAdaptation
END INTERFACE

INTERFACE MeshAdaptation
  MODULE PROCEDURE MeshAdaptation
END INTERFACE
!----------------------------------------------------------------------------------------------------------------------!
PUBLIC :: InitializeMeshAdaptation
PUBLIC :: MeshAdaptation
!----------------------------------------------------------------------------------------------------------------------!
CHARACTER(LEN=256) :: ModuleName = "MeshAdaptation"
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
SUBROUTINE InitializeMeshAdaptation()
!----------------------------------------------------------------------------------------------------------------------!
USE MOD_ConfigFilesTools
!----------------------------------------------------------------------------------------------------------------------!
USE MOD_DataStructures,ONLY: LowerCase
!----------------------------------------------------------------------------------------------------------------------!
USE MOD_MeshMain_vars,ONLY: MeshConstructionMethod
!----------------------------------------------------------------------------------------------------------------------!
USE MOD_MeshAdaptation_vars,ONLY: ParametersMeshAdaptation
USE MOD_MeshAdaptation_vars,ONLY: InitializeMeshAdaptationIsDone
!----------------------------------------------------------------------------------------------------------------------!
USE MOD_MeshImport,      ONLY: InitializeMeshImport
USE MOD_MeshExport,      ONLY: InitializeMeshExport
USE MOD_MeshBuiltIn,     ONLY: InitializeMeshBuiltIn
USE MOD_GeometryImport,  ONLY: InitializeGeometryImport
USE MOD_GeometryBuiltIn, ONLY: InitializeGeometryBuiltIn
USE MOD_MeshRefinement,  ONLY: InitializeMeshRefinement
USE MOD_MeshElementsList,ONLY: InitializeMeshElementsList
!----------------------------------------------------------------------------------------------------------------------!
USE MOD_MeshGeometry_vars,ONLY: ParametersMeshGeometry
!----------------------------------------------------------------------------------------------------------------------!
USE MOD_MeshRefinement_vars,ONLY: ParametersMeshRefinement
!----------------------------------------------------------------------------------------------------------------------!
IMPLICIT NONE
!----------------------------------------------------------------------------------------------------------------------!
! FORMAL ARGUMENTS
!----------------------------------------------------------------------------------------------------------------------!
! LOCAL VARIABLES
!----------------------------------------------------------------------------------------------------------------------!
CHARACTER(LEN=256) :: Header
CHARACTER(LEN=256) :: ErrorMessage
CHARACTER(LEN=256) :: FunctionName = "InitializeMeshAdaptation"
!----------------------------------------------------------------------------------------------------------------------!

IF (InitializeMeshAdaptationIsDone .EQV. .TRUE.) THEN
  SWRITE(UNIT_SCREEN,*) "InitializeMeshAdaptation not ready to be called or already called."
  RETURN
END IF

Header = "INITIALIZING MESH-ADAPTER MODULE..."
CALL PrintHeader(Header)

ParametersMeshAdaptation%MeshDimension               = GetInteger('MeshDimension','2')
ParametersMeshAdaptation%ProjectName                 = GetString('ProjectName')
ParametersMeshAdaptation%WhichMeshConstructionMethod = GetString('WhichMeshConstructionMethod')

PP_nDims    = ParametersMeshAdaptation%MeshDimension
ProjectName = ParametersMeshAdaptation%ProjectName
MeshConstructionMethod = ParametersMeshAdaptation%WhichMeshConstructionMethod

SELECT CASE(LowerCase(ParametersMeshAdaptation%WhichMeshConstructionMethod))
  CASE('mesh-import')
    CALL InitializeMeshImport()
  CASE('mesh-builtin')
    CALL InitializeMeshBuiltIn()
  CASE DEFAULT
  ErrorMessage = "InitializeMeshAdaptation: Unknown Mesh Construction Method"
  CALL PrintError(__STAMP__,ErrorMessage,ModuleName,FunctionName)
END SELECT

CALL InitializeMeshExport()
CALL InitializeMeshElementsList()
CALL InitializeMeshRefinement()

SELECT CASE(LowerCase(ParametersMeshRefinement%WhichMeshRefinement))
  CASE("refine-elements-around-geometry")
    SELECT CASE(LowerCase(ParametersMeshGeometry%WhichBodyGeometry))
      CASE("built-in-geometry")
        CALL InitializeGeometryBuiltIn()
      CASE("imported-geometry")
        CALL InitializeGeometryImport()
    END SELECT
  CASE("refine-elements-around-geometry-and-inside-box")
    SELECT CASE(LowerCase(ParametersMeshGeometry%WhichBodyGeometry))
      CASE("built-in-geometry")
        CALL InitializeGeometryBuiltIn()
      CASE("imported-geometry")
        CALL InitializeGeometryImport()
    END SELECT
END SELECT

InitializeMeshAdaptationIsDone = .TRUE.

!----------------------------------------------------------------------------------------------------------------------!
END SUBROUTINE InitializeMeshAdaptation
!======================================================================================================================!
!
!
!
!======================================================================================================================!
SUBROUTINE MeshAdaptation()
!----------------------------------------------------------------------------------------------------------------------!
USE MOD_DataStructures
!----------------------------------------------------------------------------------------------------------------------!
USE MOD_MeshMain_vars,ONLY: MeshInfo
USE MOD_MeshMain_vars,ONLY: ElemList
!----------------------------------------------------------------------------------------------------------------------!
USE MOD_MeshMainMethods,ONLY: CountElems
USE MOD_MeshMainMethods,ONLY: CountElemsPerLevel
USE MOD_MeshMainMethods,ONLY: CountNodes
USE MOD_MeshMainMethods,ONLY: PrintElemList
!----------------------------------------------------------------------------------------------------------------------!
USE MOD_MeshElementsList,ONLY: SetUniqueNodes
USE MOD_MeshElementsList,ONLY: SetUniqueElemID
USE MOD_MeshElementsList,ONLY: SetUniqueSideID
!----------------------------------------------------------------------------------------------------------------------!
USE MOD_MeshImport, ONLY: MeshImport
USE MOD_MeshExport, ONLY: MeshExport
USE MOD_MeshBuiltIn,ONLY: MeshBuiltIn
!----------------------------------------------------------------------------------------------------------------------!
USE MOD_GeometryImport, ONLY: GeometryImport
USE MOD_GeometryBuiltIn,ONLY: GeometryBuiltIn
!----------------------------------------------------------------------------------------------------------------------!
USE MOD_MeshElementsList,      ONLY: CreateElementsList
USE MOD_MeshAdaptation_vars,   ONLY: ParametersMeshAdaptation
USE MOD_MeshRefinement_vars,   ONLY: MaxRefinementLevel
USE MOD_MeshRefinement_vars,   ONLY: RefineMeshAroundGeometry
USE MOD_MeshRefinement_vars,   ONLY: FlagElementsForRefinement
USE MOD_MeshRefinement_vars,   ONLY: ParametersMeshRefinement
USE MOD_MeshRefinementFlagging,ONLY: FlagElementsForBalancing
USE MOD_MeshRefinementFlagging,ONLY: InitializeElementsToRefine
!----------------------------------------------------------------------------------------------------------------------!
USE MOD_MeshGeometryDistribution,ONLY: DistributeSTLFacetsInElemList
USE MOD_MeshGeometryDistribution,ONLY: DistributeSTLPointsInElemList
!----------------------------------------------------------------------------------------------------------------------!
USE MOD_MeshRefinementSplitting,ONLY: RefineElements
!----------------------------------------------------------------------------------------------------------------------!
USE MOD_MeshExportElementsList,ONLY: CreateMeshDataArrays
!----------------------------------------------------------------------------------------------------------------------!
USE MOD_MeshGeometry_vars,ONLY: ParametersMeshGeometry
!----------------------------------------------------------------------------------------------------------------------!
IMPLICIT NONE
!----------------------------------------------------------------------------------------------------------------------!
! FORMAL ARGUMENTS
!----------------------------------------------------------------------------------------------------------------------!
! LOCAL VARIABLES
!----------------------------------------------------------------------------------------------------------------------!
CHARACTER(LEN=256) :: BaseFileName
!----------------------------------------------------------------------------------------------------------------------!
CHARACTER(LEN=256) :: ErrorMessage
CHARACTER(LEN=256) :: FunctionName = "MeshAdaptation"
!----------------------------------------------------------------------------------------------------------------------!
INTEGER             :: iLevel
INTEGER             :: nNodes
INTEGER             :: nElems
INTEGER             :: MeshInfoData(1:3)
INTEGER,ALLOCATABLE :: nElemsPerLevel(:)
!----------------------------------------------------------------------------------------------------------------------!
CHARACTER(LEN=256) :: MeshInfoVarNames(1:3)
CHARACTER(LEN=256) :: Header
CHARACTER(LEN=:),ALLOCATABLE :: StrLine1
!----------------------------------------------------------------------------------------------------------------------!

CALL CreateSeparatingLine("-",StrLine1)

SELECT CASE(LowerCase(ParametersMeshAdaptation%WhichMeshConstructionMethod))
  CASE('mesh-import')
    CALL MeshImport()
  CASE('mesh-builtin')
    CALL MeshBuiltIn()
  CASE DEFAULT
  ErrorMessage = "MeshAdaptation: Unknown Mesh Construction Method"
  CALL PrintError(__STAMP__,ErrorMessage,ModuleName,FunctionName)
END SELECT

SELECT CASE(LowerCase(ParametersMeshRefinement%WhichMeshRefinement))
  CASE("refine-elements-around-geometry")
    SELECT CASE(LowerCase(ParametersMeshGeometry%WhichBodyGeometry))
      CASE("built-in-geometry")
        CALL GeometryBuiltIn()
      CASE("imported-geometry")
        CALL GeometryImport()
    END SELECT
  CASE("refine-elements-around-geometry-and-inside-box")
    SELECT CASE(LowerCase(ParametersMeshGeometry%WhichBodyGeometry))
      CASE("built-in-geometry")
        CALL GeometryBuiltIn()
      CASE("imported-geometry")
        CALL GeometryImport()
    END SELECT
END SELECT

BaseFileName = TRIM(MeshInfo%BaseFileName)

DO iLevel=0,MaxRefinementLevel
  WRITE(MeshInfo%BaseFileName,"(A,A,I0.2)") TRIM(BaseFileName), "_L", iLevel

  ! No mesh refinement at this level
  IF (iLevel .EQ. 0) THEN
    SWRITE(UNIT_SCREEN,*)
    Header = "CREATING MESH..."
    CALL PrintMessage(Header)

    !------------------------------!
    ! CREATING ELEMENT LIST
    !------------------------------!
    CALL CreateElementsList(Debug=.FALSE.)

    !------------------------------!
    ! CREATING MESH DATA ARRAYS
    !------------------------------!
    CALL CreateMeshDataArrays(Debug=.FALSE.)

    !------------------------------!
    ! EXPORTING THE MESH
    !------------------------------!
    CALL MeshExport()

    !------------------------------!
    ! MESH INFORMATION
    !------------------------------!
    CALL CountElems(ElemList,nElems)
    CALL CountNodes(ElemList,nNodes)

    MeshInfoData(1) = iLevel
    MeshInfoData(2) = nElems
    MeshInfoData(3) = nNodes
    MeshInfoVarNames(1) = "Level"
    MeshInfoVarNames(2) = "nElems"
    MeshInfoVarNames(3) = "nNodes"
    CALL PrintArrayInfo("MESH INFORMATION",MeshInfoVarNames,MeshInfoData,nTabIn=2)

  END IF

  ! Mesh refinement
  IF (iLevel .GT. 0) THEN

    SWRITE(UNIT_SCREEN,*)
    Header = "REFINING MESH..."
    CALL PrintMessage(Header)

    !------------------------------!
    ! DISTRIBUTING GEOMETRY
    !------------------------------!
    IF (ParametersMeshRefinement%nRefinedBox .GT. 0) THEN
      IF (iLevel .GT. ParametersMeshRefinement%MaxBoxRefinementLevel(1)) THEN
        IF (RefineMeshAroundGeometry .EQV. .TRUE.) THEN
          CALL DistributeSTLFacetsInElemList(iLevel)
        END IF
      END IF
    ELSE
      IF (RefineMeshAroundGeometry .EQV. .TRUE.) THEN
        CALL DistributeSTLFacetsInElemList(iLevel)
      END IF
    END IF

    !------------------------------!
    ! FLAGGING ELEMENTS
    !------------------------------!
    CALL InitializeElementsToRefine()
    CALL FlagElementsForRefinement(iLevel)
    CALL FlagElementsForBalancing(iLevel)

    !------------------------------!
    ! REFINING ELEMENTS
    !------------------------------!
    CALL RefineElements(iLevel)

    !------------------------------!
    ! CREATING MESH DATA ARRAYS
    !------------------------------!
    CALL CreateMeshDataArrays(Debug=.FALSE.)

    !------------------------------!
    ! EXPORTING THE MESH
    !------------------------------!
    CALL MeshExport()

    !------------------------------!
    ! MESH INFORMATION
    !------------------------------!
    CALL CountElems(ElemList,nElems)
    CALL CountNodes(ElemList,nNodes)
    CALL CountElemsPerLevel(ElemList,iLevel,nElemsPerLevel)

    MeshInfoData(1) = iLevel
    MeshInfoData(2) = nElems
    MeshInfoData(3) = nNodes
    MeshInfoVarNames(1) = "Level"
    MeshInfoVarNames(2) = "nElems"
    MeshInfoVarNames(3) = "nNodes"
    CALL PrintArrayInfo("MESH INFORMATION",MeshInfoVarNames,MeshInfoData,nTabIn=2)

  END IF
END DO


IF (MaxRefinementLevel .GT. 0) THEN
  SWRITE(UNIT_SCREEN,*)
  Header = "MESH STATISTICS"
  CALL PrintMessage(Header)
  DO iLevel=0,MaxRefinementLevel
    MeshInfoData(1) = iLevel
    MeshInfoData(2) = nElemsPerLevel(iLevel)
    MeshInfoVarNames(1) = "Level"
    MeshInfoVarNames(2) = "nElems"
    CALL PrintArrayInfo("nElemsPerLevel",MeshInfoVarNames(1:2),MeshInfoData(1:2))
  END DO
  SWRITE(UNIT_SCREEN,*)
END IF

!----------------------------------------------------------------------------------------------------------------------!
END SUBROUTINE MeshAdaptation
!======================================================================================================================!
!
!
!
!----------------------------------------------------------------------------------------------------------------------!
END MODULE MOD_MeshAdaptation
!======================================================================================================================!
