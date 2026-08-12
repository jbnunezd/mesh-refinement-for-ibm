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
MODULE MOD_MeshImport_vars
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
  SUBROUTINE MeshImport_GMSH_INT(&
    InputFile,&
    ElementsType,&
    ElementsToNodes,&
    NodesCoordinates,&
    BCFacesToMark,&
    BCFacesToNodes,&
    BCFacesElementType,&
    BoundaryMark,&
    BoundaryName,&
    Debug)
    IMPLICIT NONE
    CHARACTER(LEN=*),INTENT(IN)               :: InputFile
    INTEGER,ALLOCATABLE,INTENT(OUT)           :: ElementsType(:)
    INTEGER,ALLOCATABLE,INTENT(OUT)           :: ElementsToNodes(:,:)
    REAL,ALLOCATABLE,INTENT(OUT)              :: NodesCoordinates(:,:)
    INTEGER,ALLOCATABLE,INTENT(OUT)           :: BCFacesToMark(:)
    INTEGER,ALLOCATABLE,INTENT(OUT)           :: BCFacesToNodes(:,:)
    INTEGER,ALLOCATABLE,INTENT(OUT)           :: BCFacesElementType(:)
    INTEGER,ALLOCATABLE,INTENT(OUT)           :: BoundaryMark(:)
    CHARACTER(LEN=256),ALLOCATABLE,INTENT(IN) :: BoundaryName(:)
    LOGICAL,INTENT(IN),OPTIONAL               :: Debug
  END SUBROUTINE MeshImport_GMSH_INT
END INTERFACE
PROCEDURE(MeshImport_GMSH_INT),POINTER :: MeshImport_GMSH
!----------------------------------------------------------------------------------------------------------------------!
TYPE tParametersMeshImport
  LOGICAL :: RotateMesh
  REAL    :: RotationAngle3D(1:3)
  INTEGER,ALLOCATABLE            :: BCIndex(:)
  INTEGER,ALLOCATABLE            :: BoundaryMark(:)
  CHARACTER(LEN=256),ALLOCATABLE :: BoundaryName(:)
  CHARACTER(LEN=256) :: InputMeshFile
  CHARACTER(LEN=256) :: InputMeshFolder
  CHARACTER(LEN=256) :: InputMeshFileFormat
  LOGICAL            :: DebugMeshImport = .FALSE.
  LOGICAL            :: FixElementsOrientation = .FALSE.
END TYPE
!----------------------------------------------------------------------------------------------------------------------!
TYPE(tParametersMeshImport) :: ParametersMeshImport
!----------------------------------------------------------------------------------------------------------------------!
INTEGER             :: nDims
INTEGER             :: nElems
INTEGER             :: nNodes
INTEGER             :: NGeo
REAL                :: RotationAngle3D(1:3)
REAL,ALLOCATABLE    :: RotationMatrix3D(:,:,:)
LOGICAL             :: RotateMesh
INTEGER,ALLOCATABLE :: BCIDFromGMSH(:)
!----------------------------------------------------------------------------------------------------------------------!
! ! ! ! INTEGER :: GMSH_ElemType(1:7,1:2) = RESHAPE(&
! ! ! !   (/2,3,4,4,8,6,5,&
! ! ! !     1,3,4,4,6,5,5/),&
! ! ! !   (/7,2/),ORDER=[1,2])
!----------------------------------------------------------------------------------------------------------------------!
LOGICAL :: FixElementsOrientation = .FALSE.
LOGICAL :: InitializeMeshImportIsDone = .FALSE.
!----------------------------------------------------------------------------------------------------------------------!
!
!
!
!----------------------------------------------------------------------------------------------------------------------!
END MODULE MOD_MeshImport_vars
!======================================================================================================================!
