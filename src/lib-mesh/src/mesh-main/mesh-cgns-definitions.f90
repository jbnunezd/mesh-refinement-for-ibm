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
MODULE MOD_Mesh_CGNS_Definitions
!----------------------------------------------------------------------------------------------------------------------!
USE MOD_GLOBAL_vars
!----------------------------------------------------------------------------------------------------------------------!
IMPLICIT NONE
!----------------------------------------------------------------------------------------------------------------------!
PUBLIC
!----------------------------------------------------------------------------------------------------------------------!
! GLOBAL VARIABLES
!----------------------------------------------------------------------------------------------------------------------!
!
!----------------------------------------------------------------------------------------------------------------------!
INTERFACE GetnElemNodes
  MODULE PROCEDURE GetnElemNodes
END INTERFACE

INTERFACE GetnElemFaces
  MODULE PROCEDURE GetnElemFaces
END INTERFACE

INTERFACE GetnElemEdges
  MODULE PROCEDURE GetnElemEdges
END INTERFACE

INTERFACE GetnNodesOnFace
  MODULE PROCEDURE GetnNodesOnFace
END INTERFACE

INTERFACE GetNodesOnFace
  MODULE PROCEDURE GetNodesOnFace
END INTERFACE

INTERFACE GetNodesOnEdge
  MODULE PROCEDURE GetNodesOnEdge
END INTERFACE
!----------------------------------------------------------------------------------------------------------------------!
! ElemType={1}={edge}-{2-nodes}
! ElemType={2}={triangle}-{3-nodes}
! ElemType={3}={quadrangle}-{4-nodes}
! ElemType={4}={tetrahedron}-{4-nodes}
! ElemType={5}={hexahedron}-{8-nodes}
! ElemType={6}={prism}-{6-nodes}
! ElemType={7}={pyramid}-{5-nodes}
!----------------------------------------------------------------------------------------------------------------------!
INTEGER,PARAMETER :: ELEMTYPE_EDGE2  = 1
INTEGER,PARAMETER :: ELEMTYPE_TRI3   = 2
INTEGER,PARAMETER :: ELEMTYPE_QUAD4  = 3
INTEGER,PARAMETER :: ELEMTYPE_TETRA4 = 4
INTEGER,PARAMETER :: ELEMTYPE_HEXA8  = 5
INTEGER,PARAMETER :: ELEMTYPE_PRISM6 = 6
INTEGER,PARAMETER :: ELEMTYPE_PYRA5  = 7
!----------------------------------------------------------------------------------------------------------------------!
INTEGER :: CGNS_ElemToNumberOfNodes(1:7,1:2) = RESHAPE(&
  (/1,2,&
    2,3,&
    3,4,&
    4,4,&
    5,8,&
    6,6,&
    7,5/),&
  (/7,2/),ORDER=[2,1])
!----------------------------------------------------------------------------------------------------------------------!
INTEGER :: CGNS_ElemToNumberOfFaces(1:7,1:2) = RESHAPE(&
  (/1,1,&
    2,3,&
    3,4,&
    4,4,&
    5,6,&
    6,5,&
    7,5/),&
  (/7,2/),ORDER=[2,1])
!----------------------------------------------------------------------------------------------------------------------!
INTEGER :: CGNS_ElemToNumberOfEdges(1:7,1:2) = RESHAPE(&
  (/1,1,&
    2,3,&
    3,4,&
    4,6,&
    5,12,&
    6,9,&
    7,8/),&
  (/7,2/),ORDER=[2,1])
!----------------------------------------------------------------------------------------------------------------------!
INTEGER :: CGNS_FaceToNodes_EDGE2(1:1,1:2) = RESHAPE(&
  (/1,2/),&
  (/1,2/),ORDER=[2,1])
INTEGER :: CGNS_NodesPerFace_EDGE2(1:1) = (/2/)
!----------------------------------------------------------------------------------------------------------------------!
INTEGER :: CGNS_FaceToNodes_TRI3(1:3,1:2) = RESHAPE(&
  (/1,2,&
    2,3,&
    3,1/),&
  (/3,2/),ORDER=[2,1])
INTEGER :: CGNS_NodesPerFace_TRI3(1:3) = (/2,2,2/)
!----------------------------------------------------------------------------------------------------------------------!
INTEGER :: CGNS_FaceToNodes_QUAD4(1:4,1:2) = RESHAPE(&
  (/1,2,&
    2,3,&
    3,4,&
    4,1/),&
  (/4,2/),ORDER=[2,1])
INTEGER :: CGNS_NodesPerFace_QUAD4(1:4) = (/2,2,2,2/)
!----------------------------------------------------------------------------------------------------------------------!
INTEGER :: CGNS_FaceToNodes_TETRA4(1:4,1:3) = RESHAPE(&
  (/1,3,2,&
    1,2,4,&
    2,3,4,&
    3,1,4/),&
  (/4,3/),ORDER=[2,1])
INTEGER :: CGNS_NodesPerFace_TETRA4(1:4) = (/3,3,3,3/)
!----------------------------------------------------------------------------------------------------------------------!
INTEGER :: CGNS_FaceToNodes_HEXA8(1:6,1:4) = RESHAPE(&
  (/1,4,3,2,&
    1,2,6,5,&
    2,3,7,6,&
    3,4,8,7,&
    1,5,8,4,&
    5,6,7,8/),&
  (/6,4/),ORDER=[2,1])
INTEGER :: CGNS_NodesPerFace_HEXA8(1:6) = (/4,4,4,4,4,4/)
!----------------------------------------------------------------------------------------------------------------------!
INTEGER :: CGNS_FaceToNodes_PRISM6(1:5,1:4) = RESHAPE(&
  (/1,2,5,4,&
    2,3,6,5,&
    3,1,4,6,&
    1,3,2,-1,&
    4,5,6,-1/),&
  (/5,4/),ORDER=[2,1])
INTEGER :: CGNS_NodesPerFace_PRISM6(1:5) = (/4,4,4,3,3/)
!----------------------------------------------------------------------------------------------------------------------!
INTEGER :: CGNS_FaceToNodes_PYRAM5(1:5,1:4) = RESHAPE(&
  (/1,4,3,2,&
    1,2,5,-1,&
    2,3,5,-1,&
    3,4,5,-1,&
    4,1,5,-1/),&
  (/5,4/),ORDER=[2,1])
INTEGER :: CGNS_NodesPerFace_PYRAM5(1:5) = (/4,3,3,3,3/)
!----------------------------------------------------------------------------------------------------------------------!
INTEGER :: CGNS_EdgeToNodes_EDGE2(1:1,1:2) = RESHAPE(&
  (/1,2/),& ! E1
  (/1,2/),ORDER=[2,1])
!----------------------------------------------------------------------------------------------------------------------!
INTEGER :: CGNS_EdgeToNodes_TRI3(1:3,1:2) = RESHAPE(&
  (/1,2,&   ! E1
    2,3,&   ! E2
    3,1/),& ! E3
  (/3,2/),ORDER=[2,1])
!----------------------------------------------------------------------------------------------------------------------!
INTEGER :: CGNS_EdgeToNodes_QUAD4(1:4,1:2) = RESHAPE(&
  (/1,2,&   ! E1
    2,3,&   ! E2
    3,4,&   ! E3
    4,1/),& ! E4
  (/4,2/),ORDER=[2,1])
!----------------------------------------------------------------------------------------------------------------------!
INTEGER :: CGNS_EdgeToNodes_TETRA4(1:6,1:2) = RESHAPE(&
  (/1,2,&   ! E1
    2,3,&   ! E2
    3,1,&   ! E3
    1,4,&   ! E4
    2,4,&   ! E5
    3,4/),& ! E6
  (/6,2/),ORDER=[2,1])
!----------------------------------------------------------------------------------------------------------------------!
INTEGER :: CGNS_EdgeToNodes_HEXA8(1:12,1:2) = RESHAPE(&
  (/1,2,&   ! E1
    2,3,&   ! E2
    3,4,&   ! E3
    4,1,&   ! E4
    1,5,&   ! E5
    2,6,&   ! E6
    3,7,&   ! E7
    4,8,&   ! E8
    5,6,&   ! E9
    6,7,&   ! E10
    7,8,&   ! E11
    8,5/),& ! E12
  (/12,2/),ORDER=[2,1])
!----------------------------------------------------------------------------------------------------------------------!
INTEGER :: CGNS_EdgeToNodes_PRISM6(1:9,1:2) = RESHAPE(&
  (/1,2,&   ! E1
    2,3,&   ! E2
    3,1,&   ! E3
    1,4,&   ! E4
    2,5,&   ! E5
    3,6,&   ! E6
    4,5,&   ! E7
    5,6,&   ! E8
    6,4/),& ! E9
  (/9,2/),ORDER=[2,1])
!----------------------------------------------------------------------------------------------------------------------!
INTEGER :: CGNS_EdgeToNodes_PYRA5(1:8,1:2) = RESHAPE(&
  (/1,2,&   ! E1
    2,3,&   ! E2
    3,4,&   ! E3
    4,1,&   ! E4
    1,5,&   ! E5
    2,5,&   ! E6
    3,5,&   ! E7
    4,5/),& ! E8
  (/8,2/),ORDER=[2,1])
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
SUBROUTINE GetnElemNodes(ElemType,nElemNodes)
!----------------------------------------------------------------------------------------------------------------------!
IMPLICIT NONE
!----------------------------------------------------------------------------------------------------------------------!
! FORMAL ARGUMENTS
!----------------------------------------------------------------------------------------------------------------------!
INTEGER,INTENT(IN)  :: ElemType
INTEGER,INTENT(OUT) :: nElemNodes
!----------------------------------------------------------------------------------------------------------------------!
! LOCAL VARIABLES
!----------------------------------------------------------------------------------------------------------------------!

nElemNodes = CGNS_ElemToNumberOfNodes(ElemType,2)

!----------------------------------------------------------------------------------------------------------------------!
END SUBROUTINE GetnElemNodes
!======================================================================================================================!
!
!
!
!======================================================================================================================!
SUBROUTINE GetnElemFaces(ElemType,nElemFaces)
!----------------------------------------------------------------------------------------------------------------------!
IMPLICIT NONE
!----------------------------------------------------------------------------------------------------------------------!
! FORMAL ARGUMENTS
!----------------------------------------------------------------------------------------------------------------------!
INTEGER,INTENT(IN)  :: ElemType
INTEGER,INTENT(OUT) :: nElemFaces
!----------------------------------------------------------------------------------------------------------------------!
! LOCAL VARIABLES
!----------------------------------------------------------------------------------------------------------------------!

nElemFaces = CGNS_ElemToNumberOfFaces(ElemType,2)

!----------------------------------------------------------------------------------------------------------------------!
END SUBROUTINE GetnElemFaces
!======================================================================================================================!
!
!
!
!======================================================================================================================!
SUBROUTINE GetnNodesOnFace(ElemType,LocalFace,nNodesOnFace)
!----------------------------------------------------------------------------------------------------------------------!
IMPLICIT NONE
!----------------------------------------------------------------------------------------------------------------------!
! FORMAL ARGUMENTS
!----------------------------------------------------------------------------------------------------------------------!
INTEGER,INTENT(IN)  :: ElemType
INTEGER,INTENT(IN)  :: LocalFace
INTEGER,INTENT(OUT) :: nNodesOnFace
!----------------------------------------------------------------------------------------------------------------------!
! LOCAL VARIABLES
!----------------------------------------------------------------------------------------------------------------------!

SELECT CASE (ElemType)
  CASE(ELEMTYPE_EDGE2)
    nNodesOnFace = CGNS_NodesPerFace_EDGE2(LocalFace)
  CASE(ELEMTYPE_TRI3)
    nNodesOnFace = CGNS_NodesPerFace_TRI3(LocalFace)
  CASE(ELEMTYPE_QUAD4)
    nNodesOnFace = CGNS_NodesPerFace_QUAD4(LocalFace)
  CASE(ELEMTYPE_TETRA4)
    nNodesOnFace = CGNS_NodesPerFace_TETRA4(LocalFace)
  CASE(ELEMTYPE_HEXA8)
    nNodesOnFace = CGNS_NodesPerFace_HEXA8(LocalFace)
  CASE(ELEMTYPE_PRISM6)
    nNodesOnFace = CGNS_NodesPerFace_PRISM6(LocalFace)
  CASE(ELEMTYPE_PYRA5)
    nNodesOnFace = CGNS_NodesPerFace_PYRAM5(LocalFace)
END SELECT

!----------------------------------------------------------------------------------------------------------------------!
END SUBROUTINE GetnNodesOnFace
!======================================================================================================================!
!
!
!
!======================================================================================================================!
SUBROUTINE GetNodesOnFace(ElemType,LocalFace,NodesOnFace)
!----------------------------------------------------------------------------------------------------------------------!
IMPLICIT NONE
!----------------------------------------------------------------------------------------------------------------------!
! FORMAL ARGUMENTS
!----------------------------------------------------------------------------------------------------------------------!
INTEGER,INTENT(IN)              :: ElemType
INTEGER,INTENT(IN)              :: LocalFace
INTEGER,ALLOCATABLE,INTENT(OUT) :: NodesOnFace(:)
!----------------------------------------------------------------------------------------------------------------------!
! LOCAL VARIABLES
!----------------------------------------------------------------------------------------------------------------------!
INTEGER :: nNodesOnFace
!----------------------------------------------------------------------------------------------------------------------!

CALL GetnNodesOnFace(ElemType,LocalFace,nNodesOnFace)

SELECT CASE (ElemType)
  CASE(ELEMTYPE_EDGE2)
    IF (ALLOCATED(NodesOnFace)) THEN
      DEALLOCATE(NodesOnFace)
    END IF
    ALLOCATE(NodesOnFace(1:nNodesOnFace))
    NodesOnFace(1:nNodesOnFace) = CGNS_FaceToNodes_EDGE2(LocalFace,1:nNodesOnFace)
  CASE(ELEMTYPE_TRI3)
    IF (ALLOCATED(NodesOnFace)) THEN
      DEALLOCATE(NodesOnFace)
    END IF
    ALLOCATE(NodesOnFace(1:nNodesOnFace))
    NodesOnFace(1:nNodesOnFace) = CGNS_FaceToNodes_TRI3(LocalFace,1:nNodesOnFace)
  CASE(ELEMTYPE_QUAD4)
    IF (ALLOCATED(NodesOnFace)) THEN
      DEALLOCATE(NodesOnFace)
    END IF
    ALLOCATE(NodesOnFace(1:nNodesOnFace))
    NodesOnFace(1:nNodesOnFace) = CGNS_FaceToNodes_QUAD4(LocalFace,1:nNodesOnFace)
  CASE(ELEMTYPE_TETRA4)
    IF (ALLOCATED(NodesOnFace)) THEN
      DEALLOCATE(NodesOnFace)
    END IF
    ALLOCATE(NodesOnFace(1:nNodesOnFace))
    NodesOnFace(1:nNodesOnFace) = CGNS_FaceToNodes_TETRA4(LocalFace,1:nNodesOnFace)
  CASE(ELEMTYPE_HEXA8)
    IF (ALLOCATED(NodesOnFace)) THEN
      DEALLOCATE(NodesOnFace)
    END IF
    ALLOCATE(NodesOnFace(1:nNodesOnFace))
    NodesOnFace(1:nNodesOnFace) = CGNS_FaceToNodes_HEXA8(LocalFace,1:nNodesOnFace)
  CASE(ELEMTYPE_PRISM6)
    IF (ALLOCATED(NodesOnFace)) THEN
      DEALLOCATE(NodesOnFace)
    END IF
    ALLOCATE(NodesOnFace(1:nNodesOnFace))
    NodesOnFace(1:nNodesOnFace) = CGNS_FaceToNodes_PRISM6(LocalFace,1:nNodesOnFace)
  CASE(ELEMTYPE_PYRA5)
    IF (ALLOCATED(NodesOnFace)) THEN
      DEALLOCATE(NodesOnFace)
    END IF
    ALLOCATE(NodesOnFace(1:nNodesOnFace))
    NodesOnFace(1:nNodesOnFace) = CGNS_FaceToNodes_PYRAM5(LocalFace,1:nNodesOnFace)
END SELECT

!----------------------------------------------------------------------------------------------------------------------!
END SUBROUTINE GetNodesOnFace
!======================================================================================================================!
!
!
!
!======================================================================================================================!
SUBROUTINE GetnElemEdges(ElemType,nElemEdges)
!----------------------------------------------------------------------------------------------------------------------!
IMPLICIT NONE
!----------------------------------------------------------------------------------------------------------------------!
! FORMAL ARGUMENTS
!----------------------------------------------------------------------------------------------------------------------!
INTEGER,INTENT(IN)  :: ElemType
INTEGER,INTENT(OUT) :: nElemEdges
!----------------------------------------------------------------------------------------------------------------------!
! LOCAL VARIABLES
!----------------------------------------------------------------------------------------------------------------------!

nElemEdges = CGNS_ElemToNumberOfEdges(ElemType,2)

!----------------------------------------------------------------------------------------------------------------------!
END SUBROUTINE GetnElemEdges
!======================================================================================================================!
!
!
!
!======================================================================================================================!
SUBROUTINE GetNodesOnEdge(ElemType,LocalEdge,NodesOnEdge)
!----------------------------------------------------------------------------------------------------------------------!
IMPLICIT NONE
!----------------------------------------------------------------------------------------------------------------------!
! FORMAL ARGUMENTS
!----------------------------------------------------------------------------------------------------------------------!
INTEGER,INTENT(IN)  :: ElemType
INTEGER,INTENT(IN)  :: LocalEdge
INTEGER,INTENT(OUT) :: NodesOnEdge(1:2)
!----------------------------------------------------------------------------------------------------------------------!
! LOCAL VARIABLES
!----------------------------------------------------------------------------------------------------------------------!
INTEGER :: nNodesOnEdge = 2
!----------------------------------------------------------------------------------------------------------------------!

SELECT CASE (ElemType)
  CASE(ELEMTYPE_EDGE2)
    NodesOnEdge(1:nNodesOnEdge) = CGNS_EdgeToNodes_EDGE2(LocalEdge,1:nNodesOnEdge)
  CASE(ELEMTYPE_TRI3)
    NodesOnEdge(1:nNodesOnEdge) = CGNS_EdgeToNodes_TRI3(LocalEdge,1:nNodesOnEdge)
  CASE(ELEMTYPE_QUAD4)
    NodesOnEdge(1:nNodesOnEdge) = CGNS_EdgeToNodes_QUAD4(LocalEdge,1:nNodesOnEdge)
  CASE(ELEMTYPE_TETRA4)
    NodesOnEdge(1:nNodesOnEdge) = CGNS_EdgeToNodes_TETRA4(LocalEdge,1:nNodesOnEdge)
  CASE(ELEMTYPE_HEXA8)
    NodesOnEdge(1:nNodesOnEdge) = CGNS_EdgeToNodes_HEXA8(LocalEdge,1:nNodesOnEdge)
  CASE(ELEMTYPE_PRISM6)
    NodesOnEdge(1:nNodesOnEdge) = CGNS_EdgeToNodes_PRISM6(LocalEdge,1:nNodesOnEdge)
  CASE(ELEMTYPE_PYRA5)
    NodesOnEdge(1:nNodesOnEdge) = CGNS_EdgeToNodes_PYRA5(LocalEdge,1:nNodesOnEdge)
END SELECT

!----------------------------------------------------------------------------------------------------------------------!
END SUBROUTINE GetNodesOnEdge
!======================================================================================================================!
!
!
!
!----------------------------------------------------------------------------------------------------------------------!
END MODULE MOD_Mesh_CGNS_Definitions
!======================================================================================================================!
