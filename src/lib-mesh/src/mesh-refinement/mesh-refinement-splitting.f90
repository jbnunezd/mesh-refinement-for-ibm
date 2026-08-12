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
MODULE MOD_MeshRefinementSplitting
!----------------------------------------------------------------------------------------------------------------------!
USE MOD_GLOBAL_vars
!----------------------------------------------------------------------------------------------------------------------!
IMPLICIT NONE
!----------------------------------------------------------------------------------------------------------------------!
PRIVATE
!----------------------------------------------------------------------------------------------------------------------!
INTERFACE RefineElements
  MODULE PROCEDURE RefineElements
END INTERFACE
!----------------------------------------------------------------------------------------------------------------------!
PUBLIC :: RefineElements
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
SUBROUTINE RefineElements(Level)
!----------------------------------------------------------------------------------------------------------------------!
USE MOD_DataStructures,ONLY: LowerCase
!----------------------------------------------------------------------------------------------------------------------!
USE MOD_MeshMain_vars,ONLY: tElem
USE MOD_MeshMain_vars,ONLY: ElemList
USE MOD_MeshMain_vars,ONLY: MeshInfo
USE MOD_MeshMain_vars,ONLY: MeshData_ElementsToRefineFlag
!----------------------------------------------------------------------------------------------------------------------!
USE MOD_MeshRefinement_vars,ONLY: IsotropicRefinement
!----------------------------------------------------------------------------------------------------------------------!
USE MOD_MeshElementsList,ONLY: SetUniqueNodes
USE MOD_MeshElementsList,ONLY: SetUniqueElemID
USE MOD_MeshElementsList,ONLY: SetUniqueSideID
!----------------------------------------------------------------------------------------------------------------------!
USE MOD_Mesh_CGNS_Definitions,ONLY: ELEMTYPE_TRI3
USE MOD_Mesh_CGNS_Definitions,ONLY: ELEMTYPE_QUAD4
USE MOD_Mesh_CGNS_Definitions,ONLY: ELEMTYPE_TETRA4
USE MOD_Mesh_CGNS_Definitions,ONLY: ELEMTYPE_HEXA8
USE MOD_Mesh_CGNS_Definitions,ONLY: ELEMTYPE_PRISM6
USE MOD_Mesh_CGNS_Definitions,ONLY: ELEMTYPE_PYRA5
!----------------------------------------------------------------------------------------------------------------------!
IMPLICIT NONE
!----------------------------------------------------------------------------------------------------------------------!
! FORMAL ARGUMENTS
!----------------------------------------------------------------------------------------------------------------------!
INTEGER,INTENT(IN) :: Level
!----------------------------------------------------------------------------------------------------------------------!
! LOCAL VARIABLES
!----------------------------------------------------------------------------------------------------------------------!
TYPE(tElem),POINTER :: aElem
!----------------------------------------------------------------------------------------------------------------------!
INTEGER :: nTabIn
!----------------------------------------------------------------------------------------------------------------------!
REAL               :: CalcTimeIni
REAL               :: CalcTimeEnd
CHARACTER(LEN=256) :: ElapsedTime
CHARACTER(LEN=256) :: Header
!----------------------------------------------------------------------------------------------------------------------!

nTabIn = 2
Header = "REFINING ELEMENTS"
CALL PrintMessage(Header,nTabIn=nTabIn)

CalcTimeIni = RunningTime()

IF (IsotropicRefinement .EQV. .TRUE.) THEN
  aElem => ElemList%FirstElem
  DO WHILE (ASSOCIATED(aElem))
    IF (MeshData_ElementsToRefineFlag(aElem%ElemID) .EQV. .TRUE.) THEN
      SELECT CASE(aElem%ElemType)
        CASE(ELEMTYPE_TRI3)
          CALL SplitElem_TRI3(aElem)
        CASE(ELEMTYPE_QUAD4)
          CALL SplitElem_QUAD4(aElem)
        CASE(ELEMTYPE_TETRA4)
          CALL SplitElem_TETRA4(aElem)
        CASE(ELEMTYPE_HEXA8)
          CALL SplitElem_HEXA8(aElem)
        CASE(ELEMTYPE_PRISM6)
          CALL SplitElem_PRISM6(aElem)
        CASE(ELEMTYPE_PYRA5)
          CALL SplitElem_PYRA5(aElem)
      END SELECT
    END IF
    aElem => aElem%NextElem
  END DO
ELSE
  aElem => ElemList%FirstElem
  DO WHILE (ASSOCIATED(aElem))
    IF (MeshData_ElementsToRefineFlag(aElem%ElemID) .EQV. .TRUE.) THEN
      SELECT CASE(aElem%ElemType)
        CASE(ELEMTYPE_HEXA8)
          CALL SplitElem_HEXA8_XZ(aElem)
        CASE(ELEMTYPE_PRISM6)
          CALL SplitElem_PRISM6_XZ(aElem)
      END SELECT
    END IF
    aElem => aElem%NextElem
  END DO
END IF

MeshInfo%MaxRefLevel = Level

CALL SetUniqueNodes()
CALL SetUniqueElemID()
CALL SetUniqueSideID()

CalcTimeEnd = RunningTime()
CALL ComputeRuntime(CalcTimeEnd-CalcTimeIni,ElapsedTime)
Header = "Elapsed Time"
CALL PrintAnalyze(Header,ElapsedTime,nTabIn=4)

!----------------------------------------------------------------------------------------------------------------------!
END SUBROUTINE RefineElements
!======================================================================================================================!
!
!
!
!======================================================================================================================!
SUBROUTINE SplitElem_TRI3(Elem)
!----------------------------------------------------------------------------------------------------------------------!
USE MOD_MeshMain_vars,ONLY: tElem
USE MOD_MeshMain_vars,ONLY: tSide
USE MOD_MeshMain_vars,ONLY: tElemPtr
USE MOD_MeshMain_vars,ONLY: tNodePtr
USE MOD_MeshMain_vars,ONLY: tSidePtr
USE MOD_MeshMain_vars,ONLY: ElemList
!----------------------------------------------------------------------------------------------------------------------!
USE MOD_MeshMainMethods,ONLY: CountElems
USE MOD_MeshMainMethods,ONLY: CopyBC
USE MOD_MeshMainMethods,ONLY: RemoveElem
USE MOD_MeshMainMethods,ONLY: CreateNode
USE MOD_MeshMainMethods,ONLY: CreateElem_CGNS
!----------------------------------------------------------------------------------------------------------------------!
USE MOD_MeshMainMethods,ONLY: CountNodes
!----------------------------------------------------------------------------------------------------------------------!
USE MOD_Mesh_CGNS_Definitions
!----------------------------------------------------------------------------------------------------------------------!
IMPLICIT NONE
!----------------------------------------------------------------------------------------------------------------------!
! FORMAL ARGUMENTS
!----------------------------------------------------------------------------------------------------------------------!
TYPE(tElem),POINTER,INTENT(INOUT) :: Elem
!----------------------------------------------------------------------------------------------------------------------!
! LOCAL VARIABLES
!----------------------------------------------------------------------------------------------------------------------!
INTEGER :: ElemType
INTEGER :: LocSideID
INTEGER :: LocElemID
REAL    :: dh
!----------------------------------------------------------------------------------------------------------------------!
INTEGER,PARAMETER :: nElemFaces  = 3
INTEGER,PARAMETER :: nTotalElem  = 4
INTEGER,PARAMETER :: nElemNodes  = 3
INTEGER,PARAMETER :: nTotalNodes = 6
!----------------------------------------------------------------------------------------------------------------------!
TYPE(tElem),POINTER :: aElem
TYPE(tElem),POINTER :: PrevElem
TYPE(tElem),POINTER :: NextElem
TYPE(tSide),POINTER :: aSide
!----------------------------------------------------------------------------------------------------------------------!
TYPE(tNodePtr) :: ElemNodes(1:nElemNodes)
TYPE(tNodePtr) :: NewNodes(1:nTotalNodes)
TYPE(tElemPtr) :: NewElems(1:nTotalElem)
TYPE(tSidePtr) :: NewSides(1:nTotalElem,1:nElemFaces)
!----------------------------------------------------------------------------------------------------------------------!

! For splitting the TRI3 element into 4 TRI3 subelements,
! we use the CGNS-TRI6 nodes numbering

dh = 0.5

NewNodes(1)%Node => Elem%Nodes(1)%Node
NewNodes(2)%Node => Elem%Nodes(2)%Node
NewNodes(3)%Node => Elem%Nodes(3)%Node

! New midnodes at edges
CALL CreateNode(NewNodes(4)%Node)
CALL CreateNode(NewNodes(5)%Node)
CALL CreateNode(NewNodes(6)%Node)

NewNodes(4)%Node%Coords = dh*(NewNodes(1)%Node%Coords + NewNodes(2)%Node%Coords)
NewNodes(5)%Node%Coords = dh*(NewNodes(2)%Node%Coords + NewNodes(3)%Node%Coords)
NewNodes(6)%Node%Coords = dh*(NewNodes(1)%Node%Coords + NewNodes(3)%Node%Coords)

!--------------------------------------------------!
! Create Child = 1
!--------------------------------------------------!
LocElemID = 1
ElemType  = ELEMTYPE_TRI3
ElemNodes(1)%Node => NewNodes(1)%Node
ElemNodes(2)%Node => NewNodes(4)%Node
ElemNodes(3)%Node => NewNodes(6)%Node
CALL CreateElem_CGNS(NewElems(LocElemID)%Elem,ElemType,1,ElemNodes)
NewElems(LocElemID)%Elem%ElemType = ElemType
NewElems(LocElemID)%Elem%Flag     = Elem%Flag
NewElems(LocElemID)%Elem%Level    = Elem%Level+1

LocSideID = 0
aSide => NewElems(LocElemID)%Elem%FirstSide
DO WHILE (ASSOCIATED(aSide))
  LocSideID = LocSideID+1
  NewSides(LocElemID,LocSideID)%Side => aSide
  aSide => aSide%NextElemSide
END DO

!--------------------------------------------------!
! Create Child = 2
!--------------------------------------------------!
LocElemID = 2
ElemType  = ELEMTYPE_TRI3
ElemNodes(1)%Node => NewNodes(4)%Node
ElemNodes(2)%Node => NewNodes(2)%Node
ElemNodes(3)%Node => NewNodes(5)%Node
CALL CreateElem_CGNS(NewElems(LocElemID)%Elem,ElemType,1,ElemNodes)
NewElems(LocElemID)%Elem%ElemType = ElemType
NewElems(LocElemID)%Elem%Flag     = Elem%Flag
NewElems(LocElemID)%Elem%Level    = Elem%Level+1

LocSideID = 0
aSide => NewElems(LocElemID)%Elem%FirstSide
DO WHILE (ASSOCIATED(aSide))
  LocSideID = LocSideID+1
  NewSides(LocElemID,LocSideID)%Side => aSide
  aSide => aSide%NextElemSide
END DO

!--------------------------------------------------!
! Create Child = 3
!--------------------------------------------------!
LocElemID = 3
ElemType  = ELEMTYPE_TRI3
ElemNodes(1)%Node => NewNodes(6)%Node
ElemNodes(2)%Node => NewNodes(5)%Node
ElemNodes(3)%Node => NewNodes(3)%Node
CALL CreateElem_CGNS(NewElems(LocElemID)%Elem,ElemType,1,ElemNodes)
NewElems(LocElemID)%Elem%ElemType = ElemType
NewElems(LocElemID)%Elem%Flag     = Elem%Flag
NewElems(LocElemID)%Elem%Level    = Elem%Level+1

LocSideID = 0
aSide => NewElems(LocElemID)%Elem%FirstSide
DO WHILE (ASSOCIATED(aSide))
  LocSideID = LocSideID+1
  NewSides(LocElemID,LocSideID)%Side => aSide
  aSide => aSide%NextElemSide
END DO

!--------------------------------------------------!
! Create Child = 4
!--------------------------------------------------!
LocElemID = 4
ElemType  = ELEMTYPE_TRI3
ElemNodes(1)%Node => NewNodes(4)%Node
ElemNodes(2)%Node => NewNodes(5)%Node
ElemNodes(3)%Node => NewNodes(6)%Node
CALL CreateElem_CGNS(NewElems(LocElemID)%Elem,ElemType,1,ElemNodes)
NewElems(LocElemID)%Elem%ElemType = ElemType
NewElems(LocElemID)%Elem%Flag     = Elem%Flag
NewElems(LocElemID)%Elem%Level    = Elem%Level+1

LocSideID = 0
aSide => NewElems(LocElemID)%Elem%FirstSide
DO WHILE (ASSOCIATED(aSide))
  LocSideID = LocSideID+1
  NewSides(LocElemID,LocSideID)%Side => aSide
  aSide => aSide%NextElemSide
END DO

!--------------------------------------------------!
! Set Boundary Conditions
!--------------------------------------------------!
LocSideID = 0
aSide => Elem%FirstSide
DO WHILE (ASSOCIATED(aSide))
  LocSideID = LocSideID+1
  IF (ASSOCIATED(aSide%BC)) THEN
    DO LocElemID=1,nTotalElem
      SELECT CASE (LocElemID)
        CASE(1)
          SELECT CASE (LocSideID)
            CASE(1)
              CALL CopyBC(aSide,NewSides(LocElemID,1)%Side)
            CASE(3)
              CALL CopyBC(aSide,NewSides(LocElemID,3)%Side)
          END SELECT
        CASE(2)
          SELECT CASE (LocSideID)
            CASE(1)
              CALL CopyBC(aSide,NewSides(LocElemID,1)%Side)
            CASE(2)
              CALL CopyBC(aSide,NewSides(LocElemID,2)%Side)
          END SELECT
        CASE(3)
          SELECT CASE (LocSideID)
            CASE(2)
              CALL CopyBC(aSide,NewSides(LocElemID,2)%Side)
            CASE(3)
              CALL CopyBC(aSide,NewSides(LocElemID,3)%Side)
          END SELECT
      END SELECT
    END DO
  END IF
  aSide => aSide%NextElemSide
END DO

aElem    => Elem
NextElem => Elem%NextElem
PrevElem => Elem%PrevElem

DO LocElemID=1,nTotalElem
  aElem%NextElem          => NewElems(LocElemID)%Elem
  aElem%NextElem%PrevElem => aElem
  aElem                   => aElem%NextElem
END DO

IF (ASSOCIATED(NextElem)) THEN
  aElem%NextElem    => NextElem
  NextElem%PrevElem => aElem
END IF

CALL RemoveElem(Elem,ElemList)

Elem => aElem

IF (ASSOCIATED(PrevElem)) THEN
  NewElems(1)%Elem%PrevElem => PrevElem
  PrevElem%NextElem         => NewElems(1)%Elem
ELSE
  ElemList%FirstElem => NewElems(1)%Elem
END IF

!----------------------------------------------------------------------------------------------------------------------!
END SUBROUTINE SplitElem_TRI3
!======================================================================================================================!
!
!
!
!======================================================================================================================!
SUBROUTINE SplitElem_QUAD4(Elem)
!----------------------------------------------------------------------------------------------------------------------!
USE MOD_MeshMain_vars,ONLY: tElem
USE MOD_MeshMain_vars,ONLY: tSide
USE MOD_MeshMain_vars,ONLY: tElemPtr
USE MOD_MeshMain_vars,ONLY: tNodePtr
USE MOD_MeshMain_vars,ONLY: tSidePtr
USE MOD_MeshMain_vars,ONLY: ElemList
!----------------------------------------------------------------------------------------------------------------------!
USE MOD_MeshMainMethods,ONLY: CountElems
USE MOD_MeshMainMethods,ONLY: CopyBC
USE MOD_MeshMainMethods,ONLY: RemoveElem
USE MOD_MeshMainMethods,ONLY: CreateNode
USE MOD_MeshMainMethods,ONLY: CreateElem_CGNS
!----------------------------------------------------------------------------------------------------------------------!
USE MOD_Mesh_CGNS_Definitions
!----------------------------------------------------------------------------------------------------------------------!
IMPLICIT NONE
!----------------------------------------------------------------------------------------------------------------------!
! FORMAL ARGUMENTS
!----------------------------------------------------------------------------------------------------------------------!
TYPE(tElem),POINTER,INTENT(INOUT) :: Elem
!----------------------------------------------------------------------------------------------------------------------!
! LOCAL VARIABLES
!----------------------------------------------------------------------------------------------------------------------!
INTEGER :: ElemType
INTEGER :: LocSideID
INTEGER :: LocElemID
REAL    :: dh
!----------------------------------------------------------------------------------------------------------------------!
INTEGER,PARAMETER :: nElemFaces  = 4
INTEGER,PARAMETER :: nTotalElem  = 4
INTEGER,PARAMETER :: nElemNodes  = 4
INTEGER,PARAMETER :: nTotalNodes = 9
!----------------------------------------------------------------------------------------------------------------------!
TYPE(tElem),POINTER :: aElem
TYPE(tElem),POINTER :: PrevElem
TYPE(tElem),POINTER :: NextElem
TYPE(tSide),POINTER :: aSide
!----------------------------------------------------------------------------------------------------------------------!
TYPE(tElemPtr) :: NewElems(1:nTotalElem)
TYPE(tNodePtr) :: NewNodes(1:nTotalNodes)
TYPE(tSidePtr) :: NewSides(1:nTotalElem,1:nElemFaces)
TYPE(tNodePtr) :: ElemNodes(1:nElemNodes)
!----------------------------------------------------------------------------------------------------------------------!

! For splitting the QUAD4 element into 4 QUAD4 subelements,
! we use the CGNS-QUAD9 nodes numbering

dh = 0.5

NewNodes(1)%Node => Elem%Nodes(1)%Node
NewNodes(2)%Node => Elem%Nodes(2)%Node
NewNodes(3)%Node => Elem%Nodes(3)%Node
NewNodes(4)%Node => Elem%Nodes(4)%Node

! New midnodes at edges
CALL CreateNode(NewNodes(5)%Node)
CALL CreateNode(NewNodes(6)%Node)
CALL CreateNode(NewNodes(7)%Node)
CALL CreateNode(NewNodes(8)%Node)

NewNodes(5)%Node%Coords = dh*(NewNodes(1)%Node%Coords + NewNodes(2)%Node%Coords)
NewNodes(6)%Node%Coords = dh*(NewNodes(2)%Node%Coords + NewNodes(3)%Node%Coords)
NewNodes(7)%Node%Coords = dh*(NewNodes(3)%Node%Coords + NewNodes(4)%Node%Coords)
NewNodes(8)%Node%Coords = dh*(NewNodes(1)%Node%Coords + NewNodes(4)%Node%Coords)

! New node at inner element ******** WARNING ******** CENTROID OF QUADRILATERAL ******** WARNING ********
CALL CreateNode(NewNodes(9)%Node)
NewNodes(9)%Node%Coords = dh*(NewNodes(1)%Node%Coords + NewNodes(3)%Node%Coords)

!--------------------------------------------------!
! Create Child = 1
!--------------------------------------------------!
LocElemID = 1
ElemType  = ELEMTYPE_QUAD4
ElemNodes(1)%Node => NewNodes(1)%Node
ElemNodes(2)%Node => NewNodes(5)%Node
ElemNodes(3)%Node => NewNodes(9)%Node
ElemNodes(4)%Node => NewNodes(8)%Node
CALL CreateElem_CGNS(NewElems(LocElemID)%Elem,ElemType,1,ElemNodes)
NewElems(LocElemID)%Elem%ElemType = ElemType
NewElems(LocElemID)%Elem%Flag     = Elem%Flag
NewElems(LocElemID)%Elem%Level    = Elem%Level+1

LocSideID = 0
aSide => NewElems(LocElemID)%Elem%FirstSide
DO WHILE (ASSOCIATED(aSide))
  LocSideID = LocSideID+1
  NewSides(LocElemID,LocSideID)%Side => aSide
  aSide => aSide%NextElemSide
END DO

!--------------------------------------------------!
! Create Child = 2
!--------------------------------------------------!
LocElemID = 2
ElemType  = ELEMTYPE_QUAD4
ElemNodes(1)%Node => NewNodes(5)%Node
ElemNodes(2)%Node => NewNodes(2)%Node
ElemNodes(3)%Node => NewNodes(6)%Node
ElemNodes(4)%Node => NewNodes(9)%Node
CALL CreateElem_CGNS(NewElems(LocElemID)%Elem,ElemType,1,ElemNodes)
NewElems(LocElemID)%Elem%ElemType = ElemType
NewElems(LocElemID)%Elem%Flag     = Elem%Flag
NewElems(LocElemID)%Elem%Level    = Elem%Level+1

LocSideID = 0
aSide => NewElems(LocElemID)%Elem%FirstSide
DO WHILE (ASSOCIATED(aSide))
  LocSideID = LocSideID+1
  NewSides(LocElemID,LocSideID)%Side => aSide
  aSide => aSide%NextElemSide
END DO

!--------------------------------------------------!
! Create Child = 3
!--------------------------------------------------!
LocElemID = 3
ElemType  = ELEMTYPE_QUAD4
ElemNodes(1)%Node => NewNodes(8)%Node
ElemNodes(2)%Node => NewNodes(9)%Node
ElemNodes(3)%Node => NewNodes(7)%Node
ElemNodes(4)%Node => NewNodes(4)%Node
CALL CreateElem_CGNS(NewElems(LocElemID)%Elem,ElemType,1,ElemNodes)
NewElems(LocElemID)%Elem%ElemType = ElemType
NewElems(LocElemID)%Elem%Flag     = Elem%Flag
NewElems(LocElemID)%Elem%Level    = Elem%Level+1

LocSideID = 0
aSide => NewElems(LocElemID)%Elem%FirstSide
DO WHILE (ASSOCIATED(aSide))
  LocSideID = LocSideID+1
  NewSides(LocElemID,LocSideID)%Side => aSide
  aSide => aSide%NextElemSide
END DO

!--------------------------------------------------!
! Create Child = 4
!--------------------------------------------------!
LocElemID = 4
ElemType  = ELEMTYPE_QUAD4
ElemNodes(1)%Node => NewNodes(9)%Node
ElemNodes(2)%Node => NewNodes(6)%Node
ElemNodes(3)%Node => NewNodes(3)%Node
ElemNodes(4)%Node => NewNodes(7)%Node
CALL CreateElem_CGNS(NewElems(LocElemID)%Elem,ElemType,1,ElemNodes)
NewElems(LocElemID)%Elem%ElemType = ElemType
NewElems(LocElemID)%Elem%Flag     = Elem%Flag
NewElems(LocElemID)%Elem%Level    = Elem%Level+1

LocSideID = 0
aSide => NewElems(LocElemID)%Elem%FirstSide
DO WHILE (ASSOCIATED(aSide))
  LocSideID = LocSideID+1
  NewSides(LocElemID,LocSideID)%Side => aSide
  aSide => aSide%NextElemSide
END DO

!--------------------------------------------------!
! Set Boundary Conditions
!--------------------------------------------------!
LocSideID = 0
aSide => Elem%FirstSide
DO WHILE (ASSOCIATED(aSide))
  LocSideID = LocSideID+1
  IF (ASSOCIATED(aSide%BC)) THEN
    DO LocElemID=1,nTotalElem
      SELECT CASE (LocElemID)
        CASE(1)
          SELECT CASE (LocSideID)
            CASE(1)
              CALL CopyBC(aSide,NewSides(LocElemID,1)%Side)
            CASE(4)
              CALL CopyBC(aSide,NewSides(LocElemID,4)%Side)
          END SELECT
        CASE(2)
          SELECT CASE (LocSideID)
            CASE(1)
              CALL CopyBC(aSide,NewSides(LocElemID,1)%Side)
            CASE(2)
              CALL CopyBC(aSide,NewSides(LocElemID,2)%Side)
          END SELECT
        CASE(3)
          SELECT CASE (LocSideID)
            CASE(3)
              CALL CopyBC(aSide,NewSides(LocElemID,3)%Side)
            CASE(4)
              CALL CopyBC(aSide,NewSides(LocElemID,4)%Side)
          END SELECT
        CASE(4)
          SELECT CASE (LocSideID)
            CASE(2)
              CALL CopyBC(aSide,NewSides(LocElemID,2)%Side)
            CASE(3)
              CALL CopyBC(aSide,NewSides(LocElemID,3)%Side)
          END SELECT
      END SELECT
    END DO
  END IF
  aSide => aSide%NextElemSide
END DO

aElem    => Elem
NextElem => Elem%NextElem
PrevElem => Elem%PrevElem

DO LocElemID=1,nTotalElem
  aElem%NextElem          => NewElems(LocElemID)%Elem
  aElem%NextElem%PrevElem => aElem
  aElem                   => aElem%NextElem
END DO

IF (ASSOCIATED(NextElem)) THEN
  aElem%NextElem    => NextElem
  NextElem%PrevElem => aElem
END IF

CALL RemoveElem(Elem,ElemList)

Elem => aElem

IF (ASSOCIATED(PrevElem)) THEN
  NewElems(1)%Elem%PrevElem => PrevElem
  PrevElem%NextElem         => NewElems(1)%Elem
ELSE
  ElemList%FirstElem => NewElems(1)%Elem
END IF

!----------------------------------------------------------------------------------------------------------------------!
END SUBROUTINE SplitElem_QUAD4
!======================================================================================================================!
!
!
!
!======================================================================================================================!
SUBROUTINE SplitElem_TETRA4(Elem)
!----------------------------------------------------------------------------------------------------------------------!
USE MOD_MeshMain_vars,ONLY: tElem
USE MOD_MeshMain_vars,ONLY: tSide
USE MOD_MeshMain_vars,ONLY: tElemPtr
USE MOD_MeshMain_vars,ONLY: tNodePtr
USE MOD_MeshMain_vars,ONLY: tSidePtr
USE MOD_MeshMain_vars,ONLY: ElemList
!----------------------------------------------------------------------------------------------------------------------!
USE MOD_MeshMainMethods,ONLY: CountElems
USE MOD_MeshMainMethods,ONLY: CopyBC
USE MOD_MeshMainMethods,ONLY: RemoveElem
USE MOD_MeshMainMethods,ONLY: CreateNode
USE MOD_MeshMainMethods,ONLY: CreateElem_CGNS
!----------------------------------------------------------------------------------------------------------------------!
USE MOD_Mesh_CGNS_Definitions
!----------------------------------------------------------------------------------------------------------------------!
IMPLICIT NONE
!----------------------------------------------------------------------------------------------------------------------!
! FORMAL ARGUMENTS
!----------------------------------------------------------------------------------------------------------------------!
TYPE(tElem),POINTER,INTENT(INOUT) :: Elem
!----------------------------------------------------------------------------------------------------------------------!
! LOCAL VARIABLES
!----------------------------------------------------------------------------------------------------------------------!
INTEGER :: ElemType
INTEGER :: LocSideID
INTEGER :: LocElemID
REAL    :: dh
!----------------------------------------------------------------------------------------------------------------------!
INTEGER,PARAMETER :: nElemFaces  = 4
INTEGER,PARAMETER :: nTotalElem  = 8
INTEGER,PARAMETER :: nElemNodes  = 4
INTEGER,PARAMETER :: nTotalNodes = 10
!----------------------------------------------------------------------------------------------------------------------!
TYPE(tElem),POINTER :: aElem
TYPE(tElem),POINTER :: PrevElem
TYPE(tElem),POINTER :: NextElem
TYPE(tSide),POINTER :: aSide
!----------------------------------------------------------------------------------------------------------------------!
TYPE(tElemPtr) :: NewElems(1:nTotalElem)
TYPE(tNodePtr) :: NewNodes(1:nTotalNodes)
TYPE(tSidePtr) :: NewSides(1:nTotalElem,1:nElemFaces)
TYPE(tNodePtr) :: ElemNodes(1:nElemNodes)
!----------------------------------------------------------------------------------------------------------------------!

! For splitting the TETRA4 element into 8 TETRA4 subelements,
! we use the refinement rule of Bey (see bey1995a)

dh = 0.5

NewNodes(1)%Node => Elem%Nodes(1)%Node
NewNodes(2)%Node => Elem%Nodes(2)%Node
NewNodes(3)%Node => Elem%Nodes(3)%Node
NewNodes(4)%Node => Elem%Nodes(4)%Node

! New midnodes at edges
CALL CreateNode(NewNodes(5)%Node)
CALL CreateNode(NewNodes(6)%Node)
CALL CreateNode(NewNodes(7)%Node)
CALL CreateNode(NewNodes(8)%Node)
CALL CreateNode(NewNodes(9)%Node)
CALL CreateNode(NewNodes(10)%Node)

NewNodes(5)%Node%Coords  = dh*(NewNodes(1)%Node%Coords + NewNodes(2)%Node%Coords)
NewNodes(6)%Node%Coords  = dh*(NewNodes(2)%Node%Coords + NewNodes(3)%Node%Coords)
NewNodes(7)%Node%Coords  = dh*(NewNodes(1)%Node%Coords + NewNodes(3)%Node%Coords)
NewNodes(8)%Node%Coords  = dh*(NewNodes(1)%Node%Coords + NewNodes(4)%Node%Coords)
NewNodes(9)%Node%Coords  = dh*(NewNodes(2)%Node%Coords + NewNodes(4)%Node%Coords)
NewNodes(10)%Node%Coords = dh*(NewNodes(3)%Node%Coords + NewNodes(4)%Node%Coords)

!--------------------------------------------------!
! Create Child = 1
!--------------------------------------------------!
LocElemID = 1
ElemType  = ELEMTYPE_TETRA4
ElemNodes(1)%Node => NewNodes(1)%Node
ElemNodes(2)%Node => NewNodes(5)%Node
ElemNodes(3)%Node => NewNodes(7)%Node
ElemNodes(4)%Node => NewNodes(8)%Node
CALL CreateElem_CGNS(NewElems(LocElemID)%Elem,ElemType,1,ElemNodes)
NewElems(LocElemID)%Elem%ElemType = ElemType
NewElems(LocElemID)%Elem%Flag     = Elem%Flag
NewElems(LocElemID)%Elem%Level    = Elem%Level+1

LocSideID = 0
aSide => NewElems(LocElemID)%Elem%FirstSide
DO WHILE (ASSOCIATED(aSide))
  LocSideID = LocSideID+1
  NewSides(LocElemID,LocSideID)%Side => aSide
  aSide => aSide%NextElemSide
END DO

!--------------------------------------------------!
! Create Child = 2
!--------------------------------------------------!
LocElemID = 2
ElemType  = ELEMTYPE_TETRA4
ElemNodes(1)%Node => NewNodes(5)%Node
ElemNodes(2)%Node => NewNodes(2)%Node
ElemNodes(3)%Node => NewNodes(6)%Node
ElemNodes(4)%Node => NewNodes(9)%Node
CALL CreateElem_CGNS(NewElems(LocElemID)%Elem,ElemType,1,ElemNodes)
NewElems(LocElemID)%Elem%ElemType = ElemType
NewElems(LocElemID)%Elem%Flag     = Elem%Flag
NewElems(LocElemID)%Elem%Level    = Elem%Level+1

LocSideID = 0
aSide => NewElems(LocElemID)%Elem%FirstSide
DO WHILE (ASSOCIATED(aSide))
  LocSideID = LocSideID+1
  NewSides(LocElemID,LocSideID)%Side => aSide
  aSide => aSide%NextElemSide
END DO

!--------------------------------------------------!
! Create Child = 3
!--------------------------------------------------!
LocElemID = 3
ElemType  = ELEMTYPE_TETRA4
ElemNodes(1)%Node => NewNodes(6)%Node
ElemNodes(2)%Node => NewNodes(3)%Node
ElemNodes(3)%Node => NewNodes(7)%Node
ElemNodes(4)%Node => NewNodes(10)%Node
CALL CreateElem_CGNS(NewElems(LocElemID)%Elem,ElemType,1,ElemNodes)
NewElems(LocElemID)%Elem%ElemType = ElemType
NewElems(LocElemID)%Elem%Flag     = Elem%Flag
NewElems(LocElemID)%Elem%Level    = Elem%Level+1

LocSideID = 0
aSide => NewElems(LocElemID)%Elem%FirstSide
DO WHILE (ASSOCIATED(aSide))
  LocSideID = LocSideID+1
  NewSides(LocElemID,LocSideID)%Side => aSide
  aSide => aSide%NextElemSide
END DO

!--------------------------------------------------!
! Create Child = 4
!--------------------------------------------------!
LocElemID = 4
ElemType  = ELEMTYPE_TETRA4
ElemNodes(1)%Node => NewNodes(8)%Node
ElemNodes(2)%Node => NewNodes(9)%Node
ElemNodes(3)%Node => NewNodes(10)%Node
ElemNodes(4)%Node => NewNodes(4)%Node
CALL CreateElem_CGNS(NewElems(LocElemID)%Elem,ElemType,1,ElemNodes)
NewElems(LocElemID)%Elem%ElemType = ElemType
NewElems(LocElemID)%Elem%Flag     = Elem%Flag
NewElems(LocElemID)%Elem%Level    = Elem%Level+1

LocSideID = 0
aSide => NewElems(LocElemID)%Elem%FirstSide
DO WHILE (ASSOCIATED(aSide))
  LocSideID = LocSideID+1
  NewSides(LocElemID,LocSideID)%Side => aSide
  aSide => aSide%NextElemSide
END DO

!--------------------------------------------------!
! Create Child = 5
!--------------------------------------------------!
LocElemID = 5
ElemType  = ELEMTYPE_TETRA4
ElemNodes(1)%Node => NewNodes(5)%Node
ElemNodes(2)%Node => NewNodes(7)%Node
ElemNodes(3)%Node => NewNodes(8)%Node
ElemNodes(4)%Node => NewNodes(9)%Node
CALL CreateElem_CGNS(NewElems(LocElemID)%Elem,ElemType,1,ElemNodes)
NewElems(LocElemID)%Elem%ElemType = ElemType
NewElems(LocElemID)%Elem%Flag     = Elem%Flag
NewElems(LocElemID)%Elem%Level    = Elem%Level+1

LocSideID = 0
aSide => NewElems(LocElemID)%Elem%FirstSide
DO WHILE (ASSOCIATED(aSide))
  LocSideID = LocSideID+1
  NewSides(LocElemID,LocSideID)%Side => aSide
  aSide => aSide%NextElemSide
END DO

!--------------------------------------------------!
! Create Child = 6
!--------------------------------------------------!
LocElemID = 6
ElemType  = ELEMTYPE_TETRA4
ElemNodes(1)%Node => NewNodes(5)%Node
ElemNodes(2)%Node => NewNodes(6)%Node
ElemNodes(3)%Node => NewNodes(7)%Node
ElemNodes(4)%Node => NewNodes(9)%Node
CALL CreateElem_CGNS(NewElems(LocElemID)%Elem,ElemType,1,ElemNodes)
NewElems(LocElemID)%Elem%ElemType = ElemType
NewElems(LocElemID)%Elem%Flag     = Elem%Flag
NewElems(LocElemID)%Elem%Level    = Elem%Level+1

LocSideID = 0
aSide => NewElems(LocElemID)%Elem%FirstSide
DO WHILE (ASSOCIATED(aSide))
  LocSideID = LocSideID+1
  NewSides(LocElemID,LocSideID)%Side => aSide
  aSide => aSide%NextElemSide
END DO

!--------------------------------------------------!
! Create Child = 7
!--------------------------------------------------!
LocElemID = 7
ElemType  = ELEMTYPE_TETRA4
ElemNodes(1)%Node => NewNodes(7)%Node
ElemNodes(2)%Node => NewNodes(8)%Node
ElemNodes(3)%Node => NewNodes(9)%Node
ElemNodes(4)%Node => NewNodes(10)%Node
CALL CreateElem_CGNS(NewElems(LocElemID)%Elem,ElemType,1,ElemNodes)
NewElems(LocElemID)%Elem%ElemType = ElemType
NewElems(LocElemID)%Elem%Flag     = Elem%Flag
NewElems(LocElemID)%Elem%Level    = Elem%Level+1

LocSideID = 0
aSide => NewElems(LocElemID)%Elem%FirstSide
DO WHILE (ASSOCIATED(aSide))
  LocSideID = LocSideID+1
  NewSides(LocElemID,LocSideID)%Side => aSide
  aSide => aSide%NextElemSide
END DO

!--------------------------------------------------!
! Create Child = 8
!--------------------------------------------------!
LocElemID = 8
ElemType  = ELEMTYPE_TETRA4
ElemNodes(1)%Node => NewNodes(7)%Node
ElemNodes(2)%Node => NewNodes(9)%Node
ElemNodes(3)%Node => NewNodes(6)%Node
ElemNodes(4)%Node => NewNodes(10)%Node
CALL CreateElem_CGNS(NewElems(LocElemID)%Elem,ElemType,1,ElemNodes)
NewElems(LocElemID)%Elem%ElemType = ElemType
NewElems(LocElemID)%Elem%Flag     = Elem%Flag
NewElems(LocElemID)%Elem%Level    = Elem%Level+1

LocSideID = 0
aSide => NewElems(LocElemID)%Elem%FirstSide
DO WHILE (ASSOCIATED(aSide))
  LocSideID = LocSideID+1
  NewSides(LocElemID,LocSideID)%Side => aSide
  aSide => aSide%NextElemSide
END DO

!--------------------------------------------------!
! Set Boundary Conditions
!--------------------------------------------------!
LocSideID = 0
aSide => Elem%FirstSide
DO WHILE (ASSOCIATED(aSide))
  LocSideID = LocSideID+1
  IF (ASSOCIATED(aSide%BC)) THEN
    DO LocElemID=1,nTotalElem
      SELECT CASE (LocElemID)
        CASE(1)
          SELECT CASE (LocSideID)
            CASE(1)
              CALL CopyBC(aSide,NewSides(LocElemID,1)%Side)
            CASE(2)
              CALL CopyBC(aSide,NewSides(LocElemID,2)%Side)
            CASE(4)
              CALL CopyBC(aSide,NewSides(LocElemID,4)%Side)
          END SELECT
        CASE(2)
          SELECT CASE (LocSideID)
            CASE(1)
              CALL CopyBC(aSide,NewSides(LocElemID,1)%Side)
            CASE(2)
              CALL CopyBC(aSide,NewSides(LocElemID,2)%Side)
            CASE(3)
              CALL CopyBC(aSide,NewSides(LocElemID,3)%Side)
          END SELECT
        CASE(3)
          SELECT CASE (LocSideID)
            CASE(1)
              CALL CopyBC(aSide,NewSides(LocElemID,1)%Side)
            CASE(3)
              CALL CopyBC(aSide,NewSides(LocElemID,2)%Side)
            CASE(4)
              CALL CopyBC(aSide,NewSides(LocElemID,3)%Side)
          END SELECT
        CASE(4)
          SELECT CASE (LocSideID)
            CASE(2)
              CALL CopyBC(aSide,NewSides(LocElemID,2)%Side)
            CASE(3)
              CALL CopyBC(aSide,NewSides(LocElemID,3)%Side)
            CASE(4)
              CALL CopyBC(aSide,NewSides(LocElemID,4)%Side)
          END SELECT
        CASE(5)
          SELECT CASE (LocSideID)
            CASE(2)
              CALL CopyBC(aSide,NewSides(LocElemID,4)%Side)
          END SELECT
        CASE(6)
          SELECT CASE (LocSideID)
            CASE(1)
              CALL CopyBC(aSide,NewSides(LocElemID,1)%Side)
          END SELECT
        CASE(7)
          SELECT CASE (LocSideID)
            CASE(4)
              CALL CopyBC(aSide,NewSides(LocElemID,2)%Side)
          END SELECT
        CASE(8)
          SELECT CASE (LocSideID)
            CASE(3)
              CALL CopyBC(aSide,NewSides(LocElemID,3)%Side)
          END SELECT
      END SELECT
    END DO
  END IF
  aSide => aSide%NextElemSide
END DO

aElem    => Elem
NextElem => Elem%NextElem
PrevElem => Elem%PrevElem

DO LocElemID=1,nTotalElem
  aElem%NextElem          => NewElems(LocElemID)%Elem
  aElem%NextElem%PrevElem => aElem
  aElem                   => aElem%NextElem
END DO

IF (ASSOCIATED(NextElem)) THEN
  aElem%NextElem    => NextElem
  NextElem%PrevElem => aElem
END IF

CALL RemoveElem(Elem,ElemList)

Elem => aElem

IF (ASSOCIATED(PrevElem)) THEN
  NewElems(1)%Elem%PrevElem => PrevElem
  PrevElem%NextElem         => NewElems(1)%Elem
ELSE
  ElemList%FirstElem => NewElems(1)%Elem
END IF

!----------------------------------------------------------------------------------------------------------------------!
END SUBROUTINE SplitElem_TETRA4
!======================================================================================================================!
!
!
!
!======================================================================================================================!
SUBROUTINE SplitElem_HEXA8(Elem)
!----------------------------------------------------------------------------------------------------------------------!
USE MOD_MeshMain_vars,ONLY: tElem
USE MOD_MeshMain_vars,ONLY: tSide
USE MOD_MeshMain_vars,ONLY: tElemPtr
USE MOD_MeshMain_vars,ONLY: tNodePtr
USE MOD_MeshMain_vars,ONLY: tSidePtr
USE MOD_MeshMain_vars,ONLY: ElemList
!----------------------------------------------------------------------------------------------------------------------!
USE MOD_MeshMainMethods,ONLY: CountElems
USE MOD_MeshMainMethods,ONLY: CopyBC
USE MOD_MeshMainMethods,ONLY: RemoveElem
USE MOD_MeshMainMethods,ONLY: CreateNode
USE MOD_MeshMainMethods,ONLY: CreateElem_CGNS
!----------------------------------------------------------------------------------------------------------------------!
USE MOD_Mesh_CGNS_Definitions
!----------------------------------------------------------------------------------------------------------------------!
IMPLICIT NONE
!----------------------------------------------------------------------------------------------------------------------!
! FORMAL ARGUMENTS
!----------------------------------------------------------------------------------------------------------------------!
TYPE(tElem),POINTER,INTENT(INOUT) :: Elem
!----------------------------------------------------------------------------------------------------------------------!
! LOCAL VARIABLES
!----------------------------------------------------------------------------------------------------------------------!
INTEGER :: ElemType
INTEGER :: LocSideID
INTEGER :: LocElemID
REAL    :: dh
!----------------------------------------------------------------------------------------------------------------------!
INTEGER,PARAMETER :: nElemFaces  = 6
INTEGER,PARAMETER :: nTotalElem  = 8
INTEGER,PARAMETER :: nElemNodes  = 8
INTEGER,PARAMETER :: nTotalNodes = 27
!----------------------------------------------------------------------------------------------------------------------!
TYPE(tElem),POINTER :: aElem
TYPE(tElem),POINTER :: PrevElem
TYPE(tElem),POINTER :: NextElem
TYPE(tSide),POINTER :: aSide
!----------------------------------------------------------------------------------------------------------------------!
TYPE(tElemPtr) :: NewElems(1:nTotalElem)
TYPE(tNodePtr) :: NewNodes(1:nTotalNodes)
TYPE(tSidePtr) :: NewSides(1:nTotalElem,1:nElemFaces)
TYPE(tNodePtr) :: ElemNodes(1:nElemNodes)
!----------------------------------------------------------------------------------------------------------------------!

! For splitting the HEXA8 element into 8 HEXA8 subelements,
! we use the CGNS-HEXA27 nodes numbering

dh = 0.5

NewNodes(1)%Node => Elem%Nodes(1)%Node
NewNodes(2)%Node => Elem%Nodes(2)%Node
NewNodes(3)%Node => Elem%Nodes(3)%Node
NewNodes(4)%Node => Elem%Nodes(4)%Node
NewNodes(5)%Node => Elem%Nodes(5)%Node
NewNodes(6)%Node => Elem%Nodes(6)%Node
NewNodes(7)%Node => Elem%Nodes(7)%Node
NewNodes(8)%Node => Elem%Nodes(8)%Node

! New midnodes at edges
CALL CreateNode(NewNodes(9)%Node)
CALL CreateNode(NewNodes(10)%Node)
CALL CreateNode(NewNodes(11)%Node)
CALL CreateNode(NewNodes(12)%Node)
CALL CreateNode(NewNodes(13)%Node)
CALL CreateNode(NewNodes(14)%Node)
CALL CreateNode(NewNodes(15)%Node)
CALL CreateNode(NewNodes(16)%Node)
CALL CreateNode(NewNodes(17)%Node)
CALL CreateNode(NewNodes(18)%Node)
CALL CreateNode(NewNodes(19)%Node)
CALL CreateNode(NewNodes(20)%Node)

NewNodes(9)%Node%Coords  = dh*(NewNodes(1)%Node%Coords + NewNodes(2)%Node%Coords)
NewNodes(10)%Node%Coords = dh*(NewNodes(2)%Node%Coords + NewNodes(3)%Node%Coords)
NewNodes(11)%Node%Coords = dh*(NewNodes(3)%Node%Coords + NewNodes(4)%Node%Coords)
NewNodes(12)%Node%Coords = dh*(NewNodes(1)%Node%Coords + NewNodes(4)%Node%Coords)

NewNodes(13)%Node%Coords = dh*(NewNodes(1)%Node%Coords + NewNodes(5)%Node%Coords)
NewNodes(14)%Node%Coords = dh*(NewNodes(2)%Node%Coords + NewNodes(6)%Node%Coords)
NewNodes(15)%Node%Coords = dh*(NewNodes(3)%Node%Coords + NewNodes(7)%Node%Coords)
NewNodes(16)%Node%Coords = dh*(NewNodes(4)%Node%Coords + NewNodes(8)%Node%Coords)

NewNodes(17)%Node%Coords = dh*(NewNodes(5)%Node%Coords + NewNodes(6)%Node%Coords)
NewNodes(18)%Node%Coords = dh*(NewNodes(6)%Node%Coords + NewNodes(7)%Node%Coords)
NewNodes(19)%Node%Coords = dh*(NewNodes(7)%Node%Coords + NewNodes(8)%Node%Coords)
NewNodes(20)%Node%Coords = dh*(NewNodes(8)%Node%Coords + NewNodes(5)%Node%Coords)

! New midnodes at faces
CALL CreateNode(NewNodes(21)%Node)
CALL CreateNode(NewNodes(22)%Node)
CALL CreateNode(NewNodes(23)%Node)
CALL CreateNode(NewNodes(24)%Node)
CALL CreateNode(NewNodes(25)%Node)
CALL CreateNode(NewNodes(26)%Node)

NewNodes(21)%Node%Coords = dh*(NewNodes(1)%Node%Coords + NewNodes(3)%Node%Coords)
NewNodes(22)%Node%Coords = dh*(NewNodes(1)%Node%Coords + NewNodes(6)%Node%Coords)
NewNodes(23)%Node%Coords = dh*(NewNodes(2)%Node%Coords + NewNodes(7)%Node%Coords)
NewNodes(24)%Node%Coords = dh*(NewNodes(3)%Node%Coords + NewNodes(8)%Node%Coords)
NewNodes(25)%Node%Coords = dh*(NewNodes(4)%Node%Coords + NewNodes(5)%Node%Coords)
NewNodes(26)%Node%Coords = dh*(NewNodes(5)%Node%Coords + NewNodes(7)%Node%Coords)

! New node at center of element ******** WARNING ******** CENTROID OF HEXAHEDRON ******** WARNING ********
CALL CreateNode(NewNodes(27)%Node)
NewNodes(27)%Node%Coords = dh*(NewNodes(1)%Node%Coords + NewNodes(7)%Node%Coords)

!--------------------------------------------------!
! Create Child = 1
!--------------------------------------------------!
LocElemID = 1
ElemType  = ELEMTYPE_HEXA8
ElemNodes(1)%Node => NewNodes(1)%Node
ElemNodes(2)%Node => NewNodes(9)%Node
ElemNodes(3)%Node => NewNodes(21)%Node
ElemNodes(4)%Node => NewNodes(12)%Node
ElemNodes(5)%Node => NewNodes(13)%Node
ElemNodes(6)%Node => NewNodes(22)%Node
ElemNodes(7)%Node => NewNodes(27)%Node
ElemNodes(8)%Node => NewNodes(25)%Node
CALL CreateElem_CGNS(NewElems(LocElemID)%Elem,ElemType,1,ElemNodes)
NewElems(LocElemID)%Elem%ElemType = ElemType
NewElems(LocElemID)%Elem%Flag     = Elem%Flag
NewElems(LocElemID)%Elem%Level    = Elem%Level+1

LocSideID = 0
aSide => NewElems(LocElemID)%Elem%FirstSide
DO WHILE (ASSOCIATED(aSide))
  LocSideID = LocSideID+1
  NewSides(LocElemID,LocSideID)%Side => aSide
  aSide => aSide%NextElemSide
END DO

!--------------------------------------------------!
! Create Child = 2
!--------------------------------------------------!
LocElemID = 2
ElemType  = ELEMTYPE_HEXA8
ElemNodes(1)%Node => NewNodes(9)%Node
ElemNodes(2)%Node => NewNodes(2)%Node
ElemNodes(3)%Node => NewNodes(10)%Node
ElemNodes(4)%Node => NewNodes(21)%Node
ElemNodes(5)%Node => NewNodes(22)%Node
ElemNodes(6)%Node => NewNodes(14)%Node
ElemNodes(7)%Node => NewNodes(23)%Node
ElemNodes(8)%Node => NewNodes(27)%Node
CALL CreateElem_CGNS(NewElems(LocElemID)%Elem,ElemType,1,ElemNodes)
NewElems(LocElemID)%Elem%ElemType = ElemType
NewElems(LocElemID)%Elem%Flag     = Elem%Flag
NewElems(LocElemID)%Elem%Level    = Elem%Level+1

LocSideID = 0
aSide => NewElems(LocElemID)%Elem%FirstSide
DO WHILE (ASSOCIATED(aSide))
  LocSideID = LocSideID+1
  NewSides(LocElemID,LocSideID)%Side => aSide
  aSide => aSide%NextElemSide
END DO

!--------------------------------------------------!
! Create Child = 3
!--------------------------------------------------!
LocElemID = 3
ElemType  = ELEMTYPE_HEXA8
ElemNodes(1)%Node => NewNodes(12)%Node
ElemNodes(2)%Node => NewNodes(21)%Node
ElemNodes(3)%Node => NewNodes(11)%Node
ElemNodes(4)%Node => NewNodes(4)%Node
ElemNodes(5)%Node => NewNodes(25)%Node
ElemNodes(6)%Node => NewNodes(27)%Node
ElemNodes(7)%Node => NewNodes(24)%Node
ElemNodes(8)%Node => NewNodes(16)%Node
CALL CreateElem_CGNS(NewElems(LocElemID)%Elem,ElemType,1,ElemNodes)
NewElems(LocElemID)%Elem%ElemType = ElemType
NewElems(LocElemID)%Elem%Flag     = Elem%Flag
NewElems(LocElemID)%Elem%Level    = Elem%Level+1

LocSideID = 0
aSide => NewElems(LocElemID)%Elem%FirstSide
DO WHILE (ASSOCIATED(aSide))
  LocSideID = LocSideID+1
  NewSides(LocElemID,LocSideID)%Side => aSide
  aSide => aSide%NextElemSide
END DO

!--------------------------------------------------!
! Create Child = 4
!--------------------------------------------------!
LocElemID = 4
ElemType  = ELEMTYPE_HEXA8
ElemNodes(1)%Node => NewNodes(21)%Node
ElemNodes(2)%Node => NewNodes(10)%Node
ElemNodes(3)%Node => NewNodes(3)%Node
ElemNodes(4)%Node => NewNodes(11)%Node
ElemNodes(5)%Node => NewNodes(27)%Node
ElemNodes(6)%Node => NewNodes(23)%Node
ElemNodes(7)%Node => NewNodes(15)%Node
ElemNodes(8)%Node => NewNodes(24)%Node
CALL CreateElem_CGNS(NewElems(LocElemID)%Elem,ElemType,1,ElemNodes)
NewElems(LocElemID)%Elem%ElemType = ElemType
NewElems(LocElemID)%Elem%Flag     = Elem%Flag
NewElems(LocElemID)%Elem%Level    = Elem%Level+1

LocSideID = 0
aSide => NewElems(LocElemID)%Elem%FirstSide
DO WHILE (ASSOCIATED(aSide))
  LocSideID = LocSideID+1
  NewSides(LocElemID,LocSideID)%Side => aSide
  aSide => aSide%NextElemSide
END DO

!--------------------------------------------------!
! Create Child = 5
!--------------------------------------------------!
LocElemID = 5
ElemType  = ELEMTYPE_HEXA8
ElemNodes(1)%Node => NewNodes(13)%Node
ElemNodes(2)%Node => NewNodes(22)%Node
ElemNodes(3)%Node => NewNodes(27)%Node
ElemNodes(4)%Node => NewNodes(25)%Node
ElemNodes(5)%Node => NewNodes(5)%Node
ElemNodes(6)%Node => NewNodes(17)%Node
ElemNodes(7)%Node => NewNodes(26)%Node
ElemNodes(8)%Node => NewNodes(20)%Node
CALL CreateElem_CGNS(NewElems(LocElemID)%Elem,ElemType,1,ElemNodes)
NewElems(LocElemID)%Elem%ElemType = ElemType
NewElems(LocElemID)%Elem%Flag     = Elem%Flag
NewElems(LocElemID)%Elem%Level    = Elem%Level+1

LocSideID = 0
aSide => NewElems(LocElemID)%Elem%FirstSide
DO WHILE (ASSOCIATED(aSide))
  LocSideID = LocSideID+1
  NewSides(LocElemID,LocSideID)%Side => aSide
  aSide => aSide%NextElemSide
END DO

!--------------------------------------------------!
! Create Child = 6
!--------------------------------------------------!
LocElemID = 6
ElemType  = ELEMTYPE_HEXA8
ElemNodes(1)%Node => NewNodes(22)%Node
ElemNodes(2)%Node => NewNodes(14)%Node
ElemNodes(3)%Node => NewNodes(23)%Node
ElemNodes(4)%Node => NewNodes(27)%Node
ElemNodes(5)%Node => NewNodes(17)%Node
ElemNodes(6)%Node => NewNodes(6)%Node
ElemNodes(7)%Node => NewNodes(18)%Node
ElemNodes(8)%Node => NewNodes(26)%Node
CALL CreateElem_CGNS(NewElems(LocElemID)%Elem,ElemType,1,ElemNodes)
NewElems(LocElemID)%Elem%ElemType = ElemType
NewElems(LocElemID)%Elem%Flag     = Elem%Flag
NewElems(LocElemID)%Elem%Level    = Elem%Level+1

LocSideID = 0
aSide => NewElems(LocElemID)%Elem%FirstSide
DO WHILE (ASSOCIATED(aSide))
  LocSideID = LocSideID+1
  NewSides(LocElemID,LocSideID)%Side => aSide
  aSide => aSide%NextElemSide
END DO

!--------------------------------------------------!
! Create Child = 7
!--------------------------------------------------!
LocElemID = 7
ElemType  = ELEMTYPE_HEXA8
ElemNodes(1)%Node => NewNodes(25)%Node
ElemNodes(2)%Node => NewNodes(27)%Node
ElemNodes(3)%Node => NewNodes(24)%Node
ElemNodes(4)%Node => NewNodes(16)%Node
ElemNodes(5)%Node => NewNodes(20)%Node
ElemNodes(6)%Node => NewNodes(26)%Node
ElemNodes(7)%Node => NewNodes(19)%Node
ElemNodes(8)%Node => NewNodes(8)%Node
CALL CreateElem_CGNS(NewElems(LocElemID)%Elem,ElemType,1,ElemNodes)
NewElems(LocElemID)%Elem%ElemType = ElemType
NewElems(LocElemID)%Elem%Flag     = Elem%Flag
NewElems(LocElemID)%Elem%Level    = Elem%Level+1

LocSideID = 0
aSide => NewElems(LocElemID)%Elem%FirstSide
DO WHILE (ASSOCIATED(aSide))
  LocSideID = LocSideID+1
  NewSides(LocElemID,LocSideID)%Side => aSide
  aSide => aSide%NextElemSide
END DO

!--------------------------------------------------!
! Create Child = 8
!--------------------------------------------------!
LocElemID = 8
ElemType  = ELEMTYPE_HEXA8
ElemNodes(1)%Node => NewNodes(27)%Node
ElemNodes(2)%Node => NewNodes(23)%Node
ElemNodes(3)%Node => NewNodes(15)%Node
ElemNodes(4)%Node => NewNodes(24)%Node
ElemNodes(5)%Node => NewNodes(26)%Node
ElemNodes(6)%Node => NewNodes(18)%Node
ElemNodes(7)%Node => NewNodes(7)%Node
ElemNodes(8)%Node => NewNodes(19)%Node
CALL CreateElem_CGNS(NewElems(LocElemID)%Elem,ElemType,1,ElemNodes)
NewElems(LocElemID)%Elem%ElemType = ElemType
NewElems(LocElemID)%Elem%Flag     = Elem%Flag
NewElems(LocElemID)%Elem%Level    = Elem%Level+1

LocSideID = 0
aSide => NewElems(LocElemID)%Elem%FirstSide
DO WHILE (ASSOCIATED(aSide))
  LocSideID = LocSideID+1
  NewSides(LocElemID,LocSideID)%Side => aSide
  aSide => aSide%NextElemSide
END DO

!--------------------------------------------------!
! Set Boundary Conditions
!--------------------------------------------------!
LocSideID = 0
aSide => Elem%FirstSide
DO WHILE (ASSOCIATED(aSide))
  LocSideID = LocSideID+1
  IF (ASSOCIATED(aSide%BC)) THEN
    DO LocElemID=1,nTotalElem
      SELECT CASE (LocElemID)
        CASE(1)
          SELECT CASE (LocSideID)
            CASE(1)
              CALL CopyBC(aSide,NewSides(LocElemID,1)%Side)
            CASE(2)
              CALL CopyBC(aSide,NewSides(LocElemID,2)%Side)
            CASE(5)
              CALL CopyBC(aSide,NewSides(LocElemID,5)%Side)
          END SELECT
        CASE(2)
          SELECT CASE (LocSideID)
            CASE(1)
              CALL CopyBC(aSide,NewSides(LocElemID,1)%Side)
            CASE(2)
              CALL CopyBC(aSide,NewSides(LocElemID,2)%Side)
            CASE(3)
              CALL CopyBC(aSide,NewSides(LocElemID,3)%Side)
          END SELECT
        CASE(3)
          SELECT CASE (LocSideID)
            CASE(1)
              CALL CopyBC(aSide,NewSides(LocElemID,1)%Side)
            CASE(4)
              CALL CopyBC(aSide,NewSides(LocElemID,4)%Side)
            CASE(5)
              CALL CopyBC(aSide,NewSides(LocElemID,5)%Side)
          END SELECT
        CASE(4)
          SELECT CASE (LocSideID)
            CASE(1)
              CALL CopyBC(aSide,NewSides(LocElemID,1)%Side)
            CASE(3)
              CALL CopyBC(aSide,NewSides(LocElemID,3)%Side)
            CASE(4)
              CALL CopyBC(aSide,NewSides(LocElemID,4)%Side)
          END SELECT
        CASE(5)
          SELECT CASE (LocSideID)
            CASE(2)
              CALL CopyBC(aSide,NewSides(LocElemID,2)%Side)
            CASE(5)
              CALL CopyBC(aSide,NewSides(LocElemID,5)%Side)
            CASE(6)
              CALL CopyBC(aSide,NewSides(LocElemID,6)%Side)
          END SELECT
        CASE(6)
          SELECT CASE (LocSideID)
            CASE(2)
              CALL CopyBC(aSide,NewSides(LocElemID,2)%Side)
            CASE(3)
              CALL CopyBC(aSide,NewSides(LocElemID,3)%Side)
            CASE(6)
              CALL CopyBC(aSide,NewSides(LocElemID,6)%Side)
          END SELECT
        CASE(7)
          SELECT CASE (LocSideID)
            CASE(4)
              CALL CopyBC(aSide,NewSides(LocElemID,4)%Side)
            CASE(5)
              CALL CopyBC(aSide,NewSides(LocElemID,5)%Side)
            CASE(6)
              CALL CopyBC(aSide,NewSides(LocElemID,6)%Side)
          END SELECT
        CASE(8)
          SELECT CASE (LocSideID)
            CASE(3)
              CALL CopyBC(aSide,NewSides(LocElemID,3)%Side)
            CASE(4)
              CALL CopyBC(aSide,NewSides(LocElemID,4)%Side)
            CASE(6)
              CALL CopyBC(aSide,NewSides(LocElemID,6)%Side)
          END SELECT
      END SELECT
    END DO
  END IF
  aSide => aSide%NextElemSide
END DO

aElem    => Elem
NextElem => Elem%NextElem
PrevElem => Elem%PrevElem

DO LocElemID=1,nTotalElem
  aElem%NextElem          => NewElems(LocElemID)%Elem
  aElem%NextElem%PrevElem => aElem
  aElem                   => aElem%NextElem
END DO

IF (ASSOCIATED(NextElem)) THEN
  aElem%NextElem    => NextElem
  NextElem%PrevElem => aElem
END IF

CALL RemoveElem(Elem,ElemList)

Elem => aElem

IF (ASSOCIATED(PrevElem)) THEN
  NewElems(1)%Elem%PrevElem => PrevElem
  PrevElem%NextElem         => NewElems(1)%Elem
ELSE
  ElemList%FirstElem => NewElems(1)%Elem
END IF

!----------------------------------------------------------------------------------------------------------------------!
END SUBROUTINE SplitElem_HEXA8
!======================================================================================================================!
!
!
!
!======================================================================================================================!
SUBROUTINE SplitElem_HEXA8_XZ(Elem)
!----------------------------------------------------------------------------------------------------------------------!
USE MOD_MeshMain_vars,ONLY: tElem
USE MOD_MeshMain_vars,ONLY: tSide
USE MOD_MeshMain_vars,ONLY: tElemPtr
USE MOD_MeshMain_vars,ONLY: tNodePtr
USE MOD_MeshMain_vars,ONLY: tSidePtr
USE MOD_MeshMain_vars,ONLY: ElemList
!----------------------------------------------------------------------------------------------------------------------!
USE MOD_MeshMainMethods,ONLY: CountElems
USE MOD_MeshMainMethods,ONLY: CopyBC
USE MOD_MeshMainMethods,ONLY: RemoveElem
USE MOD_MeshMainMethods,ONLY: CreateNode
USE MOD_MeshMainMethods,ONLY: CreateElem_CGNS
!----------------------------------------------------------------------------------------------------------------------!
USE MOD_Mesh_CGNS_Definitions
!----------------------------------------------------------------------------------------------------------------------!
IMPLICIT NONE
!----------------------------------------------------------------------------------------------------------------------!
! FORMAL ARGUMENTS
!----------------------------------------------------------------------------------------------------------------------!
TYPE(tElem),POINTER,INTENT(INOUT) :: Elem
!----------------------------------------------------------------------------------------------------------------------!
! LOCAL VARIABLES
!----------------------------------------------------------------------------------------------------------------------!
INTEGER :: ElemType
INTEGER :: LocSideID
INTEGER :: LocElemID
REAL    :: dh
!----------------------------------------------------------------------------------------------------------------------!
INTEGER,PARAMETER :: nElemFaces  = 6
INTEGER,PARAMETER :: nTotalElem  = 4
INTEGER,PARAMETER :: nElemNodes  = 8
INTEGER,PARAMETER :: nTotalNodes = 18
!----------------------------------------------------------------------------------------------------------------------!
TYPE(tElem),POINTER :: aElem
TYPE(tElem),POINTER :: PrevElem
TYPE(tElem),POINTER :: NextElem
TYPE(tSide),POINTER :: aSide
!----------------------------------------------------------------------------------------------------------------------!
TYPE(tElemPtr) :: NewElems(1:nTotalElem)
TYPE(tNodePtr) :: NewNodes(1:nTotalNodes)
TYPE(tSidePtr) :: NewSides(1:nTotalElem,1:nElemFaces)
TYPE(tNodePtr) :: ElemNodes(1:nElemNodes)
!----------------------------------------------------------------------------------------------------------------------!

! For splitting the HEXA8 element into 4 HEXA8 subelements,
! we use the CGNS-HEXA27 nodes numbering

dh = 0.5

NewNodes(1)%Node => Elem%Nodes(1)%Node
NewNodes(2)%Node => Elem%Nodes(2)%Node
NewNodes(3)%Node => Elem%Nodes(3)%Node
NewNodes(4)%Node => Elem%Nodes(4)%Node
NewNodes(5)%Node => Elem%Nodes(5)%Node
NewNodes(6)%Node => Elem%Nodes(6)%Node
NewNodes(7)%Node => Elem%Nodes(7)%Node
NewNodes(8)%Node => Elem%Nodes(8)%Node

! New midnodes at edges
CALL CreateNode(NewNodes(9)%Node)
CALL CreateNode(NewNodes(10)%Node)
CALL CreateNode(NewNodes(11)%Node)
CALL CreateNode(NewNodes(12)%Node)
CALL CreateNode(NewNodes(13)%Node)
CALL CreateNode(NewNodes(14)%Node)
CALL CreateNode(NewNodes(15)%Node)
CALL CreateNode(NewNodes(16)%Node)

NewNodes(9)%Node%Coords  = dh*(NewNodes(1)%Node%Coords + NewNodes(2)%Node%Coords)
NewNodes(10)%Node%Coords = dh*(NewNodes(2)%Node%Coords + NewNodes(3)%Node%Coords)
NewNodes(11)%Node%Coords = dh*(NewNodes(3)%Node%Coords + NewNodes(4)%Node%Coords)
NewNodes(12)%Node%Coords = dh*(NewNodes(1)%Node%Coords + NewNodes(4)%Node%Coords)

NewNodes(13)%Node%Coords = dh*(NewNodes(5)%Node%Coords + NewNodes(6)%Node%Coords)
NewNodes(14)%Node%Coords = dh*(NewNodes(6)%Node%Coords + NewNodes(7)%Node%Coords)
NewNodes(15)%Node%Coords = dh*(NewNodes(7)%Node%Coords + NewNodes(8)%Node%Coords)
NewNodes(16)%Node%Coords = dh*(NewNodes(5)%Node%Coords + NewNodes(8)%Node%Coords)

! New midnodes at faces ******** WARNING ******** CENTROID OF QUADRILATERAL ******** WARNING ********
CALL CreateNode(NewNodes(17)%Node)
CALL CreateNode(NewNodes(18)%Node)

NewNodes(17)%Node%Coords = dh*(NewNodes(1)%Node%Coords + NewNodes(3)%Node%Coords)
NewNodes(18)%Node%Coords = dh*(NewNodes(5)%Node%Coords + NewNodes(7)%Node%Coords)

!--------------------------------------------------!
! Create Child = 1
!--------------------------------------------------!
LocElemID = 1
ElemType  = ELEMTYPE_HEXA8
ElemNodes(1)%Node => NewNodes(1)%Node
ElemNodes(2)%Node => NewNodes(9)%Node
ElemNodes(3)%Node => NewNodes(17)%Node
ElemNodes(4)%Node => NewNodes(12)%Node
ElemNodes(5)%Node => NewNodes(5)%Node
ElemNodes(6)%Node => NewNodes(13)%Node
ElemNodes(7)%Node => NewNodes(18)%Node
ElemNodes(8)%Node => NewNodes(16)%Node
CALL CreateElem_CGNS(NewElems(LocElemID)%Elem,ElemType,1,ElemNodes)
NewElems(LocElemID)%Elem%ElemType = ElemType
NewElems(LocElemID)%Elem%Flag     = Elem%Flag
NewElems(LocElemID)%Elem%Level    = Elem%Level+1

LocSideID = 0
aSide => NewElems(LocElemID)%Elem%FirstSide
DO WHILE (ASSOCIATED(aSide))
  LocSideID = LocSideID+1
  NewSides(LocElemID,LocSideID)%Side => aSide
  aSide => aSide%NextElemSide
END DO

!--------------------------------------------------!
! Create Child = 2
!--------------------------------------------------!
LocElemID = 2
ElemType  = ELEMTYPE_HEXA8
ElemNodes(1)%Node => NewNodes(9)%Node
ElemNodes(2)%Node => NewNodes(2)%Node
ElemNodes(3)%Node => NewNodes(10)%Node
ElemNodes(4)%Node => NewNodes(17)%Node
ElemNodes(5)%Node => NewNodes(13)%Node
ElemNodes(6)%Node => NewNodes(6)%Node
ElemNodes(7)%Node => NewNodes(14)%Node
ElemNodes(8)%Node => NewNodes(18)%Node
CALL CreateElem_CGNS(NewElems(LocElemID)%Elem,ElemType,1,ElemNodes)
NewElems(LocElemID)%Elem%ElemType = ElemType
NewElems(LocElemID)%Elem%Flag     = Elem%Flag
NewElems(LocElemID)%Elem%Level    = Elem%Level+1

LocSideID = 0
aSide => NewElems(LocElemID)%Elem%FirstSide
DO WHILE (ASSOCIATED(aSide))
  LocSideID = LocSideID+1
  NewSides(LocElemID,LocSideID)%Side => aSide
  aSide => aSide%NextElemSide
END DO

!--------------------------------------------------!
! Create Child = 3
!--------------------------------------------------!
LocElemID = 3
ElemType  = ELEMTYPE_HEXA8
ElemNodes(1)%Node => NewNodes(12)%Node
ElemNodes(2)%Node => NewNodes(17)%Node
ElemNodes(3)%Node => NewNodes(11)%Node
ElemNodes(4)%Node => NewNodes(4)%Node
ElemNodes(5)%Node => NewNodes(16)%Node
ElemNodes(6)%Node => NewNodes(18)%Node
ElemNodes(7)%Node => NewNodes(15)%Node
ElemNodes(8)%Node => NewNodes(8)%Node
CALL CreateElem_CGNS(NewElems(LocElemID)%Elem,ElemType,1,ElemNodes)
NewElems(LocElemID)%Elem%ElemType = ElemType
NewElems(LocElemID)%Elem%Flag     = Elem%Flag
NewElems(LocElemID)%Elem%Level    = Elem%Level+1

LocSideID = 0
aSide => NewElems(LocElemID)%Elem%FirstSide
DO WHILE (ASSOCIATED(aSide))
  LocSideID = LocSideID+1
  NewSides(LocElemID,LocSideID)%Side => aSide
  aSide => aSide%NextElemSide
END DO

!--------------------------------------------------!
! Create Child = 4
!--------------------------------------------------!
LocElemID = 4
ElemType  = ELEMTYPE_HEXA8
ElemNodes(1)%Node => NewNodes(17)%Node
ElemNodes(2)%Node => NewNodes(10)%Node
ElemNodes(3)%Node => NewNodes(3)%Node
ElemNodes(4)%Node => NewNodes(11)%Node
ElemNodes(5)%Node => NewNodes(18)%Node
ElemNodes(6)%Node => NewNodes(14)%Node
ElemNodes(7)%Node => NewNodes(7)%Node
ElemNodes(8)%Node => NewNodes(15)%Node
CALL CreateElem_CGNS(NewElems(LocElemID)%Elem,ElemType,1,ElemNodes)
NewElems(LocElemID)%Elem%ElemType = ElemType
NewElems(LocElemID)%Elem%Flag     = Elem%Flag
NewElems(LocElemID)%Elem%Level    = Elem%Level+1

LocSideID = 0
aSide => NewElems(LocElemID)%Elem%FirstSide
DO WHILE (ASSOCIATED(aSide))
  LocSideID = LocSideID+1
  NewSides(LocElemID,LocSideID)%Side => aSide
  aSide => aSide%NextElemSide
END DO

!--------------------------------------------------!
! Set Boundary Conditions
!--------------------------------------------------!
LocSideID = 0
aSide => Elem%FirstSide
DO WHILE (ASSOCIATED(aSide))
  LocSideID = LocSideID+1
  IF (ASSOCIATED(aSide%BC)) THEN
    DO LocElemID=1,nTotalElem
      SELECT CASE (LocElemID)
        CASE(1)
          SELECT CASE (LocSideID)
            CASE(1)
              CALL CopyBC(aSide,NewSides(LocElemID,1)%Side)
            CASE(2)
              CALL CopyBC(aSide,NewSides(LocElemID,2)%Side)
            CASE(5)
              CALL CopyBC(aSide,NewSides(LocElemID,5)%Side)
            CASE(6)
              CALL CopyBC(aSide,NewSides(LocElemID,6)%Side)
          END SELECT
        CASE(2)
          SELECT CASE (LocSideID)
            CASE(1)
              CALL CopyBC(aSide,NewSides(LocElemID,1)%Side)
            CASE(2)
              CALL CopyBC(aSide,NewSides(LocElemID,2)%Side)
            CASE(3)
              CALL CopyBC(aSide,NewSides(LocElemID,3)%Side)
            CASE(6)
              CALL CopyBC(aSide,NewSides(LocElemID,6)%Side)
          END SELECT
        CASE(3)
          SELECT CASE (LocSideID)
            CASE(1)
              CALL CopyBC(aSide,NewSides(LocElemID,1)%Side)
            CASE(4)
              CALL CopyBC(aSide,NewSides(LocElemID,4)%Side)
            CASE(5)
              CALL CopyBC(aSide,NewSides(LocElemID,5)%Side)
            CASE(6)
              CALL CopyBC(aSide,NewSides(LocElemID,6)%Side)
          END SELECT
        CASE(4)
          SELECT CASE (LocSideID)
            CASE(1)
              CALL CopyBC(aSide,NewSides(LocElemID,1)%Side)
            CASE(3)
              CALL CopyBC(aSide,NewSides(LocElemID,3)%Side)
            CASE(4)
              CALL CopyBC(aSide,NewSides(LocElemID,4)%Side)
            CASE(6)
              CALL CopyBC(aSide,NewSides(LocElemID,6)%Side)
          END SELECT
      END SELECT
    END DO
  END IF
  aSide => aSide%NextElemSide
END DO

aElem    => Elem
NextElem => Elem%NextElem
PrevElem => Elem%PrevElem

DO LocElemID=1,nTotalElem
  aElem%NextElem          => NewElems(LocElemID)%Elem
  aElem%NextElem%PrevElem => aElem
  aElem                   => aElem%NextElem
END DO

IF (ASSOCIATED(NextElem)) THEN
  aElem%NextElem    => NextElem
  NextElem%PrevElem => aElem
END IF

CALL RemoveElem(Elem,ElemList)

Elem => aElem

IF (ASSOCIATED(PrevElem)) THEN
  NewElems(1)%Elem%PrevElem => PrevElem
  PrevElem%NextElem         => NewElems(1)%Elem
ELSE
  ElemList%FirstElem => NewElems(1)%Elem
END IF

!----------------------------------------------------------------------------------------------------------------------!
END SUBROUTINE SplitElem_HEXA8_XZ
!======================================================================================================================!
!
!
!
!======================================================================================================================!
SUBROUTINE SplitElem_PRISM6(Elem)
!----------------------------------------------------------------------------------------------------------------------!
USE MOD_MeshMain_vars,ONLY: tElem
USE MOD_MeshMain_vars,ONLY: tSide
USE MOD_MeshMain_vars,ONLY: tElemPtr
USE MOD_MeshMain_vars,ONLY: tNodePtr
USE MOD_MeshMain_vars,ONLY: tSidePtr
USE MOD_MeshMain_vars,ONLY: ElemList
!----------------------------------------------------------------------------------------------------------------------!
USE MOD_MeshMainMethods,ONLY: CountElems
USE MOD_MeshMainMethods,ONLY: CopyBC
USE MOD_MeshMainMethods,ONLY: RemoveElem
USE MOD_MeshMainMethods,ONLY: CreateNode
USE MOD_MeshMainMethods,ONLY: CreateElem_CGNS
!----------------------------------------------------------------------------------------------------------------------!
USE MOD_Mesh_CGNS_Definitions
!----------------------------------------------------------------------------------------------------------------------!
IMPLICIT NONE
!----------------------------------------------------------------------------------------------------------------------!
! FORMAL ARGUMENTS
!----------------------------------------------------------------------------------------------------------------------!
TYPE(tElem),POINTER,INTENT(INOUT) :: Elem
!----------------------------------------------------------------------------------------------------------------------!
! LOCAL VARIABLES
!----------------------------------------------------------------------------------------------------------------------!
INTEGER :: ElemType
INTEGER :: LocSideID
INTEGER :: LocElemID
REAL    :: dh
!----------------------------------------------------------------------------------------------------------------------!
INTEGER,PARAMETER :: nElemFaces  = 5
INTEGER,PARAMETER :: nTotalElem  = 8
INTEGER,PARAMETER :: nElemNodes  = 6
INTEGER,PARAMETER :: nTotalNodes = 18
!----------------------------------------------------------------------------------------------------------------------!
TYPE(tElem),POINTER :: aElem
TYPE(tElem),POINTER :: PrevElem
TYPE(tElem),POINTER :: NextElem
TYPE(tSide),POINTER :: aSide
!----------------------------------------------------------------------------------------------------------------------!
TYPE(tElemPtr) :: NewElems(1:nTotalElem)
TYPE(tNodePtr) :: NewNodes(1:nTotalNodes)
TYPE(tSidePtr) :: NewSides(1:nTotalElem,1:nElemFaces)
TYPE(tNodePtr) :: ElemNodes(1:nElemNodes)
!----------------------------------------------------------------------------------------------------------------------!

! For splitting the PRISM6 element into 8 PRISM6 subelements,
! we use the CGNS-PRISM18 nodes numbering

dh = 0.5

NewNodes(1)%Node => Elem%Nodes(1)%Node
NewNodes(2)%Node => Elem%Nodes(2)%Node
NewNodes(3)%Node => Elem%Nodes(3)%Node
NewNodes(4)%Node => Elem%Nodes(4)%Node
NewNodes(5)%Node => Elem%Nodes(5)%Node
NewNodes(6)%Node => Elem%Nodes(6)%Node

! New midnodes at edges
CALL CreateNode(NewNodes(7)%Node)
CALL CreateNode(NewNodes(8)%Node)
CALL CreateNode(NewNodes(9)%Node)
CALL CreateNode(NewNodes(10)%Node)
CALL CreateNode(NewNodes(11)%Node)
CALL CreateNode(NewNodes(12)%Node)
CALL CreateNode(NewNodes(13)%Node)
CALL CreateNode(NewNodes(14)%Node)
CALL CreateNode(NewNodes(15)%Node)

NewNodes(7)%Node%Coords  = dh*(NewNodes(1)%Node%Coords + NewNodes(2)%Node%Coords)
NewNodes(8)%Node%Coords  = dh*(NewNodes(2)%Node%Coords + NewNodes(3)%Node%Coords)
NewNodes(9)%Node%Coords  = dh*(NewNodes(1)%Node%Coords + NewNodes(3)%Node%Coords)
NewNodes(10)%Node%Coords = dh*(NewNodes(1)%Node%Coords + NewNodes(4)%Node%Coords)
NewNodes(11)%Node%Coords = dh*(NewNodes(2)%Node%Coords + NewNodes(5)%Node%Coords)
NewNodes(12)%Node%Coords = dh*(NewNodes(3)%Node%Coords + NewNodes(6)%Node%Coords)
NewNodes(13)%Node%Coords = dh*(NewNodes(4)%Node%Coords + NewNodes(5)%Node%Coords)
NewNodes(14)%Node%Coords = dh*(NewNodes(5)%Node%Coords + NewNodes(6)%Node%Coords)
NewNodes(15)%Node%Coords = dh*(NewNodes(4)%Node%Coords + NewNodes(6)%Node%Coords)

! New midnodes at new edges
CALL CreateNode(NewNodes(16)%Node)
CALL CreateNode(NewNodes(17)%Node)
CALL CreateNode(NewNodes(18)%Node)

NewNodes(16)%Node%Coords = dh*(NewNodes(10)%Node%Coords + NewNodes(11)%Node%Coords)
NewNodes(17)%Node%Coords = dh*(NewNodes(11)%Node%Coords + NewNodes(12)%Node%Coords)
NewNodes(18)%Node%Coords = dh*(NewNodes(10)%Node%Coords + NewNodes(12)%Node%Coords)

!--------------------------------------------------!
! Create Child = 1
!--------------------------------------------------!
LocElemID = 1
ElemType  = ELEMTYPE_PRISM6
ElemNodes(1)%Node => NewNodes(1)%Node
ElemNodes(2)%Node => NewNodes(7)%Node
ElemNodes(3)%Node => NewNodes(9)%Node
ElemNodes(4)%Node => NewNodes(10)%Node
ElemNodes(5)%Node => NewNodes(16)%Node
ElemNodes(6)%Node => NewNodes(18)%Node
CALL CreateElem_CGNS(NewElems(LocElemID)%Elem,ElemType,1,ElemNodes)
NewElems(LocElemID)%Elem%ElemType = ElemType
NewElems(LocElemID)%Elem%Flag     = Elem%Flag
NewElems(LocElemID)%Elem%Level    = Elem%Level+1

LocSideID = 0
aSide => NewElems(LocElemID)%Elem%FirstSide
DO WHILE (ASSOCIATED(aSide))
  LocSideID = LocSideID+1
  NewSides(LocElemID,LocSideID)%Side => aSide
  aSide => aSide%NextElemSide
END DO

!--------------------------------------------------!
! Create Child = 2
!--------------------------------------------------!
LocElemID = 2
ElemType  = ELEMTYPE_PRISM6
ElemNodes(1)%Node => NewNodes(7)%Node
ElemNodes(2)%Node => NewNodes(2)%Node
ElemNodes(3)%Node => NewNodes(8)%Node
ElemNodes(4)%Node => NewNodes(16)%Node
ElemNodes(5)%Node => NewNodes(11)%Node
ElemNodes(6)%Node => NewNodes(17)%Node
CALL CreateElem_CGNS(NewElems(LocElemID)%Elem,ElemType,1,ElemNodes)
NewElems(LocElemID)%Elem%ElemType = ElemType
NewElems(LocElemID)%Elem%Flag     = Elem%Flag
NewElems(LocElemID)%Elem%Level    = Elem%Level+1

LocSideID = 0
aSide => NewElems(LocElemID)%Elem%FirstSide
DO WHILE (ASSOCIATED(aSide))
  LocSideID = LocSideID+1
  NewSides(LocElemID,LocSideID)%Side => aSide
  aSide => aSide%NextElemSide
END DO

!--------------------------------------------------!
! Create Child = 3
!--------------------------------------------------!
LocElemID = 3
ElemType  = ELEMTYPE_PRISM6
ElemNodes(1)%Node => NewNodes(9)%Node
ElemNodes(2)%Node => NewNodes(8)%Node
ElemNodes(3)%Node => NewNodes(3)%Node
ElemNodes(4)%Node => NewNodes(18)%Node
ElemNodes(5)%Node => NewNodes(17)%Node
ElemNodes(6)%Node => NewNodes(12)%Node
CALL CreateElem_CGNS(NewElems(LocElemID)%Elem,ElemType,1,ElemNodes)
NewElems(LocElemID)%Elem%ElemType = ElemType
NewElems(LocElemID)%Elem%Flag     = Elem%Flag
NewElems(LocElemID)%Elem%Level    = Elem%Level+1

LocSideID = 0
aSide => NewElems(LocElemID)%Elem%FirstSide
DO WHILE (ASSOCIATED(aSide))
  LocSideID = LocSideID+1
  NewSides(LocElemID,LocSideID)%Side => aSide
  aSide => aSide%NextElemSide
END DO

!--------------------------------------------------!
! Create Child = 4
!--------------------------------------------------!
LocElemID = 4
ElemType  = ELEMTYPE_PRISM6
ElemNodes(1)%Node => NewNodes(7)%Node
ElemNodes(2)%Node => NewNodes(8)%Node
ElemNodes(3)%Node => NewNodes(9)%Node
ElemNodes(4)%Node => NewNodes(16)%Node
ElemNodes(5)%Node => NewNodes(17)%Node
ElemNodes(6)%Node => NewNodes(18)%Node
CALL CreateElem_CGNS(NewElems(LocElemID)%Elem,ElemType,1,ElemNodes)
NewElems(LocElemID)%Elem%ElemType = ElemType
NewElems(LocElemID)%Elem%Flag     = Elem%Flag
NewElems(LocElemID)%Elem%Level    = Elem%Level+1

LocSideID = 0
aSide => NewElems(LocElemID)%Elem%FirstSide
DO WHILE (ASSOCIATED(aSide))
  LocSideID = LocSideID+1
  NewSides(LocElemID,LocSideID)%Side => aSide
  aSide => aSide%NextElemSide
END DO

!--------------------------------------------------!
! Create Child = 5
!--------------------------------------------------!
LocElemID = 5
ElemType  = ELEMTYPE_PRISM6
ElemNodes(1)%Node => NewNodes(10)%Node
ElemNodes(2)%Node => NewNodes(16)%Node
ElemNodes(3)%Node => NewNodes(18)%Node
ElemNodes(4)%Node => NewNodes(4)%Node
ElemNodes(5)%Node => NewNodes(13)%Node
ElemNodes(6)%Node => NewNodes(15)%Node
CALL CreateElem_CGNS(NewElems(LocElemID)%Elem,ElemType,1,ElemNodes)
NewElems(LocElemID)%Elem%ElemType = ElemType
NewElems(LocElemID)%Elem%Flag     = Elem%Flag
NewElems(LocElemID)%Elem%Level    = Elem%Level+1

LocSideID = 0
aSide => NewElems(LocElemID)%Elem%FirstSide
DO WHILE (ASSOCIATED(aSide))
  LocSideID = LocSideID+1
  NewSides(LocElemID,LocSideID)%Side => aSide
  aSide => aSide%NextElemSide
END DO

!--------------------------------------------------!
! Create Child = 6
!--------------------------------------------------!
LocElemID = 6
ElemType  = ELEMTYPE_PRISM6
ElemNodes(1)%Node => NewNodes(16)%Node
ElemNodes(2)%Node => NewNodes(11)%Node
ElemNodes(3)%Node => NewNodes(17)%Node
ElemNodes(4)%Node => NewNodes(13)%Node
ElemNodes(5)%Node => NewNodes(5)%Node
ElemNodes(6)%Node => NewNodes(14)%Node
CALL CreateElem_CGNS(NewElems(LocElemID)%Elem,ElemType,1,ElemNodes)
NewElems(LocElemID)%Elem%ElemType = ElemType
NewElems(LocElemID)%Elem%Flag     = Elem%Flag
NewElems(LocElemID)%Elem%Level    = Elem%Level+1

LocSideID = 0
aSide => NewElems(LocElemID)%Elem%FirstSide
DO WHILE (ASSOCIATED(aSide))
  LocSideID = LocSideID+1
  NewSides(LocElemID,LocSideID)%Side => aSide
  aSide => aSide%NextElemSide
END DO

!--------------------------------------------------!
! Create Child = 7
!--------------------------------------------------!
LocElemID = 7
ElemType  = ELEMTYPE_PRISM6
ElemNodes(1)%Node => NewNodes(18)%Node
ElemNodes(2)%Node => NewNodes(17)%Node
ElemNodes(3)%Node => NewNodes(12)%Node
ElemNodes(4)%Node => NewNodes(15)%Node
ElemNodes(5)%Node => NewNodes(14)%Node
ElemNodes(6)%Node => NewNodes(6)%Node
CALL CreateElem_CGNS(NewElems(LocElemID)%Elem,ElemType,1,ElemNodes)
NewElems(LocElemID)%Elem%ElemType = ElemType
NewElems(LocElemID)%Elem%Flag     = Elem%Flag
NewElems(LocElemID)%Elem%Level    = Elem%Level+1

LocSideID = 0
aSide => NewElems(LocElemID)%Elem%FirstSide
DO WHILE (ASSOCIATED(aSide))
  LocSideID = LocSideID+1
  NewSides(LocElemID,LocSideID)%Side => aSide
  aSide => aSide%NextElemSide
END DO

!--------------------------------------------------!
! Create Child = 8
!--------------------------------------------------!
LocElemID = 8
ElemType  = ELEMTYPE_PRISM6
ElemNodes(1)%Node => NewNodes(16)%Node
ElemNodes(2)%Node => NewNodes(17)%Node
ElemNodes(3)%Node => NewNodes(18)%Node
ElemNodes(4)%Node => NewNodes(13)%Node
ElemNodes(5)%Node => NewNodes(14)%Node
ElemNodes(6)%Node => NewNodes(15)%Node
CALL CreateElem_CGNS(NewElems(LocElemID)%Elem,ElemType,1,ElemNodes)
NewElems(LocElemID)%Elem%ElemType = ElemType
NewElems(LocElemID)%Elem%Flag     = Elem%Flag
NewElems(LocElemID)%Elem%Level    = Elem%Level+1

LocSideID = 0
aSide => NewElems(LocElemID)%Elem%FirstSide
DO WHILE (ASSOCIATED(aSide))
  LocSideID = LocSideID+1
  NewSides(LocElemID,LocSideID)%Side => aSide
  aSide => aSide%NextElemSide
END DO

!--------------------------------------------------!
! Set Boundary Conditions
!--------------------------------------------------!
LocSideID = 0
aSide => Elem%FirstSide
DO WHILE (ASSOCIATED(aSide))
  LocSideID = LocSideID+1
  IF (ASSOCIATED(aSide%BC)) THEN
    DO LocElemID=1,nTotalElem
      SELECT CASE (LocElemID)
        CASE(1)
          SELECT CASE (LocSideID)
            CASE(1)
              CALL CopyBC(aSide,NewSides(LocElemID,1)%Side)
            CASE(3)
              CALL CopyBC(aSide,NewSides(LocElemID,3)%Side)
            CASE(4)
              CALL CopyBC(aSide,NewSides(LocElemID,4)%Side)
          END SELECT
        CASE(2)
          SELECT CASE (LocSideID)
            CASE(1)
              CALL CopyBC(aSide,NewSides(LocElemID,1)%Side)
            CASE(2)
              CALL CopyBC(aSide,NewSides(LocElemID,2)%Side)
            CASE(4)
              CALL CopyBC(aSide,NewSides(LocElemID,4)%Side)
          END SELECT
        CASE(3)
          SELECT CASE (LocSideID)
            CASE(2)
              CALL CopyBC(aSide,NewSides(LocElemID,2)%Side)
            CASE(3)
              CALL CopyBC(aSide,NewSides(LocElemID,3)%Side)
            CASE(4)
              CALL CopyBC(aSide,NewSides(LocElemID,4)%Side)
          END SELECT
        CASE(4)
          SELECT CASE (LocSideID)
            CASE(4)
              CALL CopyBC(aSide,NewSides(LocElemID,4)%Side)
          END SELECT
        CASE(5)
          SELECT CASE (LocSideID)
            CASE(1)
              CALL CopyBC(aSide,NewSides(LocElemID,1)%Side)
            CASE(3)
              CALL CopyBC(aSide,NewSides(LocElemID,3)%Side)
            CASE(5)
              CALL CopyBC(aSide,NewSides(LocElemID,5)%Side)
          END SELECT
        CASE(6)
          SELECT CASE (LocSideID)
            CASE(1)
              CALL CopyBC(aSide,NewSides(LocElemID,1)%Side)
            CASE(2)
              CALL CopyBC(aSide,NewSides(LocElemID,2)%Side)
            CASE(5)
              CALL CopyBC(aSide,NewSides(LocElemID,5)%Side)
          END SELECT
        CASE(7)
          SELECT CASE (LocSideID)
            CASE(2)
              CALL CopyBC(aSide,NewSides(LocElemID,2)%Side)
            CASE(3)
              CALL CopyBC(aSide,NewSides(LocElemID,3)%Side)
            CASE(5)
              CALL CopyBC(aSide,NewSides(LocElemID,5)%Side)
          END SELECT
        CASE(8)
          SELECT CASE (LocSideID)
            CASE(5)
              CALL CopyBC(aSide,NewSides(LocElemID,5)%Side)
          END SELECT
      END SELECT
    END DO
  END IF
  aSide => aSide%NextElemSide
END DO

aElem    => Elem
NextElem => Elem%NextElem
PrevElem => Elem%PrevElem

DO LocElemID=1,nTotalElem
  aElem%NextElem          => NewElems(LocElemID)%Elem
  aElem%NextElem%PrevElem => aElem
  aElem                   => aElem%NextElem
END DO

IF (ASSOCIATED(NextElem)) THEN
  aElem%NextElem    => NextElem
  NextElem%PrevElem => aElem
END IF

CALL RemoveElem(Elem,ElemList)

Elem => aElem

IF (ASSOCIATED(PrevElem)) THEN
  NewElems(1)%Elem%PrevElem => PrevElem
  PrevElem%NextElem         => NewElems(1)%Elem
ELSE
  ElemList%FirstElem => NewElems(1)%Elem
END IF

!----------------------------------------------------------------------------------------------------------------------!
END SUBROUTINE SplitElem_PRISM6
!======================================================================================================================!
!
!
!
!======================================================================================================================!
SUBROUTINE SplitElem_PRISM6_XZ(Elem)
!----------------------------------------------------------------------------------------------------------------------!
USE MOD_MeshMain_vars,ONLY: tElem
USE MOD_MeshMain_vars,ONLY: tSide
USE MOD_MeshMain_vars,ONLY: tElemPtr
USE MOD_MeshMain_vars,ONLY: tNodePtr
USE MOD_MeshMain_vars,ONLY: tSidePtr
USE MOD_MeshMain_vars,ONLY: ElemList
!----------------------------------------------------------------------------------------------------------------------!
USE MOD_MeshMainMethods,ONLY: CountElems
USE MOD_MeshMainMethods,ONLY: CopyBC
USE MOD_MeshMainMethods,ONLY: RemoveElem
USE MOD_MeshMainMethods,ONLY: CreateNode
USE MOD_MeshMainMethods,ONLY: CreateElem_CGNS
!----------------------------------------------------------------------------------------------------------------------!
USE MOD_Mesh_CGNS_Definitions
!----------------------------------------------------------------------------------------------------------------------!
IMPLICIT NONE
!----------------------------------------------------------------------------------------------------------------------!
! FORMAL ARGUMENTS
!----------------------------------------------------------------------------------------------------------------------!
TYPE(tElem),POINTER,INTENT(INOUT) :: Elem
!----------------------------------------------------------------------------------------------------------------------!
! LOCAL VARIABLES
!----------------------------------------------------------------------------------------------------------------------!
INTEGER :: ElemType
INTEGER :: LocSideID
INTEGER :: LocElemID
REAL    :: dh
!----------------------------------------------------------------------------------------------------------------------!
INTEGER,PARAMETER :: nElemFaces  = 5
INTEGER,PARAMETER :: nTotalElem  = 4
INTEGER,PARAMETER :: nElemNodes  = 6
INTEGER,PARAMETER :: nTotalNodes = 12
!----------------------------------------------------------------------------------------------------------------------!
TYPE(tElem),POINTER :: aElem
TYPE(tElem),POINTER :: PrevElem
TYPE(tElem),POINTER :: NextElem
TYPE(tSide),POINTER :: aSide
!----------------------------------------------------------------------------------------------------------------------!
TYPE(tElemPtr) :: NewElems(1:nTotalElem)
TYPE(tNodePtr) :: NewNodes(1:nTotalNodes)
TYPE(tSidePtr) :: NewSides(1:nTotalElem,1:nElemFaces)
TYPE(tNodePtr) :: ElemNodes(1:nElemNodes)
!----------------------------------------------------------------------------------------------------------------------!

! WARNING
! For splitting the PRISM6 element into 4 PRISM6 subelements,
! we use the CGNS-PRISM18 nodes numbering

dh = 0.5

NewNodes(1)%Node => Elem%Nodes(1)%Node
NewNodes(2)%Node => Elem%Nodes(2)%Node
NewNodes(3)%Node => Elem%Nodes(3)%Node
NewNodes(4)%Node => Elem%Nodes(4)%Node
NewNodes(5)%Node => Elem%Nodes(5)%Node
NewNodes(6)%Node => Elem%Nodes(6)%Node

! New midnodes at edges
CALL CreateNode(NewNodes(7)%Node)
CALL CreateNode(NewNodes(8)%Node)
CALL CreateNode(NewNodes(9)%Node)
CALL CreateNode(NewNodes(10)%Node)
CALL CreateNode(NewNodes(11)%Node)
CALL CreateNode(NewNodes(12)%Node)

NewNodes(7)%Node%Coords  = dh*(NewNodes(1)%Node%Coords + NewNodes(2)%Node%Coords)
NewNodes(8)%Node%Coords  = dh*(NewNodes(2)%Node%Coords + NewNodes(3)%Node%Coords)
NewNodes(9)%Node%Coords  = dh*(NewNodes(1)%Node%Coords + NewNodes(3)%Node%Coords)
NewNodes(10)%Node%Coords = dh*(NewNodes(4)%Node%Coords + NewNodes(5)%Node%Coords)
NewNodes(11)%Node%Coords = dh*(NewNodes(5)%Node%Coords + NewNodes(6)%Node%Coords)
NewNodes(12)%Node%Coords = dh*(NewNodes(4)%Node%Coords + NewNodes(6)%Node%Coords)

!--------------------------------------------------!
! Create Child = 1
!--------------------------------------------------!
LocElemID = 1
ElemType  = ELEMTYPE_PRISM6
ElemNodes(1)%Node => NewNodes(1)%Node
ElemNodes(2)%Node => NewNodes(7)%Node
ElemNodes(3)%Node => NewNodes(9)%Node
ElemNodes(4)%Node => NewNodes(4)%Node
ElemNodes(5)%Node => NewNodes(10)%Node
ElemNodes(6)%Node => NewNodes(12)%Node
CALL CreateElem_CGNS(NewElems(LocElemID)%Elem,ElemType,1,ElemNodes)
NewElems(LocElemID)%Elem%ElemType = ElemType
NewElems(LocElemID)%Elem%Flag     = Elem%Flag
NewElems(LocElemID)%Elem%Level    = Elem%Level+1

LocSideID = 0
aSide => NewElems(LocElemID)%Elem%FirstSide
DO WHILE (ASSOCIATED(aSide))
  LocSideID = LocSideID+1
  NewSides(LocElemID,LocSideID)%Side => aSide
  aSide => aSide%NextElemSide
END DO

!--------------------------------------------------!
! Create Child = 2
!--------------------------------------------------!
LocElemID = 2
ElemType  = ELEMTYPE_PRISM6
ElemNodes(1)%Node => NewNodes(7)%Node
ElemNodes(2)%Node => NewNodes(2)%Node
ElemNodes(3)%Node => NewNodes(8)%Node
ElemNodes(4)%Node => NewNodes(10)%Node
ElemNodes(5)%Node => NewNodes(5)%Node
ElemNodes(6)%Node => NewNodes(11)%Node
CALL CreateElem_CGNS(NewElems(LocElemID)%Elem,ElemType,1,ElemNodes)
NewElems(LocElemID)%Elem%ElemType = ElemType
NewElems(LocElemID)%Elem%Flag     = Elem%Flag
NewElems(LocElemID)%Elem%Level    = Elem%Level+1

LocSideID = 0
aSide => NewElems(LocElemID)%Elem%FirstSide
DO WHILE (ASSOCIATED(aSide))
  LocSideID = LocSideID+1
  NewSides(LocElemID,LocSideID)%Side => aSide
  aSide => aSide%NextElemSide
END DO

!--------------------------------------------------!
! Create Child = 3
!--------------------------------------------------!
LocElemID = 3
ElemType  = ELEMTYPE_PRISM6
ElemNodes(1)%Node => NewNodes(9)%Node
ElemNodes(2)%Node => NewNodes(8)%Node
ElemNodes(3)%Node => NewNodes(3)%Node
ElemNodes(4)%Node => NewNodes(12)%Node
ElemNodes(5)%Node => NewNodes(11)%Node
ElemNodes(6)%Node => NewNodes(6)%Node
CALL CreateElem_CGNS(NewElems(LocElemID)%Elem,ElemType,1,ElemNodes)
NewElems(LocElemID)%Elem%ElemType = ElemType
NewElems(LocElemID)%Elem%Flag     = Elem%Flag
NewElems(LocElemID)%Elem%Level    = Elem%Level+1

LocSideID = 0
aSide => NewElems(LocElemID)%Elem%FirstSide
DO WHILE (ASSOCIATED(aSide))
  LocSideID = LocSideID+1
  NewSides(LocElemID,LocSideID)%Side => aSide
  aSide => aSide%NextElemSide
END DO

!--------------------------------------------------!
! Create Child = 4
!--------------------------------------------------!
LocElemID = 4
ElemType  = ELEMTYPE_PRISM6
ElemNodes(1)%Node => NewNodes(7)%Node
ElemNodes(2)%Node => NewNodes(8)%Node
ElemNodes(3)%Node => NewNodes(9)%Node
ElemNodes(4)%Node => NewNodes(10)%Node
ElemNodes(5)%Node => NewNodes(11)%Node
ElemNodes(6)%Node => NewNodes(12)%Node
CALL CreateElem_CGNS(NewElems(LocElemID)%Elem,ElemType,1,ElemNodes)
NewElems(LocElemID)%Elem%ElemType = ElemType
NewElems(LocElemID)%Elem%Flag     = Elem%Flag
NewElems(LocElemID)%Elem%Level    = Elem%Level+1

LocSideID = 0
aSide => NewElems(LocElemID)%Elem%FirstSide
DO WHILE (ASSOCIATED(aSide))
  LocSideID = LocSideID+1
  NewSides(LocElemID,LocSideID)%Side => aSide
  aSide => aSide%NextElemSide
END DO

!--------------------------------------------------!
! Set Boundary Conditions
!--------------------------------------------------!
LocSideID = 0
aSide => Elem%FirstSide
DO WHILE (ASSOCIATED(aSide))
  LocSideID = LocSideID+1
  IF (ASSOCIATED(aSide%BC)) THEN
    DO LocElemID=1,nTotalElem
      SELECT CASE (LocElemID)
        CASE(1)
          SELECT CASE (LocSideID)
            CASE(1)
              CALL CopyBC(aSide,NewSides(LocElemID,1)%Side)
            CASE(3)
              CALL CopyBC(aSide,NewSides(LocElemID,3)%Side)
            CASE(4)
              CALL CopyBC(aSide,NewSides(LocElemID,4)%Side)
            CASE(5)
              CALL CopyBC(aSide,NewSides(LocElemID,5)%Side)
          END SELECT
        CASE(2)
          SELECT CASE (LocSideID)
            CASE(1)
              CALL CopyBC(aSide,NewSides(LocElemID,1)%Side)
            CASE(2)
              CALL CopyBC(aSide,NewSides(LocElemID,2)%Side)
            CASE(4)
              CALL CopyBC(aSide,NewSides(LocElemID,4)%Side)
            CASE(5)
              CALL CopyBC(aSide,NewSides(LocElemID,5)%Side)
          END SELECT
        CASE(3)
          SELECT CASE (LocSideID)
            CASE(2)
              CALL CopyBC(aSide,NewSides(LocElemID,2)%Side)
            CASE(3)
              CALL CopyBC(aSide,NewSides(LocElemID,3)%Side)
            CASE(4)
              CALL CopyBC(aSide,NewSides(LocElemID,4)%Side)
            CASE(5)
              CALL CopyBC(aSide,NewSides(LocElemID,5)%Side)
          END SELECT
        CASE(4)
          SELECT CASE (LocSideID)
            CASE(4)
              CALL CopyBC(aSide,NewSides(LocElemID,4)%Side)
            CASE(5)
              CALL CopyBC(aSide,NewSides(LocElemID,5)%Side)
          END SELECT
      END SELECT
    END DO
  END IF
  aSide => aSide%NextElemSide
END DO

aElem    => Elem
NextElem => Elem%NextElem
PrevElem => Elem%PrevElem

DO LocElemID=1,nTotalElem
  aElem%NextElem          => NewElems(LocElemID)%Elem
  aElem%NextElem%PrevElem => aElem
  aElem                   => aElem%NextElem
END DO

IF (ASSOCIATED(NextElem)) THEN
  aElem%NextElem    => NextElem
  NextElem%PrevElem => aElem
END IF

CALL RemoveElem(Elem,ElemList)

Elem => aElem

IF (ASSOCIATED(PrevElem)) THEN
  NewElems(1)%Elem%PrevElem => PrevElem
  PrevElem%NextElem         => NewElems(1)%Elem
ELSE
  ElemList%FirstElem => NewElems(1)%Elem
END IF

!----------------------------------------------------------------------------------------------------------------------!
END SUBROUTINE SplitElem_PRISM6_XZ
!======================================================================================================================!
!
!
!
!======================================================================================================================!
SUBROUTINE SplitElem_PYRA5(Elem)
!----------------------------------------------------------------------------------------------------------------------!
USE MOD_MeshMain_vars,ONLY: tElem
USE MOD_MeshMain_vars,ONLY: tSide
USE MOD_MeshMain_vars,ONLY: tElemPtr
USE MOD_MeshMain_vars,ONLY: tNodePtr
USE MOD_MeshMain_vars,ONLY: tSidePtr
USE MOD_MeshMain_vars,ONLY: ElemList
!----------------------------------------------------------------------------------------------------------------------!
USE MOD_MeshMainMethods,ONLY: CountElems
USE MOD_MeshMainMethods,ONLY: CopyBC
USE MOD_MeshMainMethods,ONLY: RemoveElem
USE MOD_MeshMainMethods,ONLY: CreateNode
USE MOD_MeshMainMethods,ONLY: CreateElem_CGNS
!----------------------------------------------------------------------------------------------------------------------!
USE MOD_Mesh_CGNS_Definitions
!----------------------------------------------------------------------------------------------------------------------!
IMPLICIT NONE
!----------------------------------------------------------------------------------------------------------------------!
! FORMAL ARGUMENTS
!----------------------------------------------------------------------------------------------------------------------!
TYPE(tElem),POINTER,INTENT(INOUT) :: Elem
!----------------------------------------------------------------------------------------------------------------------!
! LOCAL VARIABLES
!----------------------------------------------------------------------------------------------------------------------!
INTEGER :: ElemType
INTEGER :: LocSideID
INTEGER :: LocElemID
REAL    :: dh
!----------------------------------------------------------------------------------------------------------------------!
INTEGER,PARAMETER :: nTotalElem  = 10
INTEGER,PARAMETER :: nTotalNodes = 14
INTEGER,PARAMETER :: nElemFaces_TETRA4 = 4
INTEGER,PARAMETER :: nElemFaces_PYRA5  = 5
INTEGER,PARAMETER :: nElemNodes_TETRA4 = 4
INTEGER,PARAMETER :: nElemNodes_PYRA5  = 5
!----------------------------------------------------------------------------------------------------------------------!
TYPE(tElem),POINTER :: aElem
TYPE(tElem),POINTER :: PrevElem
TYPE(tElem),POINTER :: NextElem
TYPE(tSide),POINTER :: aSide
!----------------------------------------------------------------------------------------------------------------------!
TYPE(tElemPtr) :: NewElems(1:nTotalElem)
TYPE(tNodePtr) :: NewNodes(1:nTotalNodes)
TYPE(tSidePtr) :: NewSides(1:nTotalElem,1:MAX(nElemFaces_TETRA4,nElemFaces_PYRA5))
TYPE(tNodePtr) :: ElemNodes(1:MAX(nElemNodes_TETRA4,nElemNodes_PYRA5))
!----------------------------------------------------------------------------------------------------------------------!

! For splitting the PYRA5 element into 6 PYRA5 and 4 TETRA4 subelements,
! we use the CGNS-PYRA14 nodes numbering

dh = 0.5

NewNodes(1)%Node => Elem%Nodes(1)%Node
NewNodes(2)%Node => Elem%Nodes(2)%Node
NewNodes(3)%Node => Elem%Nodes(3)%Node
NewNodes(4)%Node => Elem%Nodes(4)%Node
NewNodes(5)%Node => Elem%Nodes(5)%Node

! New midnodes at edges
CALL CreateNode(NewNodes(6)%Node)
CALL CreateNode(NewNodes(7)%Node)
CALL CreateNode(NewNodes(8)%Node)
CALL CreateNode(NewNodes(9)%Node)
CALL CreateNode(NewNodes(10)%Node)
CALL CreateNode(NewNodes(11)%Node)
CALL CreateNode(NewNodes(12)%Node)
CALL CreateNode(NewNodes(13)%Node)

NewNodes(6)%Node%Coords  = dh*(NewNodes(1)%Node%Coords + NewNodes(2)%Node%Coords)
NewNodes(7)%Node%Coords  = dh*(NewNodes(2)%Node%Coords + NewNodes(3)%Node%Coords)
NewNodes(8)%Node%Coords  = dh*(NewNodes(3)%Node%Coords + NewNodes(4)%Node%Coords)
NewNodes(9)%Node%Coords  = dh*(NewNodes(1)%Node%Coords + NewNodes(4)%Node%Coords)
NewNodes(10)%Node%Coords = dh*(NewNodes(1)%Node%Coords + NewNodes(5)%Node%Coords)
NewNodes(11)%Node%Coords = dh*(NewNodes(2)%Node%Coords + NewNodes(5)%Node%Coords)
NewNodes(12)%Node%Coords = dh*(NewNodes(3)%Node%Coords + NewNodes(5)%Node%Coords)
NewNodes(13)%Node%Coords = dh*(NewNodes(4)%Node%Coords + NewNodes(5)%Node%Coords)

! New midnodes at bottom face
CALL CreateNode(NewNodes(14)%Node)

NewNodes(14)%Node%Coords = dh*(NewNodes(1)%Node%Coords + NewNodes(3)%Node%Coords)

!--------------------------------------------------!
! Create Child = 1
!--------------------------------------------------!
LocElemID = 1
ElemType  = ELEMTYPE_PYRA5
ElemNodes(1)%Node => NewNodes(1)%Node
ElemNodes(2)%Node => NewNodes(6)%Node
ElemNodes(3)%Node => NewNodes(14)%Node
ElemNodes(4)%Node => NewNodes(9)%Node
ElemNodes(5)%Node => NewNodes(10)%Node
CALL CreateElem_CGNS(NewElems(LocElemID)%Elem,ElemType,1,ElemNodes(1:nElemNodes_PYRA5))
NewElems(LocElemID)%Elem%ElemType = ElemType
NewElems(LocElemID)%Elem%Flag     = Elem%Flag
NewElems(LocElemID)%Elem%Level    = Elem%Level+1

LocSideID = 0
aSide => NewElems(LocElemID)%Elem%FirstSide
DO WHILE (ASSOCIATED(aSide))
  LocSideID = LocSideID+1
  NewSides(LocElemID,LocSideID)%Side => aSide
  aSide => aSide%NextElemSide
END DO

!--------------------------------------------------!
! Create Child = 2
!--------------------------------------------------!
LocElemID = 2
ElemType  = ELEMTYPE_PYRA5
ElemNodes(1)%Node => NewNodes(6)%Node
ElemNodes(2)%Node => NewNodes(2)%Node
ElemNodes(3)%Node => NewNodes(7)%Node
ElemNodes(4)%Node => NewNodes(14)%Node
ElemNodes(5)%Node => NewNodes(11)%Node
CALL CreateElem_CGNS(NewElems(LocElemID)%Elem,ElemType,1,ElemNodes(1:nElemNodes_PYRA5))
NewElems(LocElemID)%Elem%ElemType = ElemType
NewElems(LocElemID)%Elem%Flag     = Elem%Flag
NewElems(LocElemID)%Elem%Level    = Elem%Level+1

LocSideID = 0
aSide => NewElems(LocElemID)%Elem%FirstSide
DO WHILE (ASSOCIATED(aSide))
  LocSideID = LocSideID+1
  NewSides(LocElemID,LocSideID)%Side => aSide
  aSide => aSide%NextElemSide
END DO

!--------------------------------------------------!
! Create Child = 3
!--------------------------------------------------!
LocElemID = 3
ElemType  = ELEMTYPE_PYRA5
ElemNodes(1)%Node => NewNodes(9)%Node
ElemNodes(2)%Node => NewNodes(14)%Node
ElemNodes(3)%Node => NewNodes(8)%Node
ElemNodes(4)%Node => NewNodes(4)%Node
ElemNodes(5)%Node => NewNodes(13)%Node
CALL CreateElem_CGNS(NewElems(LocElemID)%Elem,ElemType,1,ElemNodes(1:nElemNodes_PYRA5))
NewElems(LocElemID)%Elem%ElemType = ElemType
NewElems(LocElemID)%Elem%Flag     = Elem%Flag
NewElems(LocElemID)%Elem%Level    = Elem%Level+1

LocSideID = 0
aSide => NewElems(LocElemID)%Elem%FirstSide
DO WHILE (ASSOCIATED(aSide))
  LocSideID = LocSideID+1
  NewSides(LocElemID,LocSideID)%Side => aSide
  aSide => aSide%NextElemSide
END DO

!--------------------------------------------------!
! Create Child = 4
!--------------------------------------------------!
LocElemID = 4
ElemType  = ELEMTYPE_PYRA5
ElemNodes(1)%Node => NewNodes(14)%Node
ElemNodes(2)%Node => NewNodes(7)%Node
ElemNodes(3)%Node => NewNodes(3)%Node
ElemNodes(4)%Node => NewNodes(8)%Node
ElemNodes(5)%Node => NewNodes(12)%Node
CALL CreateElem_CGNS(NewElems(LocElemID)%Elem,ElemType,1,ElemNodes(1:nElemNodes_PYRA5))
NewElems(LocElemID)%Elem%ElemType = ElemType
NewElems(LocElemID)%Elem%Flag     = Elem%Flag
NewElems(LocElemID)%Elem%Level    = Elem%Level+1

LocSideID = 0
aSide => NewElems(LocElemID)%Elem%FirstSide
DO WHILE (ASSOCIATED(aSide))
  LocSideID = LocSideID+1
  NewSides(LocElemID,LocSideID)%Side => aSide
  aSide => aSide%NextElemSide
END DO

!--------------------------------------------------!
! Create Child = 5
!--------------------------------------------------!
LocElemID = 5
ElemType  = ELEMTYPE_PYRA5
ElemNodes(1)%Node => NewNodes(10)%Node
ElemNodes(2)%Node => NewNodes(11)%Node
ElemNodes(3)%Node => NewNodes(12)%Node
ElemNodes(4)%Node => NewNodes(13)%Node
ElemNodes(5)%Node => NewNodes(5)%Node
CALL CreateElem_CGNS(NewElems(LocElemID)%Elem,ElemType,1,ElemNodes(1:nElemNodes_PYRA5))
NewElems(LocElemID)%Elem%ElemType = ElemType
NewElems(LocElemID)%Elem%Flag     = Elem%Flag
NewElems(LocElemID)%Elem%Level    = Elem%Level+1

LocSideID = 0
aSide => NewElems(LocElemID)%Elem%FirstSide
DO WHILE (ASSOCIATED(aSide))
  LocSideID = LocSideID+1
  NewSides(LocElemID,LocSideID)%Side => aSide
  aSide => aSide%NextElemSide
END DO

!--------------------------------------------------!
! Create Child = 6
!--------------------------------------------------!
LocElemID = 6
ElemType  = ELEMTYPE_PYRA5
ElemNodes(1)%Node => NewNodes(10)%Node
ElemNodes(2)%Node => NewNodes(13)%Node
ElemNodes(3)%Node => NewNodes(12)%Node
ElemNodes(4)%Node => NewNodes(11)%Node
ElemNodes(5)%Node => NewNodes(14)%Node
CALL CreateElem_CGNS(NewElems(LocElemID)%Elem,ElemType,1,ElemNodes(1:nElemNodes_PYRA5))
NewElems(LocElemID)%Elem%ElemType = ElemType
NewElems(LocElemID)%Elem%Flag     = Elem%Flag
NewElems(LocElemID)%Elem%Level    = Elem%Level+1

LocSideID = 0
aSide => NewElems(LocElemID)%Elem%FirstSide
DO WHILE (ASSOCIATED(aSide))
  LocSideID = LocSideID+1
  NewSides(LocElemID,LocSideID)%Side => aSide
  aSide => aSide%NextElemSide
END DO

!--------------------------------------------------!
! Create Child = 7
!--------------------------------------------------!
LocElemID = 7
ElemType  = ELEMTYPE_TETRA4
ElemNodes(1)%Node => NewNodes(6)%Node
ElemNodes(2)%Node => NewNodes(10)%Node
ElemNodes(3)%Node => NewNodes(11)%Node
ElemNodes(4)%Node => NewNodes(14)%Node
CALL CreateElem_CGNS(NewElems(LocElemID)%Elem,ElemType,1,ElemNodes(1:nElemNodes_TETRA4))
NewElems(LocElemID)%Elem%ElemType = ElemType
NewElems(LocElemID)%Elem%Flag     = Elem%Flag
NewElems(LocElemID)%Elem%Level    = Elem%Level+1

LocSideID = 0
aSide => NewElems(LocElemID)%Elem%FirstSide
DO WHILE (ASSOCIATED(aSide))
  LocSideID = LocSideID+1
  NewSides(LocElemID,LocSideID)%Side => aSide
  aSide => aSide%NextElemSide
END DO

!--------------------------------------------------!
! Create Child = 8
!--------------------------------------------------!
LocElemID = 8
ElemType  = ELEMTYPE_TETRA4
ElemNodes(1)%Node => NewNodes(7)%Node
ElemNodes(2)%Node => NewNodes(11)%Node
ElemNodes(3)%Node => NewNodes(12)%Node
ElemNodes(4)%Node => NewNodes(14)%Node
CALL CreateElem_CGNS(NewElems(LocElemID)%Elem,ElemType,1,ElemNodes(1:nElemNodes_TETRA4))
NewElems(LocElemID)%Elem%ElemType = ElemType
NewElems(LocElemID)%Elem%Flag     = Elem%Flag
NewElems(LocElemID)%Elem%Level    = Elem%Level+1

LocSideID = 0
aSide => NewElems(LocElemID)%Elem%FirstSide
DO WHILE (ASSOCIATED(aSide))
  LocSideID = LocSideID+1
  NewSides(LocElemID,LocSideID)%Side => aSide
  aSide => aSide%NextElemSide
END DO

!--------------------------------------------------!
! Create Child = 9
!--------------------------------------------------!
LocElemID = 9
ElemType  = ELEMTYPE_TETRA4
ElemNodes(1)%Node => NewNodes(8)%Node
ElemNodes(2)%Node => NewNodes(12)%Node
ElemNodes(3)%Node => NewNodes(13)%Node
ElemNodes(4)%Node => NewNodes(14)%Node
CALL CreateElem_CGNS(NewElems(LocElemID)%Elem,ElemType,1,ElemNodes(1:nElemNodes_TETRA4))
NewElems(LocElemID)%Elem%ElemType = ElemType
NewElems(LocElemID)%Elem%Flag     = Elem%Flag
NewElems(LocElemID)%Elem%Level    = Elem%Level+1

LocSideID = 0
aSide => NewElems(LocElemID)%Elem%FirstSide
DO WHILE (ASSOCIATED(aSide))
  LocSideID = LocSideID+1
  NewSides(LocElemID,LocSideID)%Side => aSide
  aSide => aSide%NextElemSide
END DO

!--------------------------------------------------!
! Create Child = 10
!--------------------------------------------------!
LocElemID = 10
ElemType  = ELEMTYPE_TETRA4
ElemNodes(1)%Node => NewNodes(9)%Node
ElemNodes(2)%Node => NewNodes(13)%Node
ElemNodes(3)%Node => NewNodes(10)%Node
ElemNodes(4)%Node => NewNodes(14)%Node
CALL CreateElem_CGNS(NewElems(LocElemID)%Elem,ElemType,1,ElemNodes(1:nElemNodes_TETRA4))
NewElems(LocElemID)%Elem%ElemType = ElemType
NewElems(LocElemID)%Elem%Flag     = Elem%Flag
NewElems(LocElemID)%Elem%Level    = Elem%Level+1

LocSideID = 0
aSide => NewElems(LocElemID)%Elem%FirstSide
DO WHILE (ASSOCIATED(aSide))
  LocSideID = LocSideID+1
  NewSides(LocElemID,LocSideID)%Side => aSide
  aSide => aSide%NextElemSide
END DO

!--------------------------------------------------!
! Set Boundary Conditions
!--------------------------------------------------!
LocSideID = 0
aSide => Elem%FirstSide
DO WHILE (ASSOCIATED(aSide))
  LocSideID = LocSideID+1
  IF (ASSOCIATED(aSide%BC)) THEN
    DO LocElemID=1,nTotalElem
      SELECT CASE (LocElemID)
        CASE(1)
          SELECT CASE (LocSideID)
            CASE(1)
              CALL CopyBC(aSide,NewSides(LocElemID,1)%Side)
            CASE(2)
              CALL CopyBC(aSide,NewSides(LocElemID,2)%Side)
            CASE(5)
              CALL CopyBC(aSide,NewSides(LocElemID,5)%Side)
          END SELECT
        CASE(2)
          SELECT CASE (LocSideID)
            CASE(1)
              CALL CopyBC(aSide,NewSides(LocElemID,1)%Side)
            CASE(2)
              CALL CopyBC(aSide,NewSides(LocElemID,2)%Side)
            CASE(3)
              CALL CopyBC(aSide,NewSides(LocElemID,3)%Side)
          END SELECT
        CASE(3)
          SELECT CASE (LocSideID)
            CASE(1)
              CALL CopyBC(aSide,NewSides(LocElemID,1)%Side)
            CASE(4)
              CALL CopyBC(aSide,NewSides(LocElemID,4)%Side)
            CASE(5)
              CALL CopyBC(aSide,NewSides(LocElemID,5)%Side)
          END SELECT
        CASE(4)
          SELECT CASE (LocSideID)
            CASE(1)
              CALL CopyBC(aSide,NewSides(LocElemID,1)%Side)
            CASE(3)
              CALL CopyBC(aSide,NewSides(LocElemID,3)%Side)
            CASE(4)
              CALL CopyBC(aSide,NewSides(LocElemID,4)%Side)
          END SELECT
        CASE(5)
          SELECT CASE (LocSideID)
            CASE(2)
              CALL CopyBC(aSide,NewSides(LocElemID,2)%Side)
            CASE(3)
              CALL CopyBC(aSide,NewSides(LocElemID,3)%Side)
            CASE(4)
              CALL CopyBC(aSide,NewSides(LocElemID,4)%Side)
            CASE(5)
              CALL CopyBC(aSide,NewSides(LocElemID,5)%Side)
          END SELECT
        CASE(7)
          SELECT CASE (LocSideID)
            CASE(2)
              CALL CopyBC(aSide,NewSides(LocElemID,1)%Side)
          END SELECT
        CASE(8)
          SELECT CASE (LocSideID)
            CASE(3)
              CALL CopyBC(aSide,NewSides(LocElemID,1)%Side)
          END SELECT
        CASE(9)
          SELECT CASE (LocSideID)
            CASE(4)
              CALL CopyBC(aSide,NewSides(LocElemID,1)%Side)
          END SELECT
        CASE(10)
          SELECT CASE (LocSideID)
            CASE(5)
              CALL CopyBC(aSide,NewSides(LocElemID,1)%Side)
          END SELECT
      END SELECT
    END DO
  END IF
  aSide => aSide%NextElemSide
END DO

aElem    => Elem
NextElem => Elem%NextElem
PrevElem => Elem%PrevElem

DO LocElemID=1,nTotalElem
  aElem%NextElem          => NewElems(LocElemID)%Elem
  aElem%NextElem%PrevElem => aElem
  aElem                   => aElem%NextElem
END DO

IF (ASSOCIATED(NextElem)) THEN
  aElem%NextElem    => NextElem
  NextElem%PrevElem => aElem
END IF

CALL RemoveElem(Elem,ElemList)

Elem => aElem

IF (ASSOCIATED(PrevElem)) THEN
  NewElems(1)%Elem%PrevElem => PrevElem
  PrevElem%NextElem         => NewElems(1)%Elem
ELSE
  ElemList%FirstElem => NewElems(1)%Elem
END IF

!----------------------------------------------------------------------------------------------------------------------!
END SUBROUTINE SplitElem_PYRA5
!======================================================================================================================!
!
!
!
!----------------------------------------------------------------------------------------------------------------------!
END MODULE MOD_MeshRefinementSplitting
!======================================================================================================================!
