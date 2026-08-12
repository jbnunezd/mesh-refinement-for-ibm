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
MODULE MOD_MeshExportElementsList
!----------------------------------------------------------------------------------------------------------------------!
USE MOD_GLOBAL_vars
!----------------------------------------------------------------------------------------------------------------------!
IMPLICIT NONE
!----------------------------------------------------------------------------------------------------------------------!
PRIVATE
!----------------------------------------------------------------------------------------------------------------------!
INTERFACE CreateMeshDataArrays
  MODULE PROCEDURE CreateMeshDataArrays
END INTERFACE

INTERFACE CountElemsNodesBCFaces
  MODULE PROCEDURE CountElemsNodesBCFaces
END INTERFACE
!----------------------------------------------------------------------------------------------------------------------!
INTERFACE CreateArray_ElementsType
  MODULE PROCEDURE CreateArray_ElementsType
END INTERFACE

INTERFACE CreateArray_FacesToNodes
  MODULE PROCEDURE CreateArray_FacesToNodes
END INTERFACE

INTERFACE CreateArray_EdgesToNodes
  MODULE PROCEDURE CreateArray_EdgesToNodes
END INTERFACE

INTERFACE CreateArray_FacesToEdges
  MODULE PROCEDURE CreateArray_FacesToEdges
END INTERFACE

INTERFACE CreateArray_BCFacesToNodes
  MODULE PROCEDURE CreateArray_BCFacesToNodes
END INTERFACE

INTERFACE CreateArray_ElementsToFaces
  MODULE PROCEDURE CreateArray_ElementsToFaces
END INTERFACE

INTERFACE CreateArray_ElementsToLevel
  MODULE PROCEDURE CreateArray_ElementsToLevel
END INTERFACE

INTERFACE CreateArray_ElementsToNodes
  MODULE PROCEDURE CreateArray_ElementsToNodes
END INTERFACE

INTERFACE CreateArray_NodesCoordinates
  MODULE PROCEDURE CreateArray_NodesCoordinates
END INTERFACE

INTERFACE CreateArray_ElementsToNodesCoordinates
  MODULE PROCEDURE CreateArray_ElementsToNodesCoordinates
END INTERFACE

INTERFACE CreateArray_NonConformingFacesToNodes
  MODULE PROCEDURE CreateArray_NonConformingFacesToNodes
END INTERFACE
!----------------------------------------------------------------------------------------------------------------------!
INTERFACE CreateNodesToEdgesHashMaps
  MODULE PROCEDURE CreateNodesToEdgesHashMaps
END INTERFACE

INTERFACE CreateNodesToFacesHashMaps
  MODULE PROCEDURE CreateNodesToFacesHashMaps
END INTERFACE
!----------------------------------------------------------------------------------------------------------------------!
PUBLIC :: CreateMeshDataArrays
PUBLIC :: CountElemsNodesBCFaces
!----------------------------------------------------------------------------------------------------------------------!
PUBLIC :: CreateArray_ElementsType
PUBLIC :: CreateArray_FacesToNodes
PUBLIC :: CreateArray_EdgesToNodes
PUBLIC :: CreateArray_FacesToEdges
PUBLIC :: CreateArray_BCFacesToNodes
PUBLIC :: CreateArray_ElementsToFaces
PUBLIC :: CreateArray_ElementsToLevel
PUBLIC :: CreateArray_ElementsToNodes
PUBLIC :: CreateArray_NodesCoordinates
PUBLIC :: CreateArray_NonConformingFacesToNodes
PUBLIC :: CreateArray_ElementsToNodesCoordinates
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
SUBROUTINE CreateMeshDataArrays(Debug)
!----------------------------------------------------------------------------------------------------------------------!
IMPLICIT NONE
!----------------------------------------------------------------------------------------------------------------------!
! FORMAL ARGUMENTS
!----------------------------------------------------------------------------------------------------------------------!
LOGICAL,INTENT(IN),OPTIONAL :: Debug
!----------------------------------------------------------------------------------------------------------------------!
! LOCAL VARIABLES
!----------------------------------------------------------------------------------------------------------------------!
INTEGER :: nTabIn
LOGICAL :: DebugMode
!----------------------------------------------------------------------------------------------------------------------!
! ! ! CHARACTER(LEN=256) :: ErrorMessage
!----------------------------------------------------------------------------------------------------------------------!
REAL               :: CalcTimeIni
REAL               :: CalcTimeEnd
CHARACTER(LEN=256) :: ElapsedTime
CHARACTER(LEN=256) :: Header
!----------------------------------------------------------------------------------------------------------------------!

nTabIn = 2
Header = "CREATING MESH ARRAYS"
CALL PrintMessage(Header,nTabIn=nTabIn)

CalcTimeIni = RunningTime()

IF (PRESENT(Debug)) THEN
  DebugMode = Debug
ELSE
  DebugMode = .FALSE.
END IF

! Count Elements, Nodes, and BCFaces
CALL CountElemsNodesBCFaces(Debug=DebugMode)

! Count Elements, Nodes, and BCFaces
CALL CreateArray_ElementsType(Debug=DebugMode)

! Save Nodes-Coordinates in Array
CALL CreateArray_NodesCoordinates(Debug=DebugMode)

! Save Elements-Nodes-Coordinates in Array
CALL CreateArray_ElementsToNodesCoordinates(Debug=DebugMode)

! Save Elements-to-Nodes in Array
CALL CreateArray_ElementsToNodes(Debug=DebugMode)

! Save Elements-Level in Array
CALL CreateArray_ElementsToLevel(Debug=DebugMode)

! Save BCFaces-to-Nodes in Array
CALL CreateArray_BCFacesToNodes(Debug=DebugMode)

! Save Faces-to-Nodes in Array
CALL CreateArray_FacesToNodes(Debug=DebugMode)

! ! ! ! Save Edges-to-Nodes in Array
! ! ! CALL CreateArray_EdgesToNodes(Debug=DebugMode)

! Save Elements-to-Faces in Array
CALL CreateArray_ElementsToFaces(Debug=DebugMode)

! ! ! CalcTimeIni = RunningTime()

! Save Mesh-related HashMaps
! ! ! CALL CreateNodesToEdgesHashMaps(Debug=DebugMode)
CALL CreateNodesToFacesHashMaps(Debug=DebugMode)

! Save Faces-to-Edges in Array
! ! ! CALL CreateArray_FacesToEdges(Debug=DebugMode)

! Save NonConforming-Faces-to-Nodes in Array
CALL CreateArray_NonConformingFacesToNodes(Debug=DebugMode)

CalcTimeEnd = RunningTime()
CALL ComputeRuntime(CalcTimeEnd-CalcTimeIni,ElapsedTime)
Header = "Elapsed Time"
CALL PrintAnalyze(Header,ElapsedTime,nTabIn=4)

!----------------------------------------------------------------------------------------------------------------------!
END SUBROUTINE CreateMeshDataArrays
!======================================================================================================================!
!
!
!
!======================================================================================================================!
SUBROUTINE CountElemsNodesBCFaces(Debug)
!----------------------------------------------------------------------------------------------------------------------!
USE MOD_MeshMain_vars,ONLY: tElem
USE MOD_MeshMain_vars,ONLY: ElemList
USE MOD_MeshMain_vars,ONLY: MeshInfo
USE MOD_MeshMain_vars,ONLY: MeshArraysInfo
!----------------------------------------------------------------------------------------------------------------------!
USE MOD_MeshMainMethods,ONLY: CountNodes
USE MOD_MeshMainMethods,ONLY: CountElems
USE MOD_MeshMainMethods,ONLY: CountBCFaces
!----------------------------------------------------------------------------------------------------------------------!
IMPLICIT NONE
!----------------------------------------------------------------------------------------------------------------------!
! FORMAL ARGUMENTS
!----------------------------------------------------------------------------------------------------------------------!
LOGICAL,INTENT(IN),OPTIONAL :: Debug
!----------------------------------------------------------------------------------------------------------------------!
! LOCAL VARIABLES
!----------------------------------------------------------------------------------------------------------------------!
INTEGER :: nElems
INTEGER :: nNodes
INTEGER :: nBCFaces
LOGICAL :: DebugMode
!----------------------------------------------------------------------------------------------------------------------!

IF (PRESENT(Debug)) THEN
  DebugMode = Debug
ELSE
  DebugMode = .FALSE.
END IF

! Count Elements
CALL CountElems(ElemList,nElems)

! Count Nodes
CALL CountNodes(ElemList,nNodes)

! Count BCFaces
CALL CountBCFaces(ElemList,nBCFaces)

MeshInfo%nElems = nElems
MeshInfo%nNodes = nNodes

MeshArraysInfo%nElems   = nElems
MeshArraysInfo%nNodes   = nNodes
MeshArraysInfo%nBCFaces = nBCFaces

IF (DebugMode .EQV. .TRUE.) THEN
  WRITE(UNIT_SCREEN,*)
  WRITE(UNIT_SCREEN,"(2X,A)") "======================================================="
  WRITE(UNIT_SCREEN,"(2X,A)") "Printing MeshInfo"
  WRITE(UNIT_SCREEN,"(2X,A)") "======================================================="
  WRITE(UNIT_SCREEN,"(2X,A,I0)") "nElems   = ", nElems
  WRITE(UNIT_SCREEN,"(2X,A,I0)") "nNodes   = ", nNodes
  WRITE(UNIT_SCREEN,"(2X,A,I0)") "nBCFaces = ", nBCFaces
  WRITE(UNIT_SCREEN,*)
END IF

!----------------------------------------------------------------------------------------------------------------------!
END SUBROUTINE CountElemsNodesBCFaces
!======================================================================================================================!
!
!
!
!======================================================================================================================!
SUBROUTINE CreateArray_ElementsType(Debug)
!----------------------------------------------------------------------------------------------------------------------!
USE MOD_MeshMain_vars,ONLY: tElem
USE MOD_MeshMain_vars,ONLY: ElemList
!----------------------------------------------------------------------------------------------------------------------!
USE MOD_MeshMain_vars,ONLY: MeshArraysInfo
USE MOD_MeshMain_vars,ONLY: MeshData_ElementsToElementType
!----------------------------------------------------------------------------------------------------------------------!
IMPLICIT NONE
!----------------------------------------------------------------------------------------------------------------------!
! FORMAL ARGUMENTS
!----------------------------------------------------------------------------------------------------------------------!
LOGICAL,INTENT(IN),OPTIONAL :: Debug
!----------------------------------------------------------------------------------------------------------------------!
! LOCAL VARIABLES
!----------------------------------------------------------------------------------------------------------------------!
INTEGER :: ElemID
INTEGER :: nElems
INTEGER :: ElemType
LOGICAL :: DebugMode
!----------------------------------------------------------------------------------------------------------------------!
TYPE(tElem),POINTER :: aElem
!----------------------------------------------------------------------------------------------------------------------!
CHARACTER(LEN=256) :: FormatString
!----------------------------------------------------------------------------------------------------------------------!

IF (PRESENT(Debug)) THEN
  DebugMode = Debug
ELSE
  DebugMode = .FALSE.
END IF

nElems = MeshArraysInfo%nElems

IF (ALLOCATED(MeshData_ElementsToElementType) .EQV. .TRUE.) THEN
  DEALLOCATE(MeshData_ElementsToElementType)
END IF
ALLOCATE(MeshData_ElementsToElementType(1:nElems))

aElem => ElemList%FirstElem
DO WHILE (ASSOCIATED(aElem))
  ElemID   = aElem%ElemID
  ElemType = aElem%ElemType
  MeshData_ElementsToElementType(ElemID) = ElemType
  aElem => aElem%NextElem
END DO

IF (DebugMode .EQV. .TRUE.) THEN
  WRITE(UNIT_SCREEN,*)
  WRITE(UNIT_SCREEN,"(2X,A)") "======================================================="
  WRITE(UNIT_SCREEN,"(2X,A)") "Printing Array: Elements-to-Type"
  WRITE(UNIT_SCREEN,"(2X,A)") "======================================================="
  WRITE(UNIT_SCREEN,"(2(2X,A8))") "ElemID", "ElemType"
  FormatString = "(2(2X,I8))"
  DO ElemID=1,nElems
    WRITE(UNIT_SCREEN,FormatString) ElemID, MeshData_ElementsToElementType(ElemID)
  END DO
  WRITE(UNIT_SCREEN,*)
END IF

!----------------------------------------------------------------------------------------------------------------------!
END SUBROUTINE CreateArray_ElementsType
!======================================================================================================================!
!
!
!
!======================================================================================================================!
SUBROUTINE CreateArray_NodesCoordinates(Debug)
!----------------------------------------------------------------------------------------------------------------------!
USE MOD_MeshMain_vars,ONLY: tElem
USE MOD_MeshMain_vars,ONLY: ElemList
!----------------------------------------------------------------------------------------------------------------------!
USE MOD_MeshMain_vars,ONLY: MeshArraysInfo
USE MOD_MeshMain_vars,ONLY: MeshData_NodesCoordinates
!----------------------------------------------------------------------------------------------------------------------!
IMPLICIT NONE
!----------------------------------------------------------------------------------------------------------------------!
! FORMAL ARGUMENTS
!----------------------------------------------------------------------------------------------------------------------!
LOGICAL,INTENT(IN),OPTIONAL :: Debug
!----------------------------------------------------------------------------------------------------------------------!
! LOCAL VARIABLES
!----------------------------------------------------------------------------------------------------------------------!
INTEGER :: iNode
INTEGER :: nNodes
LOGICAL :: DebugMode
!----------------------------------------------------------------------------------------------------------------------!
TYPE(tElem),POINTER :: aElem
!----------------------------------------------------------------------------------------------------------------------!
CHARACTER(LEN=256) :: FormatString
!----------------------------------------------------------------------------------------------------------------------!

IF (PRESENT(Debug)) THEN
  DebugMode = Debug
ELSE
  DebugMode = .FALSE.
END IF

nNodes = MeshArraysInfo%nNodes

IF (ALLOCATED(MeshData_NodesCoordinates)) THEN
  DEALLOCATE(MeshData_NodesCoordinates)
END IF
ALLOCATE(MeshData_NodesCoordinates(1:3,1:nNodes))

aElem => ElemList%FirstElem
DO WHILE (ASSOCIATED(aElem))
  DO iNode=1,aElem%nNodes
    MeshData_NodesCoordinates(1:3,aElem%Nodes(iNode)%Node%NodeID) = aElem%Nodes(iNode)%Node%Coords(1:3)
  END DO
  aElem => aElem%NextElem
END DO

IF (DebugMode .EQV. .TRUE.) THEN
  WRITE(UNIT_SCREEN,*)
  WRITE(UNIT_SCREEN,"(2X,A)") "======================================================="
  WRITE(UNIT_SCREEN,"(2X,A)") "Printing Array: Nodes-Coordinates"
  WRITE(UNIT_SCREEN,"(2X,A)") "======================================================="
  WRITE(UNIT_SCREEN,"(1(2X,A8),3(2X,A13))") "iNode", "CoordinateX", "CoordinateY", "CoordinateZ"
  FormatString = "(1(2X,I8),3(2X,SP,ES13.6E2))"
  DO iNode=1,nNodes
    WRITE(UNIT_SCREEN,FormatString) iNode, MeshData_NodesCoordinates(1:3,iNode)
  END DO
  WRITE(UNIT_SCREEN,*)
END IF

!----------------------------------------------------------------------------------------------------------------------!
END SUBROUTINE CreateArray_NodesCoordinates
!======================================================================================================================!
!
!
!
!======================================================================================================================!
SUBROUTINE CreateArray_ElementsToNodesCoordinates(Debug)
!----------------------------------------------------------------------------------------------------------------------!
USE MOD_MeshMain_vars,ONLY: tElem
USE MOD_MeshMain_vars,ONLY: ElemList
!----------------------------------------------------------------------------------------------------------------------!
USE MOD_MeshMain_vars,ONLY: MeshArraysInfo
USE MOD_MeshMain_vars,ONLY: MeshData_ElementsToElementType
USE MOD_MeshMain_vars,ONLY: MeshData_ElementsToNodesCoordinates
!----------------------------------------------------------------------------------------------------------------------!
USE MOD_Mesh_CGNS_Definitions,ONLY: GetnElemNodes
!----------------------------------------------------------------------------------------------------------------------!
IMPLICIT NONE
!----------------------------------------------------------------------------------------------------------------------!
! FORMAL ARGUMENTS
!----------------------------------------------------------------------------------------------------------------------!
LOGICAL,INTENT(IN),OPTIONAL :: Debug
!----------------------------------------------------------------------------------------------------------------------!
! LOCAL VARIABLES
!----------------------------------------------------------------------------------------------------------------------!
INTEGER :: iElem
INTEGER :: iNode
INTEGER :: ElemID
INTEGER :: nElems
INTEGER :: ElemType
INTEGER :: nElemNodes
INTEGER :: nMaxElemNodes
LOGICAL :: DebugMode
!----------------------------------------------------------------------------------------------------------------------!
TYPE(tElem),POINTER :: aElem
!----------------------------------------------------------------------------------------------------------------------!
CHARACTER(LEN=256) :: FormatString
!----------------------------------------------------------------------------------------------------------------------!

IF (PRESENT(Debug)) THEN
  DebugMode = Debug
ELSE
  DebugMode = .FALSE.
END IF

nElems = MeshArraysInfo%nElems

! Exporting ElementsToNodesList to array
! For PP_nDims=2, MAX(nElemNodes)=MAX({3,4})=4
! For PP_nDims=3, MAX(nElemNodes)=MAX({4,5,6,8})=8
SELECT CASE(PP_nDims)
  CASE(2)
    nMaxElemNodes = 4
  CASE(3)
    nMaxElemNodes = 8
END SELECT

IF (ALLOCATED(MeshData_ElementsToNodesCoordinates) .EQV. .TRUE.) THEN
  DEALLOCATE(MeshData_ElementsToNodesCoordinates)
END IF
ALLOCATE(MeshData_ElementsToNodesCoordinates(1:PP_nDims,1:nMaxElemNodes,1:nElems))
MeshData_ElementsToNodesCoordinates = 0.0

aElem => ElemList%FirstElem
DO WHILE (ASSOCIATED(aElem))
  ElemID = aElem%ElemID
  DO iNode=1,aElem%nNodes
    MeshData_ElementsToNodesCoordinates(1:PP_nDims,iNode,ElemID) = aElem%Nodes(iNode)%Node%Coords(1:PP_nDims)
  END DO
  aElem => aElem%NextElem
END DO

IF (DebugMode .EQV. .TRUE.) THEN
  WRITE(UNIT_SCREEN,*)
  WRITE(UNIT_SCREEN,"(2X,A)") "======================================================="
  WRITE(UNIT_SCREEN,"(2X,A)") "Printing Array: Elements-to-Nodes-Coordinates"
  WRITE(UNIT_SCREEN,"(2X,A)") "======================================================="
  SELECT CASE(PP_nDims)
    CASE(2)
      WRITE(UNIT_SCREEN,"(2(2X,A8),2(2X,A13))") "ElemID", "iNode", "CoordinateX", "CoordinateY"
    CASE(3)
      WRITE(UNIT_SCREEN,"(2(2X,A8),3(2X,A13))") "ElemID", "iNode", "CoordinateX", "CoordinateY", "CoordinateZ"
  END SELECT
  WRITE(FormatString,"(A,I0,A)") "(2(2X,I8),", PP_nDims, "(2X,SP,ES13.6E2))"
  DO iElem=1,nElems
    ElemType = MeshData_ElementsToElementType(iElem)
    CALL GetnElemNodes(ElemType,nElemNodes)
    DO iNode=1,nElemNodes
      WRITE(UNIT_SCREEN,FormatString) iElem, iNode, MeshData_ElementsToNodesCoordinates(1:PP_nDims,iNode,iElem)
    END DO
    WRITE(UNIT_SCREEN,*)
  END DO
  WRITE(UNIT_SCREEN,*)
END IF

!----------------------------------------------------------------------------------------------------------------------!
END SUBROUTINE CreateArray_ElementsToNodesCoordinates
!======================================================================================================================!
!
!
!
!======================================================================================================================!
SUBROUTINE CreateArray_ElementsToNodes(Debug)
!----------------------------------------------------------------------------------------------------------------------!
USE MOD_MeshMain_vars,ONLY: tElem
USE MOD_MeshMain_vars,ONLY: ElemList
!----------------------------------------------------------------------------------------------------------------------!
USE MOD_MeshMain_vars,ONLY: MeshArraysInfo
USE MOD_MeshMain_vars,ONLY: MeshData_ElementsToElementType
USE MOD_MeshMain_vars,ONLY: MeshData_ElementsToNodes
!----------------------------------------------------------------------------------------------------------------------!
USE MOD_Mesh_CGNS_Definitions
!----------------------------------------------------------------------------------------------------------------------!
IMPLICIT NONE
!----------------------------------------------------------------------------------------------------------------------!
! FORMAL ARGUMENTS
!----------------------------------------------------------------------------------------------------------------------!
LOGICAL,INTENT(IN),OPTIONAL :: Debug
!----------------------------------------------------------------------------------------------------------------------!
! LOCAL VARIABLES
!----------------------------------------------------------------------------------------------------------------------!
INTEGER :: iNode
INTEGER :: nElems
INTEGER :: ElemID
INTEGER :: ElemType
INTEGER :: nElemNodes
INTEGER :: nMaxElemNodes
LOGICAL :: DebugMode
!----------------------------------------------------------------------------------------------------------------------!
TYPE(tElem),POINTER :: aElem
!----------------------------------------------------------------------------------------------------------------------!
CHARACTER(LEN=256) :: FormatString
!----------------------------------------------------------------------------------------------------------------------!

IF (PRESENT(Debug)) THEN
  DebugMode = Debug
ELSE
  DebugMode = .FALSE.
END IF

nElems = MeshArraysInfo%nElems

! Exporting ElementsToNodesList to array
! For PP_nDims=2, MAX(nElemNodes)=MAX({3,4})=4
! For PP_nDims=3, MAX(nElemNodes)=MAX({4,5,6,8})=8

SELECT CASE(PP_nDims)
  CASE(2)
    nMaxElemNodes = 4
  CASE(3)
    nMaxElemNodes = 8
END SELECT

IF (ALLOCATED(MeshData_ElementsToNodes)) THEN
  DEALLOCATE(MeshData_ElementsToNodes)
END IF
ALLOCATE(MeshData_ElementsToNodes(1:nMaxElemNodes,1:nElems))
MeshData_ElementsToNodes = -1

aElem => ElemList%FirstElem
DO WHILE (ASSOCIATED(aElem))
  ElemID   = aElem%ElemID
  DO iNode=1,aElem%nNodes
    MeshData_ElementsToNodes(iNode,ElemID) = aElem%Nodes(iNode)%Node%NodeID
  END DO
  aElem => aElem%NextElem
END DO

IF (DebugMode .EQV. .TRUE.) THEN
  WRITE(UNIT_SCREEN,*)
  WRITE(UNIT_SCREEN,"(2X,A)") "======================================================="
  WRITE(UNIT_SCREEN,"(2X,A)") "Printing Array: Elements-to-Nodes"
  WRITE(UNIT_SCREEN,"(2X,A)") "======================================================="
  WRITE(UNIT_SCREEN,"(2(2X,A8),2X,A)") "ElemID", "ElemType", "NodeIDs"
  DO ElemID=1,nElems
    ElemType = MeshData_ElementsToElementType(ElemID)
    CALL GetnElemNodes(ElemType,nElemNodes)
    WRITE(FormatString,"(A,I0,A)") "(2(2X,I8),1X,", nElemNodes, "(1X,I0))"
    WRITE(UNIT_SCREEN,FormatString) ElemID, ElemType, MeshData_ElementsToNodes(1:nElemNodes,ElemID)
  END DO
  WRITE(UNIT_SCREEN,*)
END IF

!----------------------------------------------------------------------------------------------------------------------!
END SUBROUTINE CreateArray_ElementsToNodes
!======================================================================================================================!
!
!
!
!======================================================================================================================!
SUBROUTINE CreateArray_ElementsToLevel(Debug)
!----------------------------------------------------------------------------------------------------------------------!
USE MOD_MeshMain_vars,ONLY: tElem
USE MOD_MeshMain_vars,ONLY: ElemList
!----------------------------------------------------------------------------------------------------------------------!
USE MOD_MeshMain_vars,ONLY: MeshArraysInfo
USE MOD_MeshMain_vars,ONLY: MeshData_ElementsToLevel
USE MOD_MeshMain_vars,ONLY: MeshData_ElementsToFlag
!----------------------------------------------------------------------------------------------------------------------!
IMPLICIT NONE
!----------------------------------------------------------------------------------------------------------------------!
LOGICAL,INTENT(IN),OPTIONAL :: Debug
!----------------------------------------------------------------------------------------------------------------------!
! LOCAL VARIABLES
!----------------------------------------------------------------------------------------------------------------------!
INTEGER :: ElemID
INTEGER :: nElems
LOGICAL :: DebugMode
!----------------------------------------------------------------------------------------------------------------------!
TYPE(tElem),POINTER :: aElem
!----------------------------------------------------------------------------------------------------------------------!
CHARACTER(LEN=256) :: FormatString
!----------------------------------------------------------------------------------------------------------------------!

IF (PRESENT(Debug)) THEN
  DebugMode = Debug
ELSE
  DebugMode = .FALSE.
END IF

nElems = MeshArraysInfo%nElems

IF (ALLOCATED(MeshData_ElementsToLevel) .EQV. .TRUE.) THEN
  DEALLOCATE(MeshData_ElementsToLevel)
END IF
ALLOCATE(MeshData_ElementsToLevel(1:nElems))

IF (ALLOCATED(MeshData_ElementsToFlag) .EQV. .TRUE.) THEN
  DEALLOCATE(MeshData_ElementsToFlag)
END IF
ALLOCATE(MeshData_ElementsToFlag(1:nElems))

aElem => ElemList%FirstElem
DO WHILE (ASSOCIATED(aElem))
  ElemID = aElem%ElemID
  MeshData_ElementsToLevel(aElem%ElemID) = aElem%Level
  MeshData_ElementsToFlag(aElem%ElemID)  = aElem%Flag
  aElem => aElem%NextElem
END DO

IF (DebugMode .EQV. .TRUE.) THEN
  WRITE(UNIT_SCREEN,*)
  WRITE(UNIT_SCREEN,"(2X,A)") "======================================================="
  WRITE(UNIT_SCREEN,"(2X,A)") "Printing Array: Elements-to-Level"
  WRITE(UNIT_SCREEN,"(2X,A)") "======================================================="
  WRITE(UNIT_SCREEN,"(2(2X,A8))") "ElemID", "Level"
  FormatString = "(1(2X,I8),1(2X,I8))"
  DO ElemID=1,nElems
    WRITE(UNIT_SCREEN,FormatString) ElemID, MeshData_ElementsToLevel(ElemID)
  END DO
  WRITE(UNIT_SCREEN,*)
END IF

!----------------------------------------------------------------------------------------------------------------------!
END SUBROUTINE CreateArray_ElementsToLevel
!======================================================================================================================!
!
!
!
!======================================================================================================================!
SUBROUTINE CreateArray_BCFacesToNodes(Debug)
!----------------------------------------------------------------------------------------------------------------------!
USE MOD_MeshMain_vars,ONLY: tElem
USE MOD_MeshMain_vars,ONLY: tSide
USE MOD_MeshMain_vars,ONLY: ElemList
!----------------------------------------------------------------------------------------------------------------------!
USE MOD_MeshMain_vars,ONLY: MeshArraysInfo
USE MOD_MeshMain_vars,ONLY: MeshData_BCFacesToMark
USE MOD_MeshMain_vars,ONLY: MeshData_BCFacesToLevel
USE MOD_MeshMain_vars,ONLY: MeshData_BCFacesToNodes
USE MOD_MeshMain_vars,ONLY: MeshData_BCFacesToElementType
!----------------------------------------------------------------------------------------------------------------------!
USE MOD_Mesh_CGNS_Definitions
!----------------------------------------------------------------------------------------------------------------------!
IMPLICIT NONE
!----------------------------------------------------------------------------------------------------------------------!
! FORMAL ARGUMENTS
!----------------------------------------------------------------------------------------------------------------------!
LOGICAL,INTENT(IN),OPTIONAL :: Debug
!----------------------------------------------------------------------------------------------------------------------!
! LOCAL VARIABLES
!----------------------------------------------------------------------------------------------------------------------!
INTEGER :: iNode
INTEGER :: iBCFace
INTEGER :: BCFaceID
INTEGER :: nBCFaces
INTEGER :: ElemType
INTEGER :: nBCFacesNodes
INTEGER :: nMaxBCFacesNodes
LOGICAL :: DebugMode
!----------------------------------------------------------------------------------------------------------------------!
INTEGER :: BCFaceOrientation_EDGE(1:2) = (/2,1/)
INTEGER :: BCFaceOrientation_TRI(1:3)  = (/1,3,2/)
INTEGER :: BCFaceOrientation_QUAD(1:4) = (/1,4,3,2/)
!----------------------------------------------------------------------------------------------------------------------!
TYPE(tElem),POINTER :: aElem
TYPE(tSide),POINTER :: aSide
!----------------------------------------------------------------------------------------------------------------------!
CHARACTER(LEN=256) :: FormatString
!----------------------------------------------------------------------------------------------------------------------!

IF (PRESENT(Debug)) THEN
  DebugMode = Debug
ELSE
  DebugMode = .FALSE.
END IF

! ! ! ! WARNING
! ! ! DebugMode = .TRUE.
! ! ! ! WARNING

nBCFaces = MeshArraysInfo%nBCFaces

! Exporting BCFacesToNodesList to array
! For PP_nDims=2, MAX(nBCFacesNodes)=MAX({2})=2
! For PP_nDims=3, MAX(nBCFacesNodes)=MAX({3,4})=4
SELECT CASE(PP_nDims)
  CASE(2)
    nMaxBCFacesNodes = 2
  CASE(3)
    nMaxBCFacesNodes = 4
END SELECT

IF (ALLOCATED(MeshData_BCFacesToNodes)) THEN
  DEALLOCATE(MeshData_BCFacesToNodes)
END IF
ALLOCATE(MeshData_BCFacesToNodes(1:nMaxBCFacesNodes,1:nBCFaces))
MeshData_BCFacesToNodes = -1

IF (ALLOCATED(MeshData_BCFacesToLevel)) THEN
  DEALLOCATE(MeshData_BCFacesToLevel)
END IF
ALLOCATE(MeshData_BCFacesToLevel(1:nBCFaces))

IF (ALLOCATED(MeshData_BCFacesToMark)) THEN
  DEALLOCATE(MeshData_BCFacesToMark)
END IF
ALLOCATE(MeshData_BCFacesToMark(1:nBCFaces))

IF (ALLOCATED(MeshData_BCFacesToElementType)) THEN
  DEALLOCATE(MeshData_BCFacesToElementType)
END IF
ALLOCATE(MeshData_BCFacesToElementType(1:nBCFaces))

BCFaceID = 0
aElem => ElemList%FirstElem
DO WHILE (ASSOCIATED(aElem))
  aSide => aElem%FirstSide
  DO WHILE(ASSOCIATED(aSide))
    IF (ASSOCIATED(aSide%BC) .EQV. .TRUE.) THEN
      nBCFacesNodes = aSide%nNodes
      BCFaceID = BCFaceID+1
      DO iNode=1,nBCFacesNodes
        MeshData_BCFacesToNodes(iNode,BCFaceID) = aSide%Nodes(iNode)%Node%NodeID
      END DO
      ! Set nodes ordering of boundary faces such that
      ! surface normal vector points inward the domain
      SELECT CASE(PP_nDims)
        CASE(2)
          SELECT CASE(nBCFacesNodes)
            CASE(2) ! ELEMTYPE_EDGE2
              MeshData_BCFacesToNodes(1:nBCFacesNodes,BCFaceID) = &
                MeshData_BCFacesToNodes(BCFaceOrientation_EDGE(1:nBCFacesNodes),BCFaceID)
          END SELECT
        CASE(3)
          SELECT CASE(nBCFacesNodes)
            CASE(3) ! ELEMTYPE_TRI3
              MeshData_BCFacesToNodes(1:nBCFacesNodes,BCFaceID) = &
                MeshData_BCFacesToNodes(BCFaceOrientation_TRI(1:nBCFacesNodes),BCFaceID)
            CASE(4) ! ELEMTYPE_QUAD4
              MeshData_BCFacesToNodes(1:nBCFacesNodes,BCFaceID) = &
                MeshData_BCFacesToNodes(BCFaceOrientation_QUAD(1:nBCFacesNodes),BCFaceID)
          END SELECT
      END SELECT
      SELECT CASE(PP_nDims)
        CASE(2)
          MeshData_BCFacesToElementType(BCFaceID) = 1
        CASE(3)
          SELECT CASE(nBCFacesNodes)
            CASE(3)
              MeshData_BCFacesToElementType(BCFaceID) = 2
            CASE(4)
              MeshData_BCFacesToElementType(BCFaceID) = 3
          END SELECT
      END SELECT
      MeshData_BCFacesToMark(BCFaceID)  = aSide%BC%BCMark
      MeshData_BCFacesToLevel(BCFaceID) = aElem%Level
    END IF
    aSide => aSide%NextElemSide
  END DO
  aElem => aElem%NextElem
END DO

IF (DebugMode .EQV. .TRUE.) THEN
  WRITE(UNIT_SCREEN,*)
  WRITE(UNIT_SCREEN,"(2X,A)") "======================================================="
  WRITE(UNIT_SCREEN,"(2X,A)") "Printing Array: BCFaces-to-Nodes"
  WRITE(UNIT_SCREEN,"(2X,A)") "======================================================="
  WRITE(UNIT_SCREEN,"(4(2X,A8),2X,A)") "iBCFace", "ElemType", "BCLevel", "BCMark", "NodeIDs"
  DO iBCFace=1,nBCFaces
    ElemType = MeshData_BCFacesToElementType(iBCFace)
    CALL GetnElemNodes(ElemType,nBCFacesNodes)
    WRITE(FormatString,"(A,I0,A)") "(4(2X,I8),1X,", nBCFacesNodes, "(1X,I0))"
    WRITE(UNIT_SCREEN,FormatString) &
      iBCFace, &
      MeshData_BCFacesToElementType(iBCFace), &
      MeshData_BCFacesToLevel(iBCFace), &
      MeshData_BCFacesToMark(iBCFace), &
      MeshData_BCFacesToNodes(1:nBCFacesNodes,iBCFace)
  END DO
  WRITE(UNIT_SCREEN,*)
END IF

!----------------------------------------------------------------------------------------------------------------------!
END SUBROUTINE CreateArray_BCFacesToNodes
!======================================================================================================================!
!
!
!
!======================================================================================================================!
SUBROUTINE CreateArray_FacesToNodes(Debug)
!----------------------------------------------------------------------------------------------------------------------!
USE MOD_MeshMain_vars,ONLY: tElem
USE MOD_MeshMain_vars,ONLY: tSide
USE MOD_MeshMain_vars,ONLY: tFaceNodes
USE MOD_MeshMain_vars,ONLY: ElemList
USE MOD_MeshMain_vars,ONLY: tFaceNodesList
!----------------------------------------------------------------------------------------------------------------------!
USE MOD_MeshMain_vars,ONLY: MeshInfo
USE MOD_MeshMain_vars,ONLY: MeshArraysInfo
USE MOD_MeshMain_vars,ONLY: MeshData_ElementsToElementType
USE MOD_MeshMain_vars,ONLY: MeshData_FacesElementType
USE MOD_MeshMain_vars,ONLY: MeshData_FacesToNodes
!----------------------------------------------------------------------------------------------------------------------!
USE MOD_MeshMainMethods,ONLY: CreateFaceNodes
USE MOD_MeshMainMethods,ONLY: GetFaceNodesData
USE MOD_MeshMainMethods,ONLY: AddFaceNodesToList
USE MOD_MeshMainMethods,ONLY: DestructFaceNodesList
!----------------------------------------------------------------------------------------------------------------------!
USE MOD_Mesh_CGNS_Definitions
!----------------------------------------------------------------------------------------------------------------------!
USE MOD_DataStructures,ONLY: QuickSort
USE MOD_DataStructures,ONLY: QuickSortArray
!----------------------------------------------------------------------------------------------------------------------!
IMPLICIT NONE
!----------------------------------------------------------------------------------------------------------------------!
! FORMAL ARGUMENTS
!----------------------------------------------------------------------------------------------------------------------!
LOGICAL,INTENT(IN),OPTIONAL :: Debug
!----------------------------------------------------------------------------------------------------------------------!
! LOCAL VARIABLES
!----------------------------------------------------------------------------------------------------------------------!
INTEGER :: iNode
INTEGER :: iFace
INTEGER :: iElem
INTEGER :: FaceID
INTEGER :: nFaces
INTEGER :: nElems
INTEGER :: ElemType
INTEGER :: FaceType
INTEGER :: nElemFaces
INTEGER :: nFacesTotal
INTEGER :: nFaceNodes
INTEGER :: nNodesOnFace
INTEGER :: nMaxFaceNodes
INTEGER :: nMaxElemSides
!----------------------------------------------------------------------------------------------------------------------!
INTEGER :: nElems_EDGE
INTEGER :: nElems_TRI
INTEGER :: nElems_QUAD
INTEGER :: nElems_TETRA
INTEGER :: nElems_HEXA
INTEGER :: nElems_PRISM
INTEGER :: nElems_PYRA
LOGICAL :: DebugMode
!----------------------------------------------------------------------------------------------------------------------!
INTEGER,ALLOCATABLE :: FaceNodes(:)
!----------------------------------------------------------------------------------------------------------------------!
TYPE(tElem),POINTER :: aElem
TYPE(tSide),POINTER :: aSide
!----------------------------------------------------------------------------------------------------------------------!
TYPE(tFaceNodes),POINTER :: aFaceNodes
TYPE(tFaceNodesList)     :: FaceNodesList
!----------------------------------------------------------------------------------------------------------------------!
INTEGER,ALLOCATABLE :: FacesToNodesArray(:,:)
!----------------------------------------------------------------------------------------------------------------------!
CHARACTER(LEN=256) :: FormatString
!----------------------------------------------------------------------------------------------------------------------!

IF (PRESENT(Debug)) THEN
  DebugMode = Debug
ELSE
  DebugMode = .FALSE.
END IF

nElems = MeshArraysInfo%nElems

! For PP_nDims=2, MAX(nMaxFaceNodes)=MAX({2})=2
! For PP_nDims=2, MAX(nMaxElemSides)=MAX({3,4})=4
! For PP_nDims=3, MAX(nMaxFaceNodes)=MAX({3,4})=4
! For PP_nDims=3, MAX(nMaxElemSides)=MAX({4,5,6})=6
SELECT CASE(PP_nDims)
  CASE(2)
    nMaxFaceNodes = 2
    nMaxElemSides = 4
  CASE(3)
    nMaxFaceNodes = 4
    nMaxElemSides = 6
END SELECT

nElems_EDGE  = 0
nElems_TRI   = 0
nElems_QUAD  = 0
nElems_TETRA = 0
nElems_HEXA  = 0
nElems_PRISM = 0
nElems_PYRA  = 0

DO iElem=1,SIZE(MeshData_ElementsToElementType,1)
  ElemType = MeshData_ElementsToElementType(iElem)
  SELECT CASE(ElemType)
    CASE(ELEMTYPE_EDGE2)
      nElems_EDGE = nElems_EDGE+1
    CASE(ELEMTYPE_TRI3)
      nElems_TRI = nElems_TRI+1
    CASE(ELEMTYPE_QUAD4)
      nElems_QUAD = nElems_QUAD+1
    CASE(ELEMTYPE_TETRA4)
      nElems_TETRA = nElems_TETRA+1
    CASE(ELEMTYPE_HEXA8)
      nElems_HEXA = nElems_HEXA+1
    CASE(ELEMTYPE_PRISM6)
      nElems_PRISM = nElems_PRISM+1
    CASE(ELEMTYPE_PYRA5)
      nElems_PYRA = nElems_PYRA+1
  END SELECT
END DO

nFacesTotal = 0

CALL GetnElemFaces(ELEMTYPE_EDGE2,nElemFaces)
nFacesTotal = nFacesTotal + nElemFaces*nElems_EDGE

CALL GetnElemFaces(ELEMTYPE_TRI3,nElemFaces)
nFacesTotal = nFacesTotal + nElemFaces*nElems_TRI

CALL GetnElemFaces(ELEMTYPE_QUAD4,nElemFaces)
nFacesTotal = nFacesTotal + nElemFaces*nElems_QUAD

CALL GetnElemFaces(ELEMTYPE_TETRA4,nElemFaces)
nFacesTotal = nFacesTotal + nElemFaces*nElems_TETRA

CALL GetnElemFaces(ELEMTYPE_HEXA8,nElemFaces)
nFacesTotal = nFacesTotal + nElemFaces*nElems_HEXA

CALL GetnElemFaces(ELEMTYPE_PRISM6,nElemFaces)
nFacesTotal = nFacesTotal + nElemFaces*nElems_PRISM

CALL GetnElemFaces(ELEMTYPE_PYRA5,nElemFaces)
nFacesTotal = nFacesTotal + nElemFaces*nElems_PYRA

IF (ALLOCATED(FacesToNodesArray) .EQV. .TRUE.) THEN
  DEALLOCATE(FacesToNodesArray)
END IF
ALLOCATE(FacesToNodesArray(1:nFacesTotal,1:2*nMaxFaceNodes+1))

IF (ALLOCATED(FaceNodes)) THEN
  DEALLOCATE(FaceNodes)
END IF
ALLOCATE(FaceNodes(1:nMaxFaceNodes))

FaceID = 0
aElem => ElemList%FirstElem
DO WHILE (ASSOCIATED(aElem))
  aSide => aElem%FirstSide
  DO WHILE (ASSOCIATED(aSide))
    FaceID = FaceID+1
    FaceNodes(1:nMaxFaceNodes) = 0
    DO iNode=1,aSide%nNodes
      FaceNodes(iNode) = aSide%Nodes(iNode)%Node%NodeID
    END DO
    CALL GetnNodesOnFace(aElem%ElemType,aSide%LocSide,nNodesOnFace)
    FacesToNodesArray(FaceID,:) = 0    
    FacesToNodesArray(FaceID,nMaxFaceNodes+1) = aElem%ElemID
    FacesToNodesArray(FaceID,nMaxFaceNodes+2:2*nMaxFaceNodes+1) = FaceNodes(1:nMaxFaceNodes)
    CALL QuickSort(FaceNodes(1:nNodesOnFace))
    FacesToNodesArray(FaceID,1:nNodesOnFace) = FaceNodes(1:nNodesOnFace)
    aSide => aSide%NextElemSide
  END DO
  aElem => aElem%NextElem
END DO

CALL QuickSortArray(FacesToNodesArray,nMaxFaceNodes+1)

iFace  = 1
FaceID = 0
DO WHILE (iFace .LE. nFacesTotal)
  IF (iFace .GT. 1) THEN
    IF (ALL(FacesToNodesArray(iFace,1:nMaxFaceNodes) .EQ. FacesToNodesArray(iFace-1,1:nMaxFaceNodes))) THEN
      iFace = iFace+1
      CYCLE
    ELSE
      FaceID = FaceID+1
    END IF
  ELSE
    FaceID = FaceID+1
  END IF
  ! Store FaceID
  FaceNodes(1:nMaxFaceNodes) = FacesToNodesArray(iFace,nMaxFaceNodes+2:2*nMaxFaceNodes+1)
  CALL CreateFaceNodes(aFaceNodes,FaceID,nMaxFaceNodes,FaceNodes)
  CALL AddFaceNodesToList(aFaceNodes,FaceNodesList)
  iFace = iFace+1
END DO
nFaces = FaceID

MeshInfo%nFaces       = nFaces
MeshArraysInfo%nFaces = nFaces

! Faces-to-Nodes Array
IF (ALLOCATED(FaceNodes)) THEN
  DEALLOCATE(FaceNodes)
END IF
ALLOCATE(FaceNodes(1:nMaxFaceNodes))

IF (ALLOCATED(MeshData_FacesElementType)) THEN
  DEALLOCATE(MeshData_FacesElementType)
END IF
ALLOCATE(MeshData_FacesElementType(1:nFaces))

IF (ALLOCATED(MeshData_FacesToNodes)) THEN
  DEALLOCATE(MeshData_FacesToNodes)
END IF
ALLOCATE(MeshData_FacesToNodes(1:nMaxFaceNodes,1:nFaces))

aFaceNodes => FaceNodesList%FirstFaceNodes
DO WHILE(ASSOCIATED(aFaceNodes))
  CALL GetFaceNodesData(aFaceNodes,FaceID,FaceNodes)
  MeshData_FacesToNodes(1:nMaxFaceNodes,FaceID) = FaceNodes(1:nMaxFaceNodes)
  nFaceNodes = COUNT(FaceNodes .NE. 0)
  SELECT CASE(nFaceNodes)
    CASE(2) ! EDGE2
      FaceType = ELEMTYPE_EDGE2
    CASE(3) ! TRI3
      FaceType = ELEMTYPE_TRI3
    CASE(4) ! QUAD4
      FaceType = ELEMTYPE_QUAD4
  END SELECT
  MeshData_FacesElementType(FaceID) = FaceType
  aFaceNodes => aFaceNodes%NextFaceNodes
END DO

IF (DebugMode .EQV. .TRUE.) THEN
  WRITE(UNIT_SCREEN,*)
  WRITE(UNIT_SCREEN,"(2X,A)") "======================================================="
  WRITE(UNIT_SCREEN,"(2X,A)") "Printing Array: Faces-to-Nodes"
  WRITE(UNIT_SCREEN,"(2X,A)") "======================================================="
  WRITE(UNIT_SCREEN,"(2(2X,A8),2X,A)") "iFace", "FaceType", "NodeIDs"
  DO iFace=1,nFaces
    FaceType = MeshData_FacesElementType(iFace)
    CALL GetnElemNodes(FaceType,nFaceNodes)
    WRITE(FormatString,"(A,I0,A)") "(2(2X,I8),1X,", nFaceNodes, "(1X,I0))"
    WRITE(UNIT_SCREEN,FormatString) iFace, FaceType, MeshData_FacesToNodes(1:nFaceNodes,iFace)
  END DO
  WRITE(UNIT_SCREEN,*)
END IF

DEALLOCATE(FacesToNodesArray)
CALL DestructFaceNodesList(FaceNodesList)

!----------------------------------------------------------------------------------------------------------------------!
END SUBROUTINE CreateArray_FacesToNodes
!======================================================================================================================!
!
!
!
!======================================================================================================================!
SUBROUTINE CreateArray_ElementsToFaces(Debug)
!----------------------------------------------------------------------------------------------------------------------!
USE MOD_MeshMain_vars,ONLY: tElem
USE MOD_MeshMain_vars,ONLY: tSide
USE MOD_MeshMain_vars,ONLY: ElemList
!----------------------------------------------------------------------------------------------------------------------!
USE MOD_MeshMain_vars,ONLY: MeshArraysInfo
USE MOD_MeshMain_vars,ONLY: MeshData_FacesToNodes
USE MOD_MeshMain_vars,ONLY: MeshData_FacesElementType
USE MOD_MeshMain_vars,ONLY: MeshData_ElementsToElementType
USE MOD_MeshMain_vars,ONLY: MeshData_ElementsToFaces
!----------------------------------------------------------------------------------------------------------------------!
USE MOD_Mesh_CGNS_Definitions
!----------------------------------------------------------------------------------------------------------------------!
USE MOD_DataStructures,ONLY: QuickSort
USE MOD_DataStructures,ONLY: QuickSortArray
!----------------------------------------------------------------------------------------------------------------------!
IMPLICIT NONE
!----------------------------------------------------------------------------------------------------------------------!
! FORMAL ARGUMENTS
!----------------------------------------------------------------------------------------------------------------------!
LOGICAL,INTENT(IN),OPTIONAL :: Debug
!----------------------------------------------------------------------------------------------------------------------!
! LOCAL VARIABLES
!----------------------------------------------------------------------------------------------------------------------!
INTEGER :: iElem
INTEGER :: iFace
INTEGER :: nElems
INTEGER :: FaceID
INTEGER :: nFaceNodes
INTEGER :: nElemFaces
INTEGER :: FaceType
INTEGER :: ElemType
INTEGER :: nMaxElemSides
!----------------------------------------------------------------------------------------------------------------------!
LOGICAL :: DebugMode
!----------------------------------------------------------------------------------------------------------------------!
TYPE(tElem),POINTER :: aElem
TYPE(tSide),POINTER :: aSide
!----------------------------------------------------------------------------------------------------------------------!
CHARACTER(LEN=256) :: FormatString
!----------------------------------------------------------------------------------------------------------------------!

IF (PRESENT(Debug)) THEN
  DebugMode = Debug
ELSE
  DebugMode = .FALSE.
END IF

!**************************************************!
! WARNING WARNING WARNING WARNING WARNING WARNING
!**************************************************!
! Make sure SetUniqueSideID have been called before
! All SideID have been set for each element
!**************************************************!

! For PP_nDims=2, MAX(nMaxElemSides)=MAX({3,4})=4
! For PP_nDims=3, MAX(nMaxElemSides)=MAX({4,5,6})=6
SELECT CASE(PP_nDims)
  CASE(2)
    nMaxElemSides = 4
  CASE(3)
    nMaxElemSides = 6
END SELECT

nElems = MeshArraysInfo%nElems

! Elements-to-Faces Array
IF (ALLOCATED(MeshData_ElementsToFaces)) THEN
  DEALLOCATE(MeshData_ElementsToFaces)
END IF
ALLOCATE(MeshData_ElementsToFaces(1:nMaxElemSides,1:nElems))
MeshData_ElementsToFaces = -1

aElem => ElemList%FirstElem
DO WHILE (ASSOCIATED(aElem))
  aSide => aElem%FirstSide
  DO WHILE (ASSOCIATED(aSide))
    MeshData_ElementsToFaces(aSide%LocSide,aElem%ElemID) = aSide%SideID
    aSide => aSide%NextElemSide
  END DO
  aElem => aElem%NextElem
END DO

IF (DebugMode .EQV. .TRUE.) THEN
  WRITE(UNIT_SCREEN,*)
  WRITE(UNIT_SCREEN,"(2X,A)") "======================================================="
  WRITE(UNIT_SCREEN,"(2X,A)") "Printing Array: Elements-to-Faces"
  WRITE(UNIT_SCREEN,"(2X,A)") "======================================================="
  WRITE(UNIT_SCREEN,"(2(2X,A8),2X,A)") "ElemID", "ElemType", "SideID(:)"
  DO iElem=1,nElems
    ElemType = MeshData_ElementsToElementType(iElem)
    CALL GetnElemFaces(ElemType,nElemFaces)
    WRITE(FormatString,"(A,I0,A)") "(2(2X,I8),1X,", nElemFaces, "(1X,I0))"
    WRITE(UNIT_SCREEN,FormatString) iElem, ElemType, MeshData_ElementsToFaces(1:nElemFaces,iElem)
    DO iFace=1,nElemFaces
      FaceID = MeshData_ElementsToFaces(iFace,iElem)
      FaceType = MeshData_FacesElementType(FaceID)
      CALL GetnElemNodes(FaceType,nFaceNodes)
      WRITE(FormatString,"(A,I0,A)") "(3(2X,I8),1X,", nFaceNodes, "(1X,I0))"
      WRITE(UNIT_SCREEN,FormatString) iFace, FaceType, FaceID, MeshData_FacesToNodes(1:nFaceNodes,FaceID)
    END DO
    WRITE(UNIT_SCREEN,*)
  END DO
  WRITE(UNIT_SCREEN,*)
END IF

!----------------------------------------------------------------------------------------------------------------------!
END SUBROUTINE CreateArray_ElementsToFaces
!======================================================================================================================!
!
!
!
!======================================================================================================================!
SUBROUTINE CreateArray_EdgesToNodes(Debug)
!----------------------------------------------------------------------------------------------------------------------!
USE MOD_MeshMain_vars,ONLY: tElem
USE MOD_MeshMain_vars,ONLY: tEdgeNodes
USE MOD_MeshMain_vars,ONLY: ElemList
USE MOD_MeshMain_vars,ONLY: tEdgeNodesList
!----------------------------------------------------------------------------------------------------------------------!
USE MOD_MeshMain_vars,ONLY: MeshInfo
USE MOD_MeshMain_vars,ONLY: MeshArraysInfo
USE MOD_MeshMain_vars,ONLY: MeshData_ElementsToElementType
USE MOD_MeshMain_vars,ONLY: MeshData_EdgesToNodes
!----------------------------------------------------------------------------------------------------------------------!
USE MOD_MeshMainMethods,ONLY: CreateEdgeNodes
USE MOD_MeshMainMethods,ONLY: GetEdgeNodesData
USE MOD_MeshMainMethods,ONLY: AddEdgeNodesToList
USE MOD_MeshMainMethods,ONLY: DestructEdgeNodesList
!----------------------------------------------------------------------------------------------------------------------!
USE MOD_Mesh_CGNS_Definitions
!----------------------------------------------------------------------------------------------------------------------!
USE MOD_DataStructures,ONLY: QuickSort
USE MOD_DataStructures,ONLY: QuickSortArray
!----------------------------------------------------------------------------------------------------------------------!
IMPLICIT NONE
!----------------------------------------------------------------------------------------------------------------------!
! FORMAL ARGUMENTS
!----------------------------------------------------------------------------------------------------------------------!
LOGICAL,INTENT(IN),OPTIONAL :: Debug
!----------------------------------------------------------------------------------------------------------------------!
! LOCAL VARIABLES
!----------------------------------------------------------------------------------------------------------------------!
!----------------------------------------------------------------------------------------------------------------------!
INTEGER :: iEdge
INTEGER :: iElem
INTEGER :: EdgeID
INTEGER :: nEdges
INTEGER :: nElems
INTEGER :: ElemType
INTEGER :: nEdgeNodes
INTEGER :: nElemEdges
INTEGER :: nEdgesTotal
!----------------------------------------------------------------------------------------------------------------------!
INTEGER :: nElems_EDGE
INTEGER :: nElems_TRI
INTEGER :: nElems_QUAD
INTEGER :: nElems_TETRA
INTEGER :: nElems_HEXA
INTEGER :: nElems_PRISM
INTEGER :: nElems_PYRA
LOGICAL :: DebugMode
!----------------------------------------------------------------------------------------------------------------------!
TYPE(tElem),POINTER :: aElem
!----------------------------------------------------------------------------------------------------------------------!
TYPE(tEdgeNodes),POINTER :: aEdgeNodes
TYPE(tEdgeNodesList)     :: EdgeNodesList
!----------------------------------------------------------------------------------------------------------------------!
INTEGER,ALLOCATABLE :: EdgeNodes(:)
INTEGER,ALLOCATABLE :: NodesOnEdge(:)
INTEGER,ALLOCATABLE :: EdgesToNodesArray(:,:)
!----------------------------------------------------------------------------------------------------------------------!
CHARACTER(LEN=256) :: FormatString
!----------------------------------------------------------------------------------------------------------------------!

IF (PRESENT(Debug)) THEN
  DebugMode = Debug
ELSE
  DebugMode = .FALSE.
END IF

nElems = MeshArraysInfo%nElems

nElems_EDGE  = 0
nElems_TRI   = 0
nElems_QUAD  = 0
nElems_TETRA = 0
nElems_HEXA  = 0
nElems_PRISM = 0
nElems_PYRA  = 0

DO iElem=1,SIZE(MeshData_ElementsToElementType,1)
  ElemType = MeshData_ElementsToElementType(iElem)
  SELECT CASE(ElemType)
    CASE(ELEMTYPE_EDGE2)
      nElems_EDGE = nElems_EDGE+1
    CASE(ELEMTYPE_TRI3)
      nElems_TRI = nElems_TRI+1
    CASE(ELEMTYPE_QUAD4)
      nElems_QUAD = nElems_QUAD+1
    CASE(ELEMTYPE_TETRA4)
      nElems_TETRA = nElems_TETRA+1
    CASE(ELEMTYPE_HEXA8)
      nElems_HEXA = nElems_HEXA+1
    CASE(ELEMTYPE_PRISM6)
      nElems_PRISM = nElems_PRISM+1
    CASE(ELEMTYPE_PYRA5)
      nElems_PYRA = nElems_PYRA+1
  END SELECT
END DO

nEdgesTotal = 0

CALL GetnElemEdges(ELEMTYPE_EDGE2,nElemEdges)
nEdgesTotal = nEdgesTotal + nElemEdges*nElems_EDGE

CALL GetnElemEdges(ELEMTYPE_TRI3,nElemEdges)
nEdgesTotal = nEdgesTotal + nElemEdges*nElems_TRI

CALL GetnElemEdges(ELEMTYPE_QUAD4,nElemEdges)
nEdgesTotal = nEdgesTotal + nElemEdges*nElems_QUAD

CALL GetnElemEdges(ELEMTYPE_TETRA4,nElemEdges)
nEdgesTotal = nEdgesTotal + nElemEdges*nElems_TETRA

CALL GetnElemEdges(ELEMTYPE_HEXA8,nElemEdges)
nEdgesTotal = nEdgesTotal + nElemEdges*nElems_HEXA

CALL GetnElemEdges(ELEMTYPE_PRISM6,nElemEdges)
nEdgesTotal = nEdgesTotal + nElemEdges*nElems_PRISM

CALL GetnElemEdges(ELEMTYPE_PYRA5,nElemEdges)
nEdgesTotal = nEdgesTotal + nElemEdges*nElems_PYRA

IF (ALLOCATED(EdgesToNodesArray) .EQV. .TRUE.) THEN
  DEALLOCATE(EdgesToNodesArray)
END IF
ALLOCATE(EdgesToNodesArray(1:nEdgesTotal,1:5))

nEdgeNodes = 2
IF (ALLOCATED(EdgeNodes) .EQV. .TRUE.) THEN
  DEALLOCATE(EdgeNodes)
END IF
ALLOCATE(EdgeNodes(1:nEdgeNodes))

IF (ALLOCATED(NodesOnEdge) .EQV. .TRUE.) THEN
  DEALLOCATE(NodesOnEdge)
END IF
ALLOCATE(NodesOnEdge(1:nEdgeNodes))

EdgeID = 0
aElem => ElemList%FirstElem
DO WHILE (ASSOCIATED(aElem))
  CALL GetnElemEdges(aElem%ElemType,nElemEdges)
  DO iEdge=1,nElemEdges
    EdgeID = EdgeID+1
    CALL GetNodesOnEdge(aElem%ElemType,iEdge,NodesOnEdge)
    EdgeNodes(1) = aElem%Nodes(NodesOnEdge(1))%Node%NodeID
    EdgeNodes(2) = aElem%Nodes(NodesOnEdge(2))%Node%NodeID
    EdgesToNodesArray(EdgeID,3) = aElem%ElemID
    EdgesToNodesArray(EdgeID,4) = EdgeNodes(1)
    EdgesToNodesArray(EdgeID,5) = EdgeNodes(2)
    CALL QuickSort(EdgeNodes)
    EdgesToNodesArray(EdgeID,1) = EdgeNodes(1)
    EdgesToNodesArray(EdgeID,2) = EdgeNodes(2)
  END DO
  aElem => aElem%NextElem
END DO

CALL QuickSortArray(EdgesToNodesArray,3)

iEdge  = 1
EdgeID = 0
DO WHILE (iEdge .LE. nEdgesTotal)
  IF (iEdge .GT. 1) THEN
    IF (ALL(EdgesToNodesArray(iEdge,1:nEdgeNodes) .EQ. EdgesToNodesArray(iEdge-1,1:nEdgeNodes))) THEN
      iEdge = iEdge+1
      CYCLE
    ELSE
      EdgeID = EdgeID+1
    END IF
  ELSE
    EdgeID = EdgeID+1
  END IF
  ! Store EdgeID
  EdgeNodes(1:nEdgeNodes) = EdgesToNodesArray(iEdge,4:5)
  CALL CreateEdgeNodes(aEdgeNodes,EdgeID,nEdgeNodes,EdgeNodes)
  CALL AddEdgeNodesToList(aEdgeNodes,EdgeNodesList)
  iEdge = iEdge+1
END DO
nEdges = EdgeID

MeshInfo%nEdges       = nEdges
MeshArraysInfo%nEdges = nEdges

! Faces-to-Nodes Array
IF (ALLOCATED(MeshData_EdgesToNodes)) THEN
  DEALLOCATE(MeshData_EdgesToNodes)
END IF
ALLOCATE(MeshData_EdgesToNodes(1:nEdgeNodes,1:nEdges))

aEdgeNodes => EdgeNodesList%FirstEdgeNodes
DO WHILE(ASSOCIATED(aEdgeNodes))
  CALL GetEdgeNodesData(aEdgeNodes,EdgeID,EdgeNodes)
  MeshData_EdgesToNodes(1:nEdgeNodes,EdgeID) = EdgeNodes(1:nEdgeNodes)
  aEdgeNodes => aEdgeNodes%NextEdgeNodes
END DO

DEALLOCATE(EdgesToNodesArray)
CALL DestructEdgeNodesList(EdgeNodesList)

IF (DebugMode .EQV. .TRUE.) THEN
  WRITE(UNIT_SCREEN,*)
  WRITE(UNIT_SCREEN,"(2X,A)") "======================================================="
  WRITE(UNIT_SCREEN,"(2X,A)") "Printing Array: Edges-to-Nodes"
  WRITE(UNIT_SCREEN,"(2X,A)") "======================================================="
  WRITE(UNIT_SCREEN,"(1(2X,A8),1(2X,A))") "iEdge", "NodeIDs"
  FormatString = "(1(2X,I8),1X,2(1X,I0))"
  DO iEdge=1,nEdges
    WRITE(UNIT_SCREEN,FormatString) iEdge, MeshData_EdgesToNodes(1:nEdgeNodes,iEdge)
  END DO
  WRITE(UNIT_SCREEN,*)
END IF

!----------------------------------------------------------------------------------------------------------------------!
END SUBROUTINE CreateArray_EdgesToNodes
!======================================================================================================================!
!
!
!
!======================================================================================================================!
SUBROUTINE CreateArray_FacesToEdges(Debug)
!----------------------------------------------------------------------------------------------------------------------!
USE MOD_MeshMain_vars,ONLY: NodesToEdgesMap
!----------------------------------------------------------------------------------------------------------------------!
USE MOD_MeshMain_vars,ONLY: MeshArraysInfo
USE MOD_MeshMain_vars,ONLY: MeshData_FacesToNodes
USE MOD_MeshMain_vars,ONLY: MeshData_FacesToEdges
USE MOD_MeshMain_vars,ONLY: MeshData_FacesElementType
!----------------------------------------------------------------------------------------------------------------------!
USE MOD_Mesh_CGNS_Definitions
!----------------------------------------------------------------------------------------------------------------------!
USE MOD_DataStructures,ONLY: GetBucketID
USE MOD_DataStructures,ONLY: CheckBucketIDIsInHashTable
USE MOD_DataStructures,ONLY: PrintHashTable
USE MOD_DataStructures,ONLY: CreateHashTable
!----------------------------------------------------------------------------------------------------------------------!
IMPLICIT NONE
!----------------------------------------------------------------------------------------------------------------------!
! FORMAL ARGUMENTS
!----------------------------------------------------------------------------------------------------------------------!
LOGICAL,INTENT(IN),OPTIONAL            :: Debug
!----------------------------------------------------------------------------------------------------------------------!
! LOCAL VARIABLES
!----------------------------------------------------------------------------------------------------------------------!
INTEGER :: iFace
INTEGER :: nFaces
INTEGER :: iEdge
INTEGER :: nEdges
INTEGER :: EdgeID
INTEGER :: nNodes
INTEGER :: FaceType
INTEGER :: nFaceNodes
INTEGER :: nFaceEdges
INTEGER :: nEdgeNodes
INTEGER :: nMaxFaceEdges
LOGICAL :: DebugMode
!----------------------------------------------------------------------------------------------------------------------!
INTEGER,ALLOCATABLE :: NodeID(:)
INTEGER,ALLOCATABLE :: NodesOnEdge(:)
INTEGER,ALLOCATABLE :: FaceNodes(:)
!----------------------------------------------------------------------------------------------------------------------!
CHARACTER(LEN=256) :: FormatString
!----------------------------------------------------------------------------------------------------------------------!

IF (PRESENT(Debug)) THEN
  DebugMode = Debug
ELSE
  DebugMode = .FALSE.
END IF

!**************************************************!
! WARNING WARNING WARNING WARNING WARNING WARNING
!**************************************************!
! Make sure NodesToEdgesMap have been created
!**************************************************!

! For PP_nDims=2, MAX(nMaxFaceEdges)=MAX({1})=1
! For PP_nDims=3, MAX(nMaxFaceEdges)=MAX({3,4})=4
SELECT CASE(PP_nDims)
  CASE(2)
    nEdgeNodes    = 2
    nMaxFaceEdges = 1
  CASE(3)
    nEdgeNodes    = 2
    nMaxFaceEdges = 4
END SELECT

nNodes = MeshArraysInfo%nNodes
nFaces = MeshArraysInfo%nFaces
nEdges = MeshArraysInfo%nEdges

IF (ALLOCATED(MeshData_FacesToEdges) .EQV. .TRUE.) THEN
  DEALLOCATE(MeshData_FacesToEdges)
END IF
ALLOCATE(MeshData_FacesToEdges(1:nMaxFaceEdges,1:nFaces))
MeshData_FacesToEdges = 0

IF (ALLOCATED(NodeID) .EQV. .TRUE.) THEN
  DEALLOCATE(NodeID)
END IF
ALLOCATE(NodeID(1:nEdgeNodes))

IF (ALLOCATED(NodesOnEdge) .EQV. .TRUE.) THEN
  DEALLOCATE(NodesOnEdge)
END IF
ALLOCATE(NodesOnEdge(1:nEdgeNodes))

DO iFace=1,nFaces
  FaceType = MeshData_FacesElementType(iFace)
  CALL GetnElemNodes(FaceType,nFaceNodes)
  CALL GetnElemEdges(FaceType,nFaceEdges)

  IF (ALLOCATED(FaceNodes) .EQV. .TRUE.) THEN
    DEALLOCATE(FaceNodes)
  END IF
  ALLOCATE(FaceNodes(1:nFaceNodes))
  FaceNodes(1:nFaceNodes) = MeshData_FacesToNodes(1:nFaceNodes,iFace)

  DO iEdge=1,nFaceEdges
    CALL GetNodesOnEdge(FaceType,iEdge,NodesOnEdge)
    NodeID(1:nEdgeNodes) = FaceNodes(NodesOnEdge(1:nEdgeNodes))
    IF (CheckBucketIDIsInHashTable(NodesToEdgesMap,NodeID) .EQV. .TRUE.) THEN
      EdgeID = GetBucketID(NodesToEdgesMap,NodeID)
      MeshData_FacesToEdges(iEdge,iFace) = EdgeID
    ELSE
      CYCLE
    END IF
  END DO
END DO

IF (DebugMode .EQV. .TRUE.) THEN
  WRITE(UNIT_SCREEN,*)
  WRITE(UNIT_SCREEN,"(2X,A)") "======================================================="
  WRITE(UNIT_SCREEN,"(2X,A)") "Printing Array: Faces-to-Edges"
  WRITE(UNIT_SCREEN,"(2X,A)") "======================================================="
  WRITE(UNIT_SCREEN,"(3(2X,A8))") "iFace", "FaceType", "EdgeIDs"
  FormatString = "(1(2X,I8),4(2X,I8))"
  DO iFace=1,nFaces
    FaceType = MeshData_FacesElementType(iFace)
    CALL GetnElemEdges(FaceType,nFaceEdges)
    WRITE(FormatString,"(A,I0,A)") "(2(2X,I8),1X,", nFaceEdges, "(1X,I0))"
    WRITE(UNIT_SCREEN,FormatString) iFace, FaceType, MeshData_FacesToEdges(1:nFaceEdges,iFace)
  END DO
  WRITE(UNIT_SCREEN,*)
END IF

!----------------------------------------------------------------------------------------------------------------------!
END SUBROUTINE CreateArray_FacesToEdges
!======================================================================================================================!
!
!
!
!======================================================================================================================!
SUBROUTINE CreateArray_NonConformingFacesToNodes(Debug)
!----------------------------------------------------------------------------------------------------------------------!
USE MOD_NumericsTools,ONLY: COMPAREPOINTS
!----------------------------------------------------------------------------------------------------------------------!
USE MOD_MeshMain_vars,ONLY: tElem
USE MOD_MeshMain_vars,ONLY: tSide
USE MOD_MeshMain_vars,ONLY: ElemList
!----------------------------------------------------------------------------------------------------------------------!
USE MOD_MeshMainMethods,ONLY: GetFaceIDFromNodesToFacesMap
USE MOD_MeshMainMethods,ONLY: CheckFaceIDIsInNodesToFacesMap
!----------------------------------------------------------------------------------------------------------------------!
USE MOD_MeshMain_vars,ONLY: MeshData_FacesToNodes
USE MOD_MeshMain_vars,ONLY: MeshData_NodesCoordinates
USE MOD_MeshMain_vars,ONLY: MeshData_FacesElementType
!----------------------------------------------------------------------------------------------------------------------!
USE MOD_MeshMain_vars,ONLY: MeshData_MasterSlavesToNodes_Edge2Edge
USE MOD_MeshMain_vars,ONLY: MeshData_MasterSlavesToNodes_Tri4Tri
USE MOD_MeshMain_vars,ONLY: MeshData_MasterSlavesToNodes_Quad2Quad
USE MOD_MeshMain_vars,ONLY: MeshData_MasterSlavesToNodes_Quad4Quad
!----------------------------------------------------------------------------------------------------------------------!
USE MOD_MeshMain_vars,ONLY: MeshData_MasterSlavesToElements_Edge2Edge
USE MOD_MeshMain_vars,ONLY: MeshData_MasterSlavesToElements_Tri4Tri
USE MOD_MeshMain_vars,ONLY: MeshData_MasterSlavesToElements_Quad2Quad
USE MOD_MeshMain_vars,ONLY: MeshData_MasterSlavesToElements_Quad4Quad
!----------------------------------------------------------------------------------------------------------------------!
USE MOD_Mesh_CGNS_Definitions
!----------------------------------------------------------------------------------------------------------------------!
USE MOD_DataStructures,ONLY: tArrayINTEGER
USE MOD_DataStructures,ONLY: tLinkedList
USE MOD_DataStructures,ONLY: tLinkedListNode
USE MOD_DataStructures,ONLY: CreateLinkedListNode
USE MOD_DataStructures,ONLY: AddLinkedListNode
USE MOD_DataStructures,ONLY: GetLinkedListNode
USE MOD_DataStructures,ONLY: RemoveLastLinkedListNode
USE MOD_DataStructures,ONLY: PrintLinkedList
USE MOD_DataStructures,ONLY: DestructLinkedList
USE MOD_DataStructures,ONLY: CountLinkedListNodes
!----------------------------------------------------------------------------------------------------------------------!
USE MOD_DataStructures,ONLY: tKDTree
USE MOD_DataStructures,ONLY: tKDTreeNeighbors
USE MOD_DataStructures,ONLY: ConstructKDTree
USE MOD_DataStructures,ONLY: FindNearestNeighborsAroundQueryPoint
!----------------------------------------------------------------------------------------------------------------------!
USE MOD_DataStructures,ONLY: tBucket
USE MOD_DataStructures,ONLY: CreateBucket
USE MOD_DataStructures,ONLY: AddDataToBucket
USE MOD_DataStructures,ONLY: AddBucketIDToHashTable
USE MOD_DataStructures,ONLY: PrintHashTable
USE MOD_DataStructures,ONLY: CreateHashTable
!----------------------------------------------------------------------------------------------------------------------!
USE MOD_DataStructures,ONLY: GetBucketID
USE MOD_DataStructures,ONLY: CheckBucketIDIsInHashTable
!----------------------------------------------------------------------------------------------------------------------!
USE MOD_DataStructures,ONLY: QuickSort
USE MOD_DataStructures,ONLY: QuickSortArray
!----------------------------------------------------------------------------------------------------------------------!
IMPLICIT NONE
!----------------------------------------------------------------------------------------------------------------------!
! FORMAL ARGUMENTS
!----------------------------------------------------------------------------------------------------------------------!
LOGICAL,INTENT(IN),OPTIONAL :: Debug
!----------------------------------------------------------------------------------------------------------------------!
! LOCAL VARIABLES
!----------------------------------------------------------------------------------------------------------------------!
INTEGER :: iNode
INTEGER :: ElemID
INTEGER :: ElemID1
INTEGER :: ElemID2
INTEGER :: ElemID3
INTEGER :: ElemID4
INTEGER :: ElemID5
INTEGER :: FaceID
INTEGER :: FaceID1
INTEGER :: FaceID2
INTEGER :: FaceID3
INTEGER :: FaceID4
INTEGER :: FaceID5
INTEGER :: Level
INTEGER :: NodeID
INTEGER :: NodeID1
INTEGER :: NodeID2
INTEGER :: NodeID3
INTEGER :: NodeID4
INTEGER :: NodeID5
INTEGER :: NodeID6
INTEGER :: NodeID7
INTEGER :: NodeID8
INTEGER :: NodeID9
INTEGER :: IndexFaceID1
INTEGER :: IndexFaceID2
INTEGER :: IndexFaceID3
INTEGER :: IndexFaceID4
INTEGER :: IndexFaceID5
INTEGER :: nNeighbors
INTEGER :: FaceType
INTEGER :: nFaceNodes
INTEGER :: BucketID
INTEGER :: Nodes_EDGE(1:2)
INTEGER :: Nodes_TRI(1:3)
INTEGER :: Nodes_QUAD(1:4)
INTEGER :: nTargetNodes
INTEGER :: IndexNodeID3
INTEGER :: IndexNodeID4
INTEGER :: IndexNodeID5
INTEGER :: IndexNodeID6
INTEGER :: IndexNodeID7
INTEGER :: IndexNodeID8
INTEGER :: IndexNodeID9
LOGICAL :: FlagNodeID3
LOGICAL :: FlagNodeID4
LOGICAL :: FlagNodeID5
LOGICAL :: FlagNodeID6
LOGICAL :: FlagNodeID7
LOGICAL :: FlagNodeID8
LOGICAL :: FlagNodeID9
LOGICAL :: FlagEdge2Edge
LOGICAL :: FlagTri4Tri
LOGICAL :: FlagQuad2Quadv1
LOGICAL :: FlagQuad2Quadv2
LOGICAL :: FlagQuad4Quad
LOGICAL :: DebugMode
REAL    :: QueryPoint3(1:3)
REAL    :: QueryPoint4(1:3)
REAL    :: QueryPoint5(1:3)
REAL    :: QueryPoint6(1:3)
REAL    :: QueryPoint7(1:3)
REAL    :: QueryPoint8(1:3)
REAL    :: QueryPoint9(1:3)
REAL    :: FoundPoint3(1:3)
REAL    :: FoundPoint4(1:3)
REAL    :: FoundPoint5(1:3)
REAL    :: FoundPoint6(1:3)
REAL    :: FoundPoint7(1:3)
REAL    :: FoundPoint8(1:3)
REAL    :: FoundPoint9(1:3)
!----------------------------------------------------------------------------------------------------------------------!
TYPE(tArrayINTEGER) :: BucketData
!----------------------------------------------------------------------------------------------------------------------!
INTEGER :: iInnerSide
INTEGER :: nInnerSides
INTEGER :: iNonConformingFaceNode
INTEGER :: nNonConformingFaceNodes
INTEGER :: iNonConformingSide
INTEGER :: nNonConformingSides
!----------------------------------------------------------------------------------------------------------------------!
INTEGER,ALLOCATABLE :: InnerSidesArray(:,:)
INTEGER,ALLOCATABLE :: NonConformingSidesArray(:,:)
LOGICAL,ALLOCATABLE :: NonConformingSidesFlagArray(:)
!----------------------------------------------------------------------------------------------------------------------!
REAL,ALLOCATABLE :: TargetNodesCoords(:,:)
!----------------------------------------------------------------------------------------------------------------------!
TYPE(tBucket),ALLOCATABLE :: FaceIDToIndexMap(:)
!----------------------------------------------------------------------------------------------------------------------!
INTEGER,ALLOCATABLE :: FaceNodesArray(:)
REAL,ALLOCATABLE    :: FacesNodesCoordinates(:,:)
!----------------------------------------------------------------------------------------------------------------------!
TYPE(tElem),POINTER :: aElem
TYPE(tSide),POINTER :: aSide
!----------------------------------------------------------------------------------------------------------------------!
TYPE(tLinkedList)             :: FaceNodesList
TYPE(tLinkedListNode),POINTER :: aFaceNode
!----------------------------------------------------------------------------------------------------------------------!
INTEGER                       :: iMasterSlaveNodes_Edge2Edge
INTEGER                       :: iMasterSlaveNodes_Tri4Tri
INTEGER                       :: iMasterSlaveNodes_Quad2Quad
INTEGER                       :: iMasterSlaveNodes_Quad4Quad
INTEGER                       :: nMasterSlaveNodes_Edge2Edge
INTEGER                       :: nMasterSlaveNodes_Tri4Tri
INTEGER                       :: nMasterSlaveNodes_Quad2Quad
INTEGER                       :: nMasterSlaveNodes_Quad4Quad
TYPE(tArrayINTEGER)           :: MasterSlaveNodesBucket_Edge2Edge
TYPE(tArrayINTEGER)           :: MasterSlaveNodesBucket_Tri4Tri
TYPE(tArrayINTEGER)           :: MasterSlaveNodesBucket_Quad2Quad
TYPE(tArrayINTEGER)           :: MasterSlaveNodesBucket_Quad4Quad
TYPE(tLinkedList)             :: MasterSlaveNodesList_Edge2Edge
TYPE(tLinkedList)             :: MasterSlaveNodesList_Tri4Tri
TYPE(tLinkedList)             :: MasterSlaveNodesList_Quad2Quad
TYPE(tLinkedList)             :: MasterSlaveNodesList_Quad4Quad
TYPE(tLinkedListNode),POINTER :: aMasterSlaveNode
!----------------------------------------------------------------------------------------------------------------------!
TYPE(tLinkedList)             :: InnerSidesList
TYPE(tLinkedListNode),POINTER :: aInnerSide
!----------------------------------------------------------------------------------------------------------------------!
TYPE(tLinkedList)             :: NonConformingSidesList
TYPE(tLinkedListNode),POINTER :: aNonConformingSide
!----------------------------------------------------------------------------------------------------------------------!
TYPE(tKDTree),POINTER              :: KDTreeFacesNodesCoordinates
TYPE(tKDTreeNeighbors),ALLOCATABLE :: SearchResults(:)
!----------------------------------------------------------------------------------------------------------------------!
CHARACTER(LEN=256) :: FormatString
!----------------------------------------------------------------------------------------------------------------------!

IF (PRESENT(Debug)) THEN
  DebugMode = Debug
ELSE
  DebugMode = .FALSE.
END IF

! ! ! ! WARNING
! ! ! DebugMode = .TRUE.
! ! ! ! WARNING

!------------------------------------------------------------!
! Extraction of Inner Sides (no BC Sides)
!------------------------------------------------------------!

IF (ALLOCATED(BucketData%Data)) THEN
  DEALLOCATE(BucketData%Data)
END IF
ALLOCATE(BucketData%Data(1:3))

! Extracting side information from each element
aElem => ElemList%FirstElem
DO WHILE (ASSOCIATED(aElem))
  aSide => aElem%FirstSide
  DO WHILE (ASSOCIATED(aSide))
    IF (ASSOCIATED(aSide%BC) .EQV. .FALSE.) THEN
      Level    = aElem%Level
      FaceID   = aSide%SideID
      ElemID   = aElem%ElemID
      BucketID = aSide%SideID
      BucketData%Data(1) = Level
      BucketData%Data(2) = FaceID
      BucketData%Data(3) = ElemID
      CALL CreateLinkedListNode(aInnerSide,BucketID,BucketData)
      CALL AddLinkedListNode(aInnerSide,InnerSidesList)
    END IF
    aSide => aSide%NextElemSide
  END DO
  aElem => aElem%NextElem
END DO

! Counting number of entries in list
CALL CountLinkedListNodes(InnerSidesList,nInnerSides)

! Allocating inner-sides (conforming and non-conforming) array
IF (ALLOCATED(InnerSidesArray)) THEN
  DEALLOCATE(InnerSidesArray)
END IF
ALLOCATE(InnerSidesArray(1:nInnerSides,1:3))

! Exporting inner-sides (conforming and non-conforming) data from list to array
iInnerSide = 0
aInnerSide => InnerSidesList%FirstLinkedListNode
DO WHILE(ASSOCIATED(aInnerSide))
  iInnerSide = iInnerSide+1
  CALL GetLinkedListNode(aInnerSide,BucketID,BucketData)
  Level  = BucketData%Data(1)
  FaceID = BucketData%Data(2)
  ElemID = BucketData%Data(3)
  InnerSidesArray(iInnerSide,1) = Level
  InnerSidesArray(iInnerSide,2) = FaceID
  InnerSidesArray(iInnerSide,3) = ElemID
  aInnerSide => aInnerSide%NextLinkedListNode
END DO

CALL DestructLinkedList(InnerSidesList)

! Sorting inner-sides (conforming and non-conforming) array
CALL QuickSortArray(InnerSidesArray,2)

! Removing inner-sides (conforming) in list
DO iInnerSide=1,nInnerSides
  Level  = InnerSidesArray(iInnerSide,1)
  FaceID = InnerSidesArray(iInnerSide,2)
  ElemID = InnerSidesArray(iInnerSide,3)
  IF (.NOT. ASSOCIATED(NonConformingSidesList%LastLinkedListNode)) THEN
    BucketID = iInnerSide
    BucketData%Data(1) = Level
    BucketData%Data(2) = FaceID
    BucketData%Data(3) = ElemID
    CALL CreateLinkedListNode(aNonConformingSide,BucketID,BucketData)
    CALL AddLinkedListNode(aNonConformingSide,NonConformingSidesList)
  ELSE
    CALL GetLinkedListNode(NonConformingSidesList%LastLinkedListNode,BucketID,BucketData)
    FaceID1 = BucketData%Data(2)
    IF (FaceID .EQ. FaceID1) THEN
      CALL RemoveLastLinkedListNode(NonConformingSidesList)
    ELSE
      BucketID = iInnerSide
      BucketData%Data(1) = Level
      BucketData%Data(2) = FaceID
      BucketData%Data(3) = ElemID
      CALL CreateLinkedListNode(aNonConformingSide,BucketID,BucketData)
      CALL AddLinkedListNode(aNonConformingSide,NonConformingSidesList)
    END IF
  END IF
END DO
IF (ALLOCATED(InnerSidesArray)) THEN
  DEALLOCATE(InnerSidesArray)
END IF

! Counting number of entries in list
CALL CountLinkedListNodes(NonConformingSidesList,nNonConformingSides)

IF (nNonConformingSides .GT. 0) THEN
  !------------------------------------------------------------!
  ! Extraction of NonConforming Sides
  !------------------------------------------------------------!
  ! Allocating inner-sides (non-conforming) array
  IF (ALLOCATED(NonConformingSidesArray)) THEN
    DEALLOCATE(NonConformingSidesArray)
  END IF
  ALLOCATE(NonConformingSidesArray(1:nNonConformingSides,1:3))
  IF (ALLOCATED(NonConformingSidesFlagArray)) THEN
    DEALLOCATE(NonConformingSidesFlagArray)
  END IF
  ALLOCATE(NonConformingSidesFlagArray(1:nNonConformingSides))
  NonConformingSidesFlagArray = .FALSE.

  ! Exporting inner-sides (non-conforming) data from list to array
  iNonConformingSide = 0
  aNonConformingSide => NonConformingSidesList%FirstLinkedListNode
  DO WHILE(ASSOCIATED(aNonConformingSide))
    iNonConformingSide = iNonConformingSide+1
    CALL GetLinkedListNode(aNonConformingSide,BucketID,BucketData)
    Level  = BucketData%Data(1)
    FaceID = BucketData%Data(2)
    ElemID = BucketData%Data(3)
    NonConformingSidesArray(iNonConformingSide,1) = Level
    NonConformingSidesArray(iNonConformingSide,2) = FaceID
    NonConformingSidesArray(iNonConformingSide,3) = ElemID
    aNonConformingSide => aNonConformingSide%NextLinkedListNode
  END DO

  CALL DestructLinkedList(NonConformingSidesList)

  ! Sorting inner-sides (non-conforming) array
  CALL QuickSortArray(NonConformingSidesArray,2)

  IF (DebugMode .EQV. .TRUE.) THEN
    WRITE(UNIT_SCREEN,*)
    WRITE(UNIT_SCREEN,"(2X,A)") "======================================================="
    WRITE(UNIT_SCREEN,"(2X,A)") "Printing Array: NonConformingSidesArray"
    WRITE(UNIT_SCREEN,"(2X,A)") "======================================================="
    WRITE(UNIT_SCREEN,"(5(2X,A8))") "iSide", "Level", "FaceID", "ElemID", "NodeIDs"
    DO iNonConformingSide=1,nNonConformingSides
      FaceID = NonConformingSidesArray(iNonConformingSide,2)
      FaceType = MeshData_FacesElementType(FaceID)
      CALL GetnElemNodes(FaceType,nFaceNodes)
      WRITE(FormatString,"(A,I0,A)") "(4(2X,I8),2X,", nFaceNodes, "(1X,I0))"
      WRITE(UNIT_SCREEN,FormatString) &
        iNonConformingSide, &
        NonConformingSidesArray(iNonConformingSide,1), &
        NonConformingSidesArray(iNonConformingSide,2), &
        NonConformingSidesArray(iNonConformingSide,3), &
        MeshData_FacesToNodes(1:nFaceNodes,FaceID)
    END DO
    WRITE(UNIT_SCREEN,*)
  END IF

  ! Creating FaceIDToIndexMap
  CALL CreateHashTable(FaceIDToIndexMap,nNonConformingSides,1)
  DO iNonConformingSide=1,nNonConformingSides
    FaceID = NonConformingSidesArray(iNonConformingSide,2)
    CALL AddBucketIDToHashTable(FaceIDToIndexMap,iNonConformingSide,(/FaceID/))
  END DO

  !------------------------------------------------------------!
  ! Extraction of Nodes from NonConforming Sides
  !------------------------------------------------------------!

  ! Extracting face-nodes information (non-conforming)
  DO iNonConformingSide=1,nNonConformingSides
    FaceID = NonConformingSidesArray(iNonConformingSide,2)
    FaceType = MeshData_FacesElementType(FaceID)
    CALL GetnElemNodes(FaceType,nFaceNodes)
    DO iNode=1,nFaceNodes
      NodeID = MeshData_FacesToNodes(iNode,FaceID)
      CALL CreateLinkedListNode(aFaceNode,FaceID,NodeID)
      CALL AddLinkedListNode(aFaceNode,FaceNodesList)
    END DO
  END DO

  ! Counting number of entries in list
  CALL CountLinkedListNodes(FaceNodesList,nNonConformingFaceNodes)

  ! Allocating face-nodes (non-conforming) array
  IF (ALLOCATED(FaceNodesArray)) THEN
    DEALLOCATE(FaceNodesArray)
  END IF
  ALLOCATE(FaceNodesArray(1:nNonConformingFaceNodes))

  ! Exporting face-nodes (non-conforming) data from list to array
  iNonConformingFaceNode = 0
  aFaceNode => FaceNodesList%FirstLinkedListNode
  DO WHILE(ASSOCIATED(aFaceNode))
    iNonConformingFaceNode = iNonConformingFaceNode+1
    CALL GetLinkedListNode(aFaceNode,BucketID,NodeID)
    FaceNodesArray(iNonConformingFaceNode) = NodeID
    aFaceNode => aFaceNode%NextLinkedListNode
  END DO

  CALL DestructLinkedList(FaceNodesList)

  ! Sorting face-nodes (non-conforming) array
  CALL QuickSort(FaceNodesArray)

  ! Removing face-nodes (non-conforming) entries already in list
  DO iNonConformingFaceNode=1,nNonConformingFaceNodes
    NodeID = FaceNodesArray(iNonConformingFaceNode)
    IF (.NOT. ASSOCIATED(FaceNodesList%LastLinkedListNode)) THEN
      CALL CreateLinkedListNode(aFaceNode,NodeID)
      CALL AddLinkedListNode(aFaceNode,FaceNodesList)
    ELSE
      CALL GetLinkedListNode(FaceNodesList%LastLinkedListNode,BucketID)
      IF (BucketID .EQ. NodeID) THEN
        CYCLE
      ELSE
        CALL CreateLinkedListNode(aFaceNode,NodeID)
        CALL AddLinkedListNode(aFaceNode,FaceNodesList)
      END IF
    END IF
  END DO

  ! Counting number of entries in list
  CALL CountLinkedListNodes(FaceNodesList,nNonConformingFaceNodes)

  ! Allocating array of inner sides (conforming and non-conforming)
  IF (ALLOCATED(FaceNodesArray)) THEN
    DEALLOCATE(FaceNodesArray)
  END IF
  IF (ALLOCATED(FacesNodesCoordinates)) THEN
    DEALLOCATE(FacesNodesCoordinates)
  END IF
  ALLOCATE(FaceNodesArray(1:nNonConformingFaceNodes))
  ALLOCATE(FacesNodesCoordinates(1:3,1:nNonConformingFaceNodes))

  ! Exporting inner sides (conforming and non-conforming) data from list to array
  iNonConformingFaceNode = 0
  aFaceNode => FaceNodesList%FirstLinkedListNode
  DO WHILE(ASSOCIATED(aFaceNode))
    iNonConformingFaceNode = iNonConformingFaceNode+1
    CALL GetLinkedListNode(aFaceNode,NodeID)
    FaceNodesArray(iNonConformingFaceNode) = NodeID
    FacesNodesCoordinates(1:3,iNonConformingFaceNode) = MeshData_NodesCoordinates(1:3,NodeID)
    aFaceNode => aFaceNode%NextLinkedListNode
  END DO

  CALL DestructLinkedList(FaceNodesList)

  !------------------------------------------------------------!
  ! Creation of KDTree of NonConforming FacesNodesCoordinates
  !------------------------------------------------------------!
  CALL ConstructKDTree(KDTreeFacesNodesCoordinates,FacesNodesCoordinates,Sort=.FALSE.,Rearrange=.FALSE.)

  !------------------------------------------------------------!
  ! Extracting face-nodes information (non-conforming)
  !------------------------------------------------------------!
  iMasterSlaveNodes_Edge2Edge = 0
  iMasterSlaveNodes_Tri4Tri   = 0
  iMasterSlaveNodes_Quad2Quad = 0
  iMasterSlaveNodes_Quad4Quad = 0
  DO iNonConformingSide=1,nNonConformingSides
    IF (NonConformingSidesFlagArray(iNonConformingSide) .EQV. .TRUE.) THEN
      CYCLE
    END IF
    FaceID = NonConformingSidesArray(iNonConformingSide,2)
    FaceType = MeshData_FacesElementType(FaceID)
    CALL GetnElemNodes(FaceType,nFaceNodes)
    SELECT CASE(FaceType)
      CASE(ELEMTYPE_EDGE2)
        !------------------------------------------------------------!
        ! Edge2Edge: [N1 N2 N3]
        !------------------------------------------------------------!
        !  Edge2Edge    MasterEdge    SlaveEdge1    SlaveEdge2       !
        !  o N2         o N2           x N3          o N2            !
        !  |            |              |             |               !
        !  |            |              |             |               !
        !  x N3         |              |             |               !
        !  |            |              |             |               !
        !  |            |              |             |               !
        !  o N1         o N1           o N1          x N3            !
        !------------------------------------------------------------!

        nMasterSlaveNodes_Edge2Edge = 3
        IF (ALLOCATED(MasterSlaveNodesBucket_Edge2Edge%Data)) THEN
          DEALLOCATE(MasterSlaveNodesBucket_Edge2Edge%Data)
        END IF
        ALLOCATE(MasterSlaveNodesBucket_Edge2Edge%Data(1:nMasterSlaveNodes_Edge2Edge))
        !------------------------------------------------------------!
        nNeighbors = 1
        IF (ALLOCATED(SearchResults)) THEN
          DEALLOCATE(SearchResults)
        END IF
        ALLOCATE(SearchResults(1:nNeighbors))
        !------------------------------------------------------------!
        nTargetNodes = 1
        NodeID1 = MeshData_FacesToNodes(1,FaceID)
        NodeID2 = MeshData_FacesToNodes(2,FaceID)
        IF (ALLOCATED(TargetNodesCoords)) THEN
          DEALLOCATE(TargetNodesCoords)
        END IF
        ALLOCATE(TargetNodesCoords(1:3,1:nTargetNodes))

        ! Target Node 1: NodeID3
        TargetNodesCoords(1:3,1) = 0.5*(MeshData_NodesCoordinates(1:3,NodeID1)+MeshData_NodesCoordinates(1:3,NodeID2))
        QueryPoint3(1:3) = TargetNodesCoords(1:3,1)
        CALL FindNearestNeighborsAroundQueryPoint(KDTreeFacesNodesCoordinates,QueryPoint3,nNeighbors,SearchResults)
        IndexNodeID3 = SearchResults(1)%NeighborIndex
        FoundPoint3(1:3) = FacesNodesCoordinates(1:3,IndexNodeID3)

        FlagNodeID3 = .FALSE.
        IF (COMPAREPOINTS(QueryPoint3,FoundPoint3) .EQV. .TRUE.) THEN
          NodeID3 = FaceNodesArray(IndexNodeID3)
          FlagNodeID3 = .TRUE.
        END IF

        FlagEdge2Edge = FlagNodeID3

        IF (FlagEdge2Edge .EQV. .TRUE.) THEN
          Nodes_EDGE(1:2) = (/NodeID1,NodeID3/)
          IF (CheckFaceIDIsInNodesToFacesMap(Nodes_EDGE) .EQV. .TRUE.) THEN
            FaceID1 = GetFaceIDFromNodesToFacesMap(Nodes_EDGE)
          END IF

          Nodes_EDGE(1:2) = (/NodeID3,NodeID2/)
          IF (CheckFaceIDIsInNodesToFacesMap(Nodes_EDGE) .EQV. .TRUE.) THEN
            FaceID2 = GetFaceIDFromNodesToFacesMap(Nodes_EDGE)
          END IF

          IF (CheckBucketIDIsInHashTable(FaceIDToIndexMap,(/FaceID1/)) .EQV. .TRUE.) THEN
            IndexFaceID1 = GetBucketID(FaceIDToIndexMap,(/FaceID1/))
            NonConformingSidesFlagArray(IndexFaceID1) = .TRUE.
          END IF

          IF (CheckBucketIDIsInHashTable(FaceIDToIndexMap,(/FaceID2/)) .EQV. .TRUE.) THEN
            IndexFaceID2 = GetBucketID(FaceIDToIndexMap,(/FaceID2/))
            NonConformingSidesFlagArray(IndexFaceID2) = .TRUE.
          END IF

          MasterSlaveNodesBucket_Edge2Edge%Data(1) = NodeID1
          MasterSlaveNodesBucket_Edge2Edge%Data(2) = NodeID2
          MasterSlaveNodesBucket_Edge2Edge%Data(3) = NodeID3

          iMasterSlaveNodes_Edge2Edge = iMasterSlaveNodes_Edge2Edge+1
          CALL CreateLinkedListNode(aMasterSlaveNode,iMasterSlaveNodes_Edge2Edge,MasterSlaveNodesBucket_Edge2Edge)
          CALL AddLinkedListNode(aMasterSlaveNode,MasterSlaveNodesList_Edge2Edge)
        END IF
        !------------------------------------------------------------!
      CASE(ELEMTYPE_TRI3)
        !------------------------------------------------------------!
        ! Tri2Tri: [N1 N2 N3 N4]
        !------------------------------------------------------------!
        !       Tri2Tri                    MasterTri                 !
        !          N3                          N3                    !
        !          o                           o                     !
        !         /|\                         / \                    !
        !        / | \                       /   \                   !
        !       /  |  \                     /     \                  !
        !      /   |   \                   /       \                 !
        !     /    |    \                 /         \                !
        ! N1 o-----x-----o N2         N1 o-----------o N2            !
        !          N4                                                !
        !------------------------------------------------------------!
        !       SlaveTri1                  SlaveTri2                 !
        !         N3                           N3                    !
        !          o                           o                     !
        !         /|                           |\                    !
        !        / |                           | \                   !
        !       /  |                           |  \                  !
        !      /   |                           |   \                 !
        !     /    |                           |    \                !
        ! N1 o-----x                           x-----o N2            !
        !          N4                          N4                    !
        !------------------------------------------------------------!
        ! Tri4Tri: [N1 N2 N3 N4 N5 N6]
        !------------------------------------------------------------!
        !       Tri4Tri                    MasterTri                 !
        !          N3                          N3                    !
        !          o                           o                     !
        !         / \                         / \                    !
        !        /   \                       /   \                   !
        !   N6  x-----x N5                  /     \                  !
        !      / \   / \                   /       \                 !
        !     /   \ /   \                 /         \                !
        ! N1 o-----x-----o N2         N1 o-----------o N2            !
        !          N4                                                !
        !------------------------------------------------------------!
        !       SlaveTri1                  SlaveTri2                 !
        !          N6                          N5                    !
        !          x                           x                     !
        !         / \                         / \                    !
        !        /   \                       /   \                   !
        !       /     \                     /     \                  !
        !      /       \                   /       \                 !
        !     /         \                 /         \                !
        ! N1 o-----------x N4         N4 x-----------o N2            !
        !------------------------------------------------------------!
        !       SlaveTri3                  SlaveTri4                 !
        !          N3                          N6                    !
        !          o                           x                     !
        !         / \                         / \                    !
        !        /   \                       /   \                   !
        !       /     \                     /     \                  !
        !      /       \                   /       \                 !
        !     /         \                 /         \                !
        ! N6 x-----------x N5         N4 x-----------x N5            !
        !------------------------------------------------------------!

        nMasterSlaveNodes_Tri4Tri = 6
        IF (ALLOCATED(MasterSlaveNodesBucket_Tri4Tri%Data)) THEN
          DEALLOCATE(MasterSlaveNodesBucket_Tri4Tri%Data)
        END IF
        ALLOCATE(MasterSlaveNodesBucket_Tri4Tri%Data(1:nMasterSlaveNodes_Tri4Tri))
        !------------------------------------------------------------!
        nNeighbors = 1
        IF (ALLOCATED(SearchResults)) THEN
          DEALLOCATE(SearchResults)
        END IF
        ALLOCATE(SearchResults(1:nNeighbors))
        !------------------------------------------------------------!
        nTargetNodes = 3
        NodeID1 = MeshData_FacesToNodes(1,FaceID)
        NodeID2 = MeshData_FacesToNodes(2,FaceID)
        NodeID3 = MeshData_FacesToNodes(3,FaceID)
        IF (ALLOCATED(TargetNodesCoords)) THEN
          DEALLOCATE(TargetNodesCoords)
        END IF
        ALLOCATE(TargetNodesCoords(1:3,1:nTargetNodes))

        ! Target Node 1: NodeID4
        TargetNodesCoords(1:3,1) = 0.5*(MeshData_NodesCoordinates(1:3,NodeID1)+MeshData_NodesCoordinates(1:3,NodeID2))
        QueryPoint4(1:3) = TargetNodesCoords(1:3,1)
        CALL FindNearestNeighborsAroundQueryPoint(KDTreeFacesNodesCoordinates,QueryPoint4,nNeighbors,SearchResults)
        IndexNodeID4 = SearchResults(1)%NeighborIndex
        FoundPoint4(1:3) = FacesNodesCoordinates(1:3,IndexNodeID4)

        ! Target Node 2: NodeID5
        TargetNodesCoords(1:3,2) = 0.5*(MeshData_NodesCoordinates(1:3,NodeID2)+MeshData_NodesCoordinates(1:3,NodeID3))
        QueryPoint5(1:3) = TargetNodesCoords(1:3,2)
        CALL FindNearestNeighborsAroundQueryPoint(KDTreeFacesNodesCoordinates,QueryPoint5,nNeighbors,SearchResults)
        IndexNodeID5 = SearchResults(1)%NeighborIndex
        FoundPoint5(1:3) = FacesNodesCoordinates(1:3,IndexNodeID5)

        ! Target Node 3: NodeID6
        TargetNodesCoords(1:3,3) = 0.5*(MeshData_NodesCoordinates(1:3,NodeID1)+MeshData_NodesCoordinates(1:3,NodeID3))
        QueryPoint6(1:3) = TargetNodesCoords(1:3,3)
        CALL FindNearestNeighborsAroundQueryPoint(KDTreeFacesNodesCoordinates,QueryPoint6,nNeighbors,SearchResults)
        IndexNodeID6 = SearchResults(1)%NeighborIndex
        FoundPoint6(1:3) = FacesNodesCoordinates(1:3,IndexNodeID6)
        
        FlagNodeID4 = .FALSE.
        IF (COMPAREPOINTS(QueryPoint4,FoundPoint4) .EQV. .TRUE.) THEN
          NodeID4 = FaceNodesArray(IndexNodeID4)
          FlagNodeID4 = .TRUE.
        END IF

        FlagNodeID5 = .FALSE.
        IF (COMPAREPOINTS(QueryPoint5,FoundPoint5) .EQV. .TRUE.) THEN
          NodeID5 = FaceNodesArray(IndexNodeID5)
          FlagNodeID5 = .TRUE.
        END IF

        FlagNodeID6 = .FALSE.
        IF (COMPAREPOINTS(QueryPoint6,FoundPoint6) .EQV. .TRUE.) THEN
          NodeID6 = FaceNodesArray(IndexNodeID6)
          FlagNodeID6 = .TRUE.
        END IF

        FlagTri4Tri = (FlagNodeID4 .AND. FlagNodeID5 .AND. FlagNodeID6)

        IF (FlagTri4Tri .EQV. .TRUE.) THEN
          Nodes_TRI(1:3) = (/NodeID1,NodeID4,NodeID6/)
          IF (CheckFaceIDIsInNodesToFacesMap(Nodes_TRI) .EQV. .TRUE.) THEN
            FaceID1 = GetFaceIDFromNodesToFacesMap(Nodes_TRI)
          END IF

          Nodes_TRI(1:3) = (/NodeID4,NodeID2,NodeID5/)
          IF (CheckFaceIDIsInNodesToFacesMap(Nodes_TRI) .EQV. .TRUE.) THEN
            FaceID2 = GetFaceIDFromNodesToFacesMap(Nodes_TRI)
          END IF

          Nodes_TRI(1:3) = (/NodeID6,NodeID5,NodeID3/)
          IF (CheckFaceIDIsInNodesToFacesMap(Nodes_TRI) .EQV. .TRUE.) THEN
            FaceID3 = GetFaceIDFromNodesToFacesMap(Nodes_TRI)
          END IF

          Nodes_TRI(1:3) = (/NodeID4,NodeID5,NodeID6/)
          IF (CheckFaceIDIsInNodesToFacesMap(Nodes_TRI) .EQV. .TRUE.) THEN
            FaceID4 = GetFaceIDFromNodesToFacesMap(Nodes_TRI)
          END IF

          IF (CheckBucketIDIsInHashTable(FaceIDToIndexMap,(/FaceID1/)) .EQV. .TRUE.) THEN
            IndexFaceID1 = GetBucketID(FaceIDToIndexMap,(/FaceID1/))
            NonConformingSidesFlagArray(IndexFaceID1) = .TRUE.
          END IF

          IF (CheckBucketIDIsInHashTable(FaceIDToIndexMap,(/FaceID2/)) .EQV. .TRUE.) THEN
            IndexFaceID2 = GetBucketID(FaceIDToIndexMap,(/FaceID2/))
            NonConformingSidesFlagArray(IndexFaceID2) = .TRUE.
          END IF

          IF (CheckBucketIDIsInHashTable(FaceIDToIndexMap,(/FaceID3/)) .EQV. .TRUE.) THEN
            IndexFaceID3 = GetBucketID(FaceIDToIndexMap,(/FaceID3/))
            NonConformingSidesFlagArray(IndexFaceID3) = .TRUE.
          END IF

          IF (CheckBucketIDIsInHashTable(FaceIDToIndexMap,(/FaceID4/)) .EQV. .TRUE.) THEN
            IndexFaceID4 = GetBucketID(FaceIDToIndexMap,(/FaceID4/))
            NonConformingSidesFlagArray(IndexFaceID4) = .TRUE.
          END IF

          MasterSlaveNodesBucket_Tri4Tri%Data(1) = NodeID1
          MasterSlaveNodesBucket_Tri4Tri%Data(2) = NodeID2
          MasterSlaveNodesBucket_Tri4Tri%Data(3) = NodeID3
          MasterSlaveNodesBucket_Tri4Tri%Data(4) = NodeID4
          MasterSlaveNodesBucket_Tri4Tri%Data(5) = NodeID5
          MasterSlaveNodesBucket_Tri4Tri%Data(6) = NodeID6

          iMasterSlaveNodes_Tri4Tri = iMasterSlaveNodes_Tri4Tri+1
          CALL CreateLinkedListNode(aMasterSlaveNode,iMasterSlaveNodes_Tri4Tri,MasterSlaveNodesBucket_Tri4Tri)
          CALL AddLinkedListNode(aMasterSlaveNode,MasterSlaveNodesList_Tri4Tri)
        END IF
        !-------------------------------------HashTable-----------------------!
      CASE(ELEMTYPE_QUAD4)
        !------------------------------------------------------------!
        ! Quad2Quad-v1: [N1 N2 N3 N4 N5 N7]
        !------------------------------------------------------------!
        !       Quad2Quad               MasterQuad                   !
        !           N7                                               !
        ! N4 o------x------o N3    N4 o-------------o N3             !
        !    |      |      |          |             |                !
        !    |      |      |          |             |                !
        !    |      |      |          |             |                !
        !    |      |      |          |             |                !
        !    |      |      |          |             |                !
        ! N1 o------x------o N2    N1 o-------------o N2             !
        !           N5                                               !
        !------------------------------------------------------------!
        !       SlaveQuad1               SlaveQuad2                  !
        ! N4 o------x N7                  N7 x------o N3             !
        !    |      |                        |      |                !
        !    |      |                        |      |                !
        !    |      |                        |      |                !
        !    |      |                        |      |                !
        !    |      |                        |      |                !
        ! N1 o------x N5                  N5 x------o N2             !
        !------------------------------------------------------------!
        ! Quad2Quad-v2: [N2 N3 N4 N1 N6 N8]
        !------------------------------------------------------------!
        !       Quad2Quad               MasterQuad                   !
        ! N4 o-------------o N3    N4 o-------------o N3             !
        !    |             |          |             |                !
        !    |             |          |             |                !
        ! N8 x-------------x N6       |             |                !
        !    |             |          |             |                !
        !    |             |          |             |                !
        ! N1 o-------------o N2    N1 o-------------o N2             !
        !                                                            !
        !------------------------------------------------------------!
        !       SlaveQuad1               SlaveQuad2                  !
        ! N8 x-------------x N6    N4 o-------------o N3             !
        !    |             |          |             |                !
        !    |             |          |             |                !
        ! N1 o-------------o N2    N8 x-------------x N6             !
        !------------------------------------------------------------!
        ! Quad4Quad: [N1 N2 N3 N4 N5 N6 N7 N8 N9]
        !------------------------------------------------------------!
        !       Quad4Quad               MasterQuad                   !
        !           N7                                               !
        ! N4 o------x------o N3    N4 o-------------o N3             !
        !    |      |      |          |             |                !
        !    |      |      |          |             |                !
        ! N8 x------x------x N6       |             |                !
        !    |      |N9    |          |             |                !
        !    |      |      |          |             |                !
        ! N1 o------x------o N2    N1 o-------------o N2             !
        !           N5                                               !
        !------------------------------------------------------------!
        !       SlaveQuad1               SlaveQuad2                  !
        ! N8 x-------------x N9    N9 x-------------x N6             !
        !    |             |          |             |                !
        !    |             |          |             |                !
        !    |             |          |             |                !
        !    |             |          |             |                !
        !    |             |          |             |                !
        ! N1 o-------------x N5    N5 x-------------o N2             !
        !------------------------------------------------------------!
        !       SlaveQuad3               SlaveQuad4                  !
        ! N4 o-------------x N7    N7 x-------------o N3             !
        !    |             |          |             |                !
        !    |             |          |             |                !
        !    |             |          |             |                !
        !    |             |          |             |                !
        !    |             |          |             |                !
        ! N8 x-------------x N9    N9 x-------------x N6             !
        !------------------------------------------------------------!

        nMasterSlaveNodes_Quad2Quad = 6
        IF (ALLOCATED(MasterSlaveNodesBucket_Quad2Quad%Data)) THEN
          DEALLOCATE(MasterSlaveNodesBucket_Quad2Quad%Data)
        END IF
        ALLOCATE(MasterSlaveNodesBucket_Quad2Quad%Data(1:nMasterSlaveNodes_Quad2Quad))
        !------------------------------------------------------------!
        nMasterSlaveNodes_Quad4Quad = 9
        IF (ALLOCATED(MasterSlaveNodesBucket_Quad4Quad%Data)) THEN
          DEALLOCATE(MasterSlaveNodesBucket_Quad4Quad%Data)
        END IF
        ALLOCATE(MasterSlaveNodesBucket_Quad4Quad%Data(1:nMasterSlaveNodes_Quad4Quad))
        !------------------------------------------------------------!
        nNeighbors = 1
        IF (ALLOCATED(SearchResults)) THEN
          DEALLOCATE(SearchResults)
        END IF
        ALLOCATE(SearchResults(1:nNeighbors))
        !------------------------------------------------------------!
        nTargetNodes = 5
        NodeID1 = MeshData_FacesToNodes(1,FaceID)
        NodeID2 = MeshData_FacesToNodes(2,FaceID)
        NodeID3 = MeshData_FacesToNodes(3,FaceID)
        NodeID4 = MeshData_FacesToNodes(4,FaceID)

        ! Initialized to invalid values
        NodeID5 = -995
        NodeID6 = -996
        NodeID7 = -997
        NodeID8 = -998
        NodeID9 = -999

        IF (ALLOCATED(TargetNodesCoords)) THEN
          DEALLOCATE(TargetNodesCoords)
        END IF
        ALLOCATE(TargetNodesCoords(1:3,1:nTargetNodes))

        ! Target Node 1: NodeID5
        TargetNodesCoords(1:3,1) = 0.5*(MeshData_NodesCoordinates(1:3,NodeID1)+MeshData_NodesCoordinates(1:3,NodeID2))
        QueryPoint5(1:3) = TargetNodesCoords(1:3,1)
        CALL FindNearestNeighborsAroundQueryPoint(KDTreeFacesNodesCoordinates,QueryPoint5,nNeighbors,SearchResults)
        IndexNodeID5 = SearchResults(1)%NeighborIndex
        FoundPoint5(1:3) = FacesNodesCoordinates(1:3,IndexNodeID5)

        ! Target Node 2: NodeID6
        TargetNodesCoords(1:3,2) = 0.5*(MeshData_NodesCoordinates(1:3,NodeID2)+MeshData_NodesCoordinates(1:3,NodeID3))
        QueryPoint6(1:3) = TargetNodesCoords(1:3,2)
        CALL FindNearestNeighborsAroundQueryPoint(KDTreeFacesNodesCoordinates,QueryPoint6,nNeighbors,SearchResults)
        IndexNodeID6 = SearchResults(1)%NeighborIndex
        FoundPoint6(1:3) = FacesNodesCoordinates(1:3,IndexNodeID6)

        ! Target Node 3: NodeID7
        TargetNodesCoords(1:3,3) = 0.5*(MeshData_NodesCoordinates(1:3,NodeID3)+MeshData_NodesCoordinates(1:3,NodeID4))
        QueryPoint7(1:3) = TargetNodesCoords(1:3,3)
        CALL FindNearestNeighborsAroundQueryPoint(KDTreeFacesNodesCoordinates,QueryPoint7,nNeighbors,SearchResults)
        IndexNodeID7 = SearchResults(1)%NeighborIndex
        FoundPoint7(1:3) = FacesNodesCoordinates(1:3,IndexNodeID7)

        ! Target Node 4: NodeID8
        TargetNodesCoords(1:3,4) = 0.5*(MeshData_NodesCoordinates(1:3,NodeID4)+MeshData_NodesCoordinates(1:3,NodeID1))
        QueryPoint8(1:3) = TargetNodesCoords(1:3,4)
        CALL FindNearestNeighborsAroundQueryPoint(KDTreeFacesNodesCoordinates,QueryPoint8,nNeighbors,SearchResults)
        IndexNodeID8 = SearchResults(1)%NeighborIndex
        FoundPoint8(1:3) = FacesNodesCoordinates(1:3,IndexNodeID8)

        ! Target Node 5: NodeID9 ******** WARNING ******** CENTROID OF QUADRILATERAL ******** WARNING ********
        TargetNodesCoords(1:3,5) = 0.5*(MeshData_NodesCoordinates(1:3,NodeID1)+MeshData_NodesCoordinates(1:3,NodeID3))
        QueryPoint9(1:3) = TargetNodesCoords(1:3,5)
        CALL FindNearestNeighborsAroundQueryPoint(KDTreeFacesNodesCoordinates,QueryPoint9,nNeighbors,SearchResults)
        IndexNodeID9 = SearchResults(1)%NeighborIndex
        FoundPoint9(1:3) = FacesNodesCoordinates(1:3,IndexNodeID9)

        FlagNodeID5 = .FALSE.
        IF (COMPAREPOINTS(QueryPoint5,FoundPoint5) .EQV. .TRUE.) THEN
          NodeID5 = FaceNodesArray(IndexNodeID5)
          FlagNodeID5 = .TRUE.
        END IF

        FlagNodeID6 = .FALSE.
        IF (COMPAREPOINTS(QueryPoint6,FoundPoint6) .EQV. .TRUE.) THEN
          NodeID6 = FaceNodesArray(IndexNodeID6)
          FlagNodeID6 = .TRUE.
        END IF

        FlagNodeID7 = .FALSE.
        IF (COMPAREPOINTS(QueryPoint7,FoundPoint7) .EQV. .TRUE.) THEN
          NodeID7 = FaceNodesArray(IndexNodeID7)
          FlagNodeID7 = .TRUE.
        END IF

        FlagNodeID8 = .FALSE.
        IF (COMPAREPOINTS(QueryPoint8,FoundPoint8) .EQV. .TRUE.) THEN
          NodeID8 = FaceNodesArray(IndexNodeID8)
          FlagNodeID8 = .TRUE.
        END IF

        FlagNodeID9 = .FALSE.
        IF (COMPAREPOINTS(QueryPoint9,FoundPoint9) .EQV. .TRUE.) THEN
          NodeID9 = FaceNodesArray(IndexNodeID9)
          FlagNodeID9 = .TRUE.
        END IF

        FlagQuad4Quad   = (FlagNodeID5 .AND. FlagNodeID6 .AND. FlagNodeID7 .AND. FlagNodeID8 .AND. FlagNodeID9)
        FlagQuad2Quadv1 = (FlagNodeID5 .AND. FlagNodeID7 .AND. (.NOT. FlagNodeID9))
        FlagQuad2Quadv2 = (FlagNodeID6 .AND. FlagNodeID8 .AND. (.NOT. FlagNodeID9))

        IF (FlagQuad2Quadv1 .EQV. .TRUE.) THEN
          Nodes_QUAD(1:4) = (/NodeID1,NodeID5,NodeID7,NodeID4/)
          IF (CheckFaceIDIsInNodesToFacesMap(Nodes_QUAD) .EQV. .TRUE.) THEN
            FaceID1 = GetFaceIDFromNodesToFacesMap(Nodes_QUAD)
          END IF

          Nodes_QUAD(1:4) = (/NodeID5,NodeID2,NodeID3,NodeID7/)
          IF (CheckFaceIDIsInNodesToFacesMap(Nodes_QUAD) .EQV. .TRUE.) THEN
            FaceID2 = GetFaceIDFromNodesToFacesMap(Nodes_QUAD)
          END IF

          IF (CheckBucketIDIsInHashTable(FaceIDToIndexMap,(/FaceID1/)) .EQV. .TRUE.) THEN
            IndexFaceID1 = GetBucketID(FaceIDToIndexMap,(/FaceID1/))
            NonConformingSidesFlagArray(IndexFaceID1) = .TRUE.
          END IF

          IF (CheckBucketIDIsInHashTable(FaceIDToIndexMap,(/FaceID2/)) .EQV. .TRUE.) THEN
            IndexFaceID2 = GetBucketID(FaceIDToIndexMap,(/FaceID2/))
            NonConformingSidesFlagArray(IndexFaceID2) = .TRUE.
          END IF

          MasterSlaveNodesBucket_Quad2Quad%Data(1) = NodeID1
          MasterSlaveNodesBucket_Quad2Quad%Data(2) = NodeID2
          MasterSlaveNodesBucket_Quad2Quad%Data(3) = NodeID3
          MasterSlaveNodesBucket_Quad2Quad%Data(4) = NodeID4
          MasterSlaveNodesBucket_Quad2Quad%Data(5) = NodeID5
          MasterSlaveNodesBucket_Quad2Quad%Data(6) = NodeID7

          iMasterSlaveNodes_Quad2Quad = iMasterSlaveNodes_Quad2Quad+1
          CALL CreateLinkedListNode(aMasterSlaveNode,iMasterSlaveNodes_Quad2Quad,MasterSlaveNodesBucket_Quad2Quad)
          CALL AddLinkedListNode(aMasterSlaveNode,MasterSlaveNodesList_Quad2Quad)
        END IF
        IF (FlagQuad2Quadv2 .EQV. .TRUE.) THEN
          Nodes_QUAD(1:4) = (/NodeID1,NodeID2,NodeID6,NodeID8/)
          IF (CheckFaceIDIsInNodesToFacesMap(Nodes_QUAD) .EQV. .TRUE.) THEN
            FaceID1 = GetFaceIDFromNodesToFacesMap(Nodes_QUAD)
          END IF

          Nodes_QUAD(1:4) = (/NodeID8,NodeID6,NodeID3,NodeID4/)
          IF (CheckFaceIDIsInNodesToFacesMap(Nodes_QUAD) .EQV. .TRUE.) THEN
            FaceID2 = GetFaceIDFromNodesToFacesMap(Nodes_QUAD)
          END IF

          IF (CheckBucketIDIsInHashTable(FaceIDToIndexMap,(/FaceID1/)) .EQV. .TRUE.) THEN
            IndexFaceID1 = GetBucketID(FaceIDToIndexMap,(/FaceID1/))
            NonConformingSidesFlagArray(IndexFaceID1) = .TRUE.
          END IF

          IF (CheckBucketIDIsInHashTable(FaceIDToIndexMap,(/FaceID2/)) .EQV. .TRUE.) THEN
            IndexFaceID2 = GetBucketID(FaceIDToIndexMap,(/FaceID2/))
            NonConformingSidesFlagArray(IndexFaceID2) = .TRUE.
          END IF

          MasterSlaveNodesBucket_Quad2Quad%Data(1) = NodeID2
          MasterSlaveNodesBucket_Quad2Quad%Data(2) = NodeID3
          MasterSlaveNodesBucket_Quad2Quad%Data(3) = NodeID4
          MasterSlaveNodesBucket_Quad2Quad%Data(4) = NodeID1
          MasterSlaveNodesBucket_Quad2Quad%Data(5) = NodeID6
          MasterSlaveNodesBucket_Quad2Quad%Data(6) = NodeID8

          iMasterSlaveNodes_Quad2Quad = iMasterSlaveNodes_Quad2Quad+1
          CALL CreateLinkedListNode(aMasterSlaveNode,iMasterSlaveNodes_Quad2Quad,MasterSlaveNodesBucket_Quad2Quad)
          CALL AddLinkedListNode(aMasterSlaveNode,MasterSlaveNodesList_Quad2Quad)
        END IF
        IF (FlagQuad4Quad .EQV. .TRUE.) THEN
          Nodes_QUAD(1:4) = (/NodeID1,NodeID5,NodeID9,NodeID8/)
          IF (CheckFaceIDIsInNodesToFacesMap(Nodes_QUAD) .EQV. .TRUE.) THEN
            FaceID1 = GetFaceIDFromNodesToFacesMap(Nodes_QUAD)
          END IF

          Nodes_QUAD(1:4) = (/NodeID5,NodeID2,NodeID6,NodeID9/)
          IF (CheckFaceIDIsInNodesToFacesMap(Nodes_QUAD) .EQV. .TRUE.) THEN
            FaceID2 = GetFaceIDFromNodesToFacesMap(Nodes_QUAD)
          END IF

          Nodes_QUAD(1:4) = (/NodeID8,NodeID9,NodeID7,NodeID4/)
          IF (CheckFaceIDIsInNodesToFacesMap(Nodes_QUAD) .EQV. .TRUE.) THEN
            FaceID3 = GetFaceIDFromNodesToFacesMap(Nodes_QUAD)
          END IF

          Nodes_QUAD(1:4) = (/NodeID9,NodeID6,NodeID3,NodeID7/)
          IF (CheckFaceIDIsInNodesToFacesMap(Nodes_QUAD) .EQV. .TRUE.) THEN
            FaceID4 = GetFaceIDFromNodesToFacesMap(Nodes_QUAD)
          END IF

          IF (CheckBucketIDIsInHashTable(FaceIDToIndexMap,(/FaceID1/)) .EQV. .TRUE.) THEN
            IndexFaceID1 = GetBucketID(FaceIDToIndexMap,(/FaceID1/))
            NonConformingSidesFlagArray(IndexFaceID1) = .TRUE.
          END IF

          IF (CheckBucketIDIsInHashTable(FaceIDToIndexMap,(/FaceID2/)) .EQV. .TRUE.) THEN
            IndexFaceID2 = GetBucketID(FaceIDToIndexMap,(/FaceID2/))
            NonConformingSidesFlagArray(IndexFaceID2) = .TRUE.
          END IF

          IF (CheckBucketIDIsInHashTable(FaceIDToIndexMap,(/FaceID3/)) .EQV. .TRUE.) THEN
            IndexFaceID3 = GetBucketID(FaceIDToIndexMap,(/FaceID3/))
            NonConformingSidesFlagArray(IndexFaceID3) = .TRUE.
          END IF

          IF (CheckBucketIDIsInHashTable(FaceIDToIndexMap,(/FaceID4/)) .EQV. .TRUE.) THEN
            IndexFaceID4 = GetBucketID(FaceIDToIndexMap,(/FaceID4/))
            NonConformingSidesFlagArray(IndexFaceID4) = .TRUE.
          END IF

          MasterSlaveNodesBucket_Quad4Quad%Data(1) = NodeID1
          MasterSlaveNodesBucket_Quad4Quad%Data(2) = NodeID2
          MasterSlaveNodesBucket_Quad4Quad%Data(3) = NodeID3
          MasterSlaveNodesBucket_Quad4Quad%Data(4) = NodeID4
          MasterSlaveNodesBucket_Quad4Quad%Data(5) = NodeID5
          MasterSlaveNodesBucket_Quad4Quad%Data(6) = NodeID6
          MasterSlaveNodesBucket_Quad4Quad%Data(7) = NodeID7
          MasterSlaveNodesBucket_Quad4Quad%Data(8) = NodeID8
          MasterSlaveNodesBucket_Quad4Quad%Data(9) = NodeID9

          iMasterSlaveNodes_Quad4Quad = iMasterSlaveNodes_Quad4Quad+1
          CALL CreateLinkedListNode(aMasterSlaveNode,iMasterSlaveNodes_Quad4Quad,MasterSlaveNodesBucket_Quad4Quad)
          CALL AddLinkedListNode(aMasterSlaveNode,MasterSlaveNodesList_Quad4Quad)
        END IF
        !------------------------------------------------------------!
    END SELECT
  END DO
END IF

IF (nNonConformingSides .GT. 0) THEN
  !------------------------------------------------------------!
  ! MeshData_MasterSlavesToElements: Edge2Edge
  !------------------------------------------------------------!
  CALL CountLinkedListNodes(MasterSlaveNodesList_Edge2Edge,nMasterSlaveNodes_Edge2Edge)
  IF (nMasterSlaveNodes_Edge2Edge .GT. 0) THEN
    IF (ALLOCATED(MeshData_MasterSlavesToNodes_Edge2Edge)) THEN
      DEALLOCATE(MeshData_MasterSlavesToNodes_Edge2Edge)
    END IF
    IF (ALLOCATED(MeshData_MasterSlavesToElements_Edge2Edge)) THEN
      DEALLOCATE(MeshData_MasterSlavesToElements_Edge2Edge)
    END IF
    ALLOCATE(MeshData_MasterSlavesToNodes_Edge2Edge(1:3,1:nMasterSlaveNodes_Edge2Edge))
    ALLOCATE(MeshData_MasterSlavesToElements_Edge2Edge(1:3,1:nMasterSlaveNodes_Edge2Edge))

    aMasterSlaveNode => MasterSlaveNodesList_Edge2Edge%FirstLinkedListNode
    DO WHILE(ASSOCIATED(aMasterSlaveNode))
      CALL GetLinkedListNode(aMasterSlaveNode,iMasterSlaveNodes_Edge2Edge,MasterSlaveNodesBucket_Edge2Edge)
      NodeID1 = MasterSlaveNodesBucket_Edge2Edge%Data(1)
      NodeID2 = MasterSlaveNodesBucket_Edge2Edge%Data(2)
      NodeID3 = MasterSlaveNodesBucket_Edge2Edge%Data(3)

      Nodes_EDGE(1:2) = (/NodeID1,NodeID2/)
      IF (CheckFaceIDIsInNodesToFacesMap(Nodes_EDGE) .EQV. .TRUE.) THEN
        FaceID1 = GetFaceIDFromNodesToFacesMap(Nodes_EDGE)
        IndexFaceID1 = GetBucketID(FaceIDToIndexMap,(/FaceID1/))
        ElemID1 = NonConformingSidesArray(IndexFaceID1,3)
      END IF

      Nodes_EDGE(1:2) = (/NodeID1,NodeID3/)
      IF (CheckFaceIDIsInNodesToFacesMap(Nodes_EDGE) .EQV. .TRUE.) THEN
        FaceID2 = GetFaceIDFromNodesToFacesMap(Nodes_EDGE)
        IndexFaceID2 = GetBucketID(FaceIDToIndexMap,(/FaceID2/))
        ElemID2 = NonConformingSidesArray(IndexFaceID2,3)
      END IF

      Nodes_EDGE(1:2) = (/NodeID3,NodeID2/)
      IF (CheckFaceIDIsInNodesToFacesMap(Nodes_EDGE) .EQV. .TRUE.) THEN
        FaceID3 = GetFaceIDFromNodesToFacesMap(Nodes_EDGE)
        IndexFaceID3 = GetBucketID(FaceIDToIndexMap,(/FaceID3/))
        ElemID3 = NonConformingSidesArray(IndexFaceID3,3)
      END IF

      MeshData_MasterSlavesToNodes_Edge2Edge(1,iMasterSlaveNodes_Edge2Edge) = NodeID1
      MeshData_MasterSlavesToNodes_Edge2Edge(2,iMasterSlaveNodes_Edge2Edge) = NodeID2
      MeshData_MasterSlavesToNodes_Edge2Edge(3,iMasterSlaveNodes_Edge2Edge) = NodeID3

      MeshData_MasterSlavesToElements_Edge2Edge(1,iMasterSlaveNodes_Edge2Edge) = ElemID1
      MeshData_MasterSlavesToElements_Edge2Edge(2,iMasterSlaveNodes_Edge2Edge) = ElemID2
      MeshData_MasterSlavesToElements_Edge2Edge(3,iMasterSlaveNodes_Edge2Edge) = ElemID3

      aMasterSlaveNode => aMasterSlaveNode%NextLinkedListNode
    END DO

    IF (DebugMode .EQV. .TRUE.) THEN
      WRITE(UNIT_SCREEN,*)
      WRITE(UNIT_SCREEN,"(2X,A)") "======================================================="
      WRITE(UNIT_SCREEN,"(2X,A)") "Printing List: MasterSlaveNodesList_Edge2Edge"
      WRITE(UNIT_SCREEN,"(2X,A)") "======================================================="
      WRITE(UNIT_SCREEN,"(3(2X,A8))") "iBucket", "BucketID", "NodeIDs"
      CALL PrintLinkedList(MasterSlaveNodesList_Edge2Edge)
      WRITE(UNIT_SCREEN,*)
      WRITE(UNIT_SCREEN,"(2X,A)") "======================================================="
      WRITE(UNIT_SCREEN,"(2X,A)") "Printing List: MeshData_MasterSlavesToNodes_Edge2Edge"
      WRITE(UNIT_SCREEN,"(2X,A)") "======================================================="
      WRITE(UNIT_SCREEN,"(2(2X,A8))") "iBucket", "ElemIDs"
      FormatString = "(1(2X,I8),2X,3(1X,I0))"
      DO iMasterSlaveNodes_Edge2Edge=1,nMasterSlaveNodes_Edge2Edge
        WRITE(UNIT_SCREEN,FormatString) &
          iMasterSlaveNodes_Edge2Edge, MeshData_MasterSlavesToNodes_Edge2Edge(1:3,iMasterSlaveNodes_Edge2Edge)
      END DO
      WRITE(UNIT_SCREEN,*)
      WRITE(UNIT_SCREEN,*)
      WRITE(UNIT_SCREEN,"(2X,A)") "======================================================="
      WRITE(UNIT_SCREEN,"(2X,A)") "Printing List: MeshData_MasterSlavesToElements_Edge2Edge"
      WRITE(UNIT_SCREEN,"(2X,A)") "======================================================="
      WRITE(UNIT_SCREEN,"(2(2X,A8))") "iBucket", "ElemIDs"
      FormatString = "(1(2X,I8),2X,3(1X,I0))"
      DO iMasterSlaveNodes_Edge2Edge=1,nMasterSlaveNodes_Edge2Edge
        WRITE(UNIT_SCREEN,FormatString) &
          iMasterSlaveNodes_Edge2Edge, MeshData_MasterSlavesToElements_Edge2Edge(1:3,iMasterSlaveNodes_Edge2Edge)
      END DO
      WRITE(UNIT_SCREEN,*)
    END IF
  END IF

  !------------------------------------------------------------!
  ! MeshData_MasterSlavesToElements: Tri4Tri
  !------------------------------------------------------------!
  CALL CountLinkedListNodes(MasterSlaveNodesList_Tri4Tri,nMasterSlaveNodes_Tri4Tri)
  IF (nMasterSlaveNodes_Tri4Tri .GT. 0) THEN
    IF (ALLOCATED(MeshData_MasterSlavesToNodes_Tri4Tri)) THEN
      DEALLOCATE(MeshData_MasterSlavesToNodes_Tri4Tri)
    END IF
    IF (ALLOCATED(MeshData_MasterSlavesToElements_Tri4Tri)) THEN
      DEALLOCATE(MeshData_MasterSlavesToElements_Tri4Tri)
    END IF
    ALLOCATE(MeshData_MasterSlavesToNodes_Tri4Tri(1:6,1:nMasterSlaveNodes_Tri4Tri))
    ALLOCATE(MeshData_MasterSlavesToElements_Tri4Tri(1:5,1:nMasterSlaveNodes_Tri4Tri))

    aMasterSlaveNode => MasterSlaveNodesList_Tri4Tri%FirstLinkedListNode
    DO WHILE(ASSOCIATED(aMasterSlaveNode))
      CALL GetLinkedListNode(aMasterSlaveNode,iMasterSlaveNodes_Tri4Tri,MasterSlaveNodesBucket_Tri4Tri)
      NodeID1 = MasterSlaveNodesBucket_Tri4Tri%Data(1)
      NodeID2 = MasterSlaveNodesBucket_Tri4Tri%Data(2)
      NodeID3 = MasterSlaveNodesBucket_Tri4Tri%Data(3)
      NodeID4 = MasterSlaveNodesBucket_Tri4Tri%Data(4)
      NodeID5 = MasterSlaveNodesBucket_Tri4Tri%Data(5)
      NodeID6 = MasterSlaveNodesBucket_Tri4Tri%Data(6)

      Nodes_TRI(1:3) = (/NodeID1,NodeID2,NodeID3/)
      IF (CheckFaceIDIsInNodesToFacesMap(Nodes_TRI) .EQV. .TRUE.) THEN
        FaceID1 = GetFaceIDFromNodesToFacesMap(Nodes_TRI)
        IndexFaceID1 = GetBucketID(FaceIDToIndexMap,(/FaceID1/))
        ElemID1 = NonConformingSidesArray(IndexFaceID1,3)
      END IF

      Nodes_TRI(1:3) = (/NodeID1,NodeID4,NodeID6/)
      IF (CheckFaceIDIsInNodesToFacesMap(Nodes_TRI) .EQV. .TRUE.) THEN
        FaceID2 = GetFaceIDFromNodesToFacesMap(Nodes_TRI)
        IndexFaceID2 = GetBucketID(FaceIDToIndexMap,(/FaceID2/))
        ElemID2 = NonConformingSidesArray(IndexFaceID2,3)
      END IF

      Nodes_TRI(1:3) = (/NodeID4,NodeID2,NodeID5/)
      IF (CheckFaceIDIsInNodesToFacesMap(Nodes_TRI) .EQV. .TRUE.) THEN
        FaceID3 = GetFaceIDFromNodesToFacesMap(Nodes_TRI)
        IndexFaceID3 = GetBucketID(FaceIDToIndexMap,(/FaceID3/))
        ElemID3 = NonConformingSidesArray(IndexFaceID3,3)
      END IF

      Nodes_TRI(1:3) = (/NodeID6,NodeID5,NodeID3/)
      IF (CheckFaceIDIsInNodesToFacesMap(Nodes_TRI) .EQV. .TRUE.) THEN
        FaceID4 = GetFaceIDFromNodesToFacesMap(Nodes_TRI)
        IndexFaceID4 = GetBucketID(FaceIDToIndexMap,(/FaceID4/))
        ElemID4 = NonConformingSidesArray(IndexFaceID4,3)
      END IF

      Nodes_TRI(1:3) = (/NodeID4,NodeID5,NodeID6/)
      IF (CheckFaceIDIsInNodesToFacesMap(Nodes_TRI) .EQV. .TRUE.) THEN
        FaceID5 = GetFaceIDFromNodesToFacesMap(Nodes_TRI)
        IndexFaceID5 = GetBucketID(FaceIDToIndexMap,(/FaceID5/))
        ElemID5 = NonConformingSidesArray(IndexFaceID5,3)
      END IF

      MeshData_MasterSlavesToNodes_Tri4Tri(1,iMasterSlaveNodes_Tri4Tri) = NodeID1
      MeshData_MasterSlavesToNodes_Tri4Tri(2,iMasterSlaveNodes_Tri4Tri) = NodeID2
      MeshData_MasterSlavesToNodes_Tri4Tri(3,iMasterSlaveNodes_Tri4Tri) = NodeID3
      MeshData_MasterSlavesToNodes_Tri4Tri(4,iMasterSlaveNodes_Tri4Tri) = NodeID4
      MeshData_MasterSlavesToNodes_Tri4Tri(5,iMasterSlaveNodes_Tri4Tri) = NodeID5
      MeshData_MasterSlavesToNodes_Tri4Tri(6,iMasterSlaveNodes_Tri4Tri) = NodeID6

      MeshData_MasterSlavesToElements_Tri4Tri(1,iMasterSlaveNodes_Tri4Tri) = ElemID1
      MeshData_MasterSlavesToElements_Tri4Tri(2,iMasterSlaveNodes_Tri4Tri) = ElemID2
      MeshData_MasterSlavesToElements_Tri4Tri(3,iMasterSlaveNodes_Tri4Tri) = ElemID3
      MeshData_MasterSlavesToElements_Tri4Tri(4,iMasterSlaveNodes_Tri4Tri) = ElemID4
      MeshData_MasterSlavesToElements_Tri4Tri(5,iMasterSlaveNodes_Tri4Tri) = ElemID5

      aMasterSlaveNode => aMasterSlaveNode%NextLinkedListNode
    END DO

    IF (DebugMode .EQV. .TRUE.) THEN
      WRITE(UNIT_SCREEN,*)
      WRITE(UNIT_SCREEN,"(2X,A)") "======================================================="
      WRITE(UNIT_SCREEN,"(2X,A)") "Printing List: MasterSlaveNodesList_Tri4Tri"
      WRITE(UNIT_SCREEN,"(2X,A)") "======================================================="
      WRITE(UNIT_SCREEN,"(3(2X,A8))") "iBucket", "BucketID", "NodeIDs"
      CALL PrintLinkedList(MasterSlaveNodesList_Tri4Tri)
      WRITE(UNIT_SCREEN,*)
      WRITE(UNIT_SCREEN,"(2X,A)") "======================================================="
      WRITE(UNIT_SCREEN,"(2X,A)") "Printing List: MeshData_MasterSlavesToNodes_Tri4Tri"
      WRITE(UNIT_SCREEN,"(2X,A)") "======================================================="
      WRITE(UNIT_SCREEN,"(2(2X,A8))") "iBucket", "NodeIDs"
      FormatString = "(1(2X,I8),2X,6(1X,I0))"
      DO iMasterSlaveNodes_Tri4Tri=1,nMasterSlaveNodes_Tri4Tri
        WRITE(UNIT_SCREEN,FormatString) &
          iMasterSlaveNodes_Tri4Tri, MeshData_MasterSlavesToNodes_Tri4Tri(1:6,iMasterSlaveNodes_Tri4Tri)
      END DO
      WRITE(UNIT_SCREEN,*)
      WRITE(UNIT_SCREEN,*)
      WRITE(UNIT_SCREEN,"(2X,A)") "======================================================="
      WRITE(UNIT_SCREEN,"(2X,A)") "Printing List: MeshData_MasterSlavesToElements_Tri4Tri"
      WRITE(UNIT_SCREEN,"(2X,A)") "======================================================="
      WRITE(UNIT_SCREEN,"(2(2X,A8))") "iBucket", "ElemIDs"
      FormatString = "(1(2X,I8),2X,5(1X,I0))"
      DO iMasterSlaveNodes_Tri4Tri=1,nMasterSlaveNodes_Tri4Tri
        WRITE(UNIT_SCREEN,FormatString) &
          iMasterSlaveNodes_Tri4Tri, MeshData_MasterSlavesToElements_Tri4Tri(1:5,iMasterSlaveNodes_Tri4Tri)
      END DO
      WRITE(UNIT_SCREEN,*)
    END IF
  END IF

  !------------------------------------------------------------!
  ! MeshData_MasterSlavesToElements: Quad2Quad
  !------------------------------------------------------------!
  CALL CountLinkedListNodes(MasterSlaveNodesList_Quad2Quad,nMasterSlaveNodes_Quad2Quad)
  IF (nMasterSlaveNodes_Quad2Quad .GT. 0) THEN
    IF (ALLOCATED(MeshData_MasterSlavesToNodes_Quad2Quad)) THEN
      DEALLOCATE(MeshData_MasterSlavesToNodes_Quad2Quad)
    END IF
    IF (ALLOCATED(MeshData_MasterSlavesToElements_Quad2Quad)) THEN
      DEALLOCATE(MeshData_MasterSlavesToElements_Quad2Quad)
    END IF
    ALLOCATE(MeshData_MasterSlavesToNodes_Quad2Quad(1:6,1:nMasterSlaveNodes_Quad2Quad))
    ALLOCATE(MeshData_MasterSlavesToElements_Quad2Quad(1:3,1:nMasterSlaveNodes_Quad2Quad))

    aMasterSlaveNode => MasterSlaveNodesList_Quad2Quad%FirstLinkedListNode
    DO WHILE(ASSOCIATED(aMasterSlaveNode))
      CALL GetLinkedListNode(aMasterSlaveNode,iMasterSlaveNodes_Quad2Quad,MasterSlaveNodesBucket_Quad2Quad)
      NodeID1 = MasterSlaveNodesBucket_Quad2Quad%Data(1)
      NodeID2 = MasterSlaveNodesBucket_Quad2Quad%Data(2)
      NodeID3 = MasterSlaveNodesBucket_Quad2Quad%Data(3)
      NodeID4 = MasterSlaveNodesBucket_Quad2Quad%Data(4)
      NodeID5 = MasterSlaveNodesBucket_Quad2Quad%Data(5)
      NodeID6 = MasterSlaveNodesBucket_Quad2Quad%Data(6)

      Nodes_QUAD(1:4) = (/NodeID1,NodeID2,NodeID3,NodeID4/)
      IF (CheckFaceIDIsInNodesToFacesMap(Nodes_QUAD) .EQV. .TRUE.) THEN
        FaceID1 = GetFaceIDFromNodesToFacesMap(Nodes_QUAD)
        IndexFaceID1 = GetBucketID(FaceIDToIndexMap,(/FaceID1/))
        ElemID1 = NonConformingSidesArray(IndexFaceID1,3)
      END IF

      Nodes_QUAD(1:4) = (/NodeID1,NodeID5,NodeID6,NodeID4/)
      IF (CheckFaceIDIsInNodesToFacesMap(Nodes_QUAD) .EQV. .TRUE.) THEN
        FaceID2 = GetFaceIDFromNodesToFacesMap(Nodes_QUAD)
        IndexFaceID2 = GetBucketID(FaceIDToIndexMap,(/FaceID2/))
        ElemID2 = NonConformingSidesArray(IndexFaceID2,3)
      END IF

      Nodes_QUAD(1:4) = (/NodeID5,NodeID2,NodeID3,NodeID6/)
      IF (CheckFaceIDIsInNodesToFacesMap(Nodes_QUAD) .EQV. .TRUE.) THEN
        FaceID3 = GetFaceIDFromNodesToFacesMap(Nodes_QUAD)
        IndexFaceID3 = GetBucketID(FaceIDToIndexMap,(/FaceID3/))
        ElemID3 = NonConformingSidesArray(IndexFaceID3,3)
      END IF

      MeshData_MasterSlavesToNodes_Quad2Quad(1,iMasterSlaveNodes_Quad2Quad) = NodeID1
      MeshData_MasterSlavesToNodes_Quad2Quad(2,iMasterSlaveNodes_Quad2Quad) = NodeID2
      MeshData_MasterSlavesToNodes_Quad2Quad(3,iMasterSlaveNodes_Quad2Quad) = NodeID3
      MeshData_MasterSlavesToNodes_Quad2Quad(4,iMasterSlaveNodes_Quad2Quad) = NodeID4
      MeshData_MasterSlavesToNodes_Quad2Quad(5,iMasterSlaveNodes_Quad2Quad) = NodeID5
      MeshData_MasterSlavesToNodes_Quad2Quad(6,iMasterSlaveNodes_Quad2Quad) = NodeID6

      MeshData_MasterSlavesToElements_Quad2Quad(1,iMasterSlaveNodes_Quad2Quad) = ElemID1
      MeshData_MasterSlavesToElements_Quad2Quad(2,iMasterSlaveNodes_Quad2Quad) = ElemID2
      MeshData_MasterSlavesToElements_Quad2Quad(3,iMasterSlaveNodes_Quad2Quad) = ElemID3

      aMasterSlaveNode => aMasterSlaveNode%NextLinkedListNode
    END DO

    IF (DebugMode .EQV. .TRUE.) THEN
      WRITE(UNIT_SCREEN,*)
      WRITE(UNIT_SCREEN,"(2X,A)") "======================================================="
      WRITE(UNIT_SCREEN,"(2X,A)") "Printing List: MasterSlaveNodesList_Quad2Quad"
      WRITE(UNIT_SCREEN,"(2X,A)") "======================================================="
      WRITE(UNIT_SCREEN,"(3(2X,A8))") "iBucket", "BucketID", "NodeIDs"
      CALL PrintLinkedList(MasterSlaveNodesList_Quad2Quad)
      WRITE(UNIT_SCREEN,*)
      WRITE(UNIT_SCREEN,"(2X,A)") "======================================================="
      WRITE(UNIT_SCREEN,"(2X,A)") "Printing List: MeshData_MasterSlavesToNodes_Quad2Quad"
      WRITE(UNIT_SCREEN,"(2X,A)") "======================================================="
      WRITE(UNIT_SCREEN,"(2(2X,A8))") "iBucket", "ElemIDs"
      FormatString = "(1(2X,I8),2X,6(1X,I0))"
      DO iMasterSlaveNodes_Quad2Quad=1,nMasterSlaveNodes_Quad2Quad
        WRITE(UNIT_SCREEN,FormatString) &
          iMasterSlaveNodes_Quad2Quad, MeshData_MasterSlavesToNodes_Quad2Quad(1:6,iMasterSlaveNodes_Quad2Quad)
      END DO
      WRITE(UNIT_SCREEN,*)
      WRITE(UNIT_SCREEN,*)
      WRITE(UNIT_SCREEN,"(2X,A)") "======================================================="
      WRITE(UNIT_SCREEN,"(2X,A)") "Printing List: MeshData_MasterSlavesToElements_Quad2Quad"
      WRITE(UNIT_SCREEN,"(2X,A)") "======================================================="
      WRITE(UNIT_SCREEN,"(2(2X,A8))") "iBucket", "ElemIDs"
      FormatString = "(1(2X,I8),2X,3(1X,I0))"
      DO iMasterSlaveNodes_Quad2Quad=1,nMasterSlaveNodes_Quad2Quad
        WRITE(UNIT_SCREEN,FormatString) &
          iMasterSlaveNodes_Quad2Quad, MeshData_MasterSlavesToElements_Quad2Quad(1:3,iMasterSlaveNodes_Quad2Quad)
      END DO
      WRITE(UNIT_SCREEN,*)
    END IF
  END IF

  !------------------------------------------------------------!
  ! MeshData_MasterSlavesToElements: Quad4Quad
  !------------------------------------------------------------!
  CALL CountLinkedListNodes(MasterSlaveNodesList_Quad4Quad,nMasterSlaveNodes_Quad4Quad)
  IF (nMasterSlaveNodes_Quad4Quad .GT. 0) THEN
    IF (ALLOCATED(MeshData_MasterSlavesToNodes_Quad4Quad)) THEN
      DEALLOCATE(MeshData_MasterSlavesToNodes_Quad4Quad)
    END IF
    IF (ALLOCATED(MeshData_MasterSlavesToElements_Quad4Quad)) THEN
      DEALLOCATE(MeshData_MasterSlavesToElements_Quad4Quad)
    END IF
    ALLOCATE(MeshData_MasterSlavesToNodes_Quad4Quad(1:9,1:nMasterSlaveNodes_Quad4Quad))
    ALLOCATE(MeshData_MasterSlavesToElements_Quad4Quad(1:5,1:nMasterSlaveNodes_Quad4Quad))

    aMasterSlaveNode => MasterSlaveNodesList_Quad4Quad%FirstLinkedListNode
    DO WHILE(ASSOCIATED(aMasterSlaveNode))
      CALL GetLinkedListNode(aMasterSlaveNode,iMasterSlaveNodes_Quad4Quad,MasterSlaveNodesBucket_Quad4Quad)
      NodeID1 = MasterSlaveNodesBucket_Quad4Quad%Data(1)
      NodeID2 = MasterSlaveNodesBucket_Quad4Quad%Data(2)
      NodeID3 = MasterSlaveNodesBucket_Quad4Quad%Data(3)
      NodeID4 = MasterSlaveNodesBucket_Quad4Quad%Data(4)
      NodeID5 = MasterSlaveNodesBucket_Quad4Quad%Data(5)
      NodeID6 = MasterSlaveNodesBucket_Quad4Quad%Data(6)
      NodeID7 = MasterSlaveNodesBucket_Quad4Quad%Data(7)
      NodeID8 = MasterSlaveNodesBucket_Quad4Quad%Data(8)
      NodeID9 = MasterSlaveNodesBucket_Quad4Quad%Data(9)

      Nodes_QUAD(1:4) = (/NodeID1,NodeID2,NodeID3,NodeID4/)
      IF (CheckFaceIDIsInNodesToFacesMap(Nodes_QUAD) .EQV. .TRUE.) THEN
        FaceID1 = GetFaceIDFromNodesToFacesMap(Nodes_QUAD)
        IndexFaceID1 = GetBucketID(FaceIDToIndexMap,(/FaceID1/))
        ElemID1 = NonConformingSidesArray(IndexFaceID1,3)
      END IF

      Nodes_QUAD(1:4) = (/NodeID1,NodeID5,NodeID9,NodeID8/)
      IF (CheckFaceIDIsInNodesToFacesMap(Nodes_QUAD) .EQV. .TRUE.) THEN
        FaceID2 = GetFaceIDFromNodesToFacesMap(Nodes_QUAD)
        IndexFaceID2 = GetBucketID(FaceIDToIndexMap,(/FaceID2/))
        ElemID2 = NonConformingSidesArray(IndexFaceID2,3)
      END IF

      Nodes_QUAD(1:4) = (/NodeID5,NodeID2,NodeID6,NodeID9/)
      IF (CheckFaceIDIsInNodesToFacesMap(Nodes_QUAD) .EQV. .TRUE.) THEN
        FaceID3 = GetFaceIDFromNodesToFacesMap(Nodes_QUAD)
        IndexFaceID3 = GetBucketID(FaceIDToIndexMap,(/FaceID3/))
        ElemID3 = NonConformingSidesArray(IndexFaceID3,3)
      END IF

      Nodes_QUAD(1:4) = (/NodeID8,NodeID9,NodeID7,NodeID4/)
      IF (CheckFaceIDIsInNodesToFacesMap(Nodes_QUAD) .EQV. .TRUE.) THEN
        FaceID4 = GetFaceIDFromNodesToFacesMap(Nodes_QUAD)
        IndexFaceID4 = GetBucketID(FaceIDToIndexMap,(/FaceID4/))
        ElemID4 = NonConformingSidesArray(IndexFaceID4,3)
      END IF

      Nodes_QUAD(1:4) = (/NodeID9,NodeID6,NodeID3,NodeID7/)
      IF (CheckFaceIDIsInNodesToFacesMap(Nodes_QUAD) .EQV. .TRUE.) THEN
        FaceID5 = GetFaceIDFromNodesToFacesMap(Nodes_QUAD)
        IndexFaceID5 = GetBucketID(FaceIDToIndexMap,(/FaceID5/))
        ElemID5 = NonConformingSidesArray(IndexFaceID5,3)
      END IF

      MeshData_MasterSlavesToNodes_Quad4Quad(1,iMasterSlaveNodes_Quad4Quad) = NodeID1
      MeshData_MasterSlavesToNodes_Quad4Quad(2,iMasterSlaveNodes_Quad4Quad) = NodeID2
      MeshData_MasterSlavesToNodes_Quad4Quad(3,iMasterSlaveNodes_Quad4Quad) = NodeID3
      MeshData_MasterSlavesToNodes_Quad4Quad(4,iMasterSlaveNodes_Quad4Quad) = NodeID4
      MeshData_MasterSlavesToNodes_Quad4Quad(5,iMasterSlaveNodes_Quad4Quad) = NodeID5
      MeshData_MasterSlavesToNodes_Quad4Quad(6,iMasterSlaveNodes_Quad4Quad) = NodeID6
      MeshData_MasterSlavesToNodes_Quad4Quad(7,iMasterSlaveNodes_Quad4Quad) = NodeID7
      MeshData_MasterSlavesToNodes_Quad4Quad(8,iMasterSlaveNodes_Quad4Quad) = NodeID8
      MeshData_MasterSlavesToNodes_Quad4Quad(9,iMasterSlaveNodes_Quad4Quad) = NodeID9

      MeshData_MasterSlavesToElements_Quad4Quad(1,iMasterSlaveNodes_Quad4Quad) = ElemID1
      MeshData_MasterSlavesToElements_Quad4Quad(2,iMasterSlaveNodes_Quad4Quad) = ElemID2
      MeshData_MasterSlavesToElements_Quad4Quad(3,iMasterSlaveNodes_Quad4Quad) = ElemID3
      MeshData_MasterSlavesToElements_Quad4Quad(4,iMasterSlaveNodes_Quad4Quad) = ElemID4
      MeshData_MasterSlavesToElements_Quad4Quad(5,iMasterSlaveNodes_Quad4Quad) = ElemID5

      aMasterSlaveNode => aMasterSlaveNode%NextLinkedListNode
    END DO

    IF (DebugMode .EQV. .TRUE.) THEN
      WRITE(UNIT_SCREEN,*)
      WRITE(UNIT_SCREEN,"(2X,A)") "======================================================="
      WRITE(UNIT_SCREEN,"(2X,A)") "Printing List: MasterSlaveNodesList_Quad4Quad"
      WRITE(UNIT_SCREEN,"(2X,A)") "======================================================="
      WRITE(UNIT_SCREEN,"(3(2X,A8))") "iBucket", "BucketID", "NodeIDs"
      CALL PrintLinkedList(MasterSlaveNodesList_Quad4Quad)
      WRITE(UNIT_SCREEN,*)
      WRITE(UNIT_SCREEN,"(2X,A)") "======================================================="
      WRITE(UNIT_SCREEN,"(2X,A)") "Printing List: MeshData_MasterSlavesToNodes_Quad4Quad"
      WRITE(UNIT_SCREEN,"(2X,A)") "======================================================="
      WRITE(UNIT_SCREEN,"(2(2X,A8))") "iBucket", "NodeIDs"
      FormatString = "(1(2X,I8),2X,9(1X,I0))"
      DO iMasterSlaveNodes_Quad4Quad=1,nMasterSlaveNodes_Quad4Quad
        WRITE(UNIT_SCREEN,FormatString) &
          iMasterSlaveNodes_Quad4Quad, MeshData_MasterSlavesToNodes_Quad4Quad(1:9,iMasterSlaveNodes_Quad4Quad)
      END DO
      WRITE(UNIT_SCREEN,*)
      WRITE(UNIT_SCREEN,*)
      WRITE(UNIT_SCREEN,"(2X,A)") "======================================================="
      WRITE(UNIT_SCREEN,"(2X,A)") "Printing List: MeshData_MasterSlavesToElements_Quad4Quad"
      WRITE(UNIT_SCREEN,"(2X,A)") "======================================================="
      WRITE(UNIT_SCREEN,"(2(2X,A8))") "iBucket", "ElemIDs"
      FormatString = "(1(2X,I8),2X,5(1X,I0))"
      DO iMasterSlaveNodes_Quad4Quad=1,nMasterSlaveNodes_Quad4Quad
        WRITE(UNIT_SCREEN,FormatString) &
          iMasterSlaveNodes_Quad4Quad, MeshData_MasterSlavesToElements_Quad4Quad(1:5,iMasterSlaveNodes_Quad4Quad)
      END DO
      WRITE(UNIT_SCREEN,*)
    END IF
  END IF

END IF

!----------------------------------------------------------------------------------------------------------------------!
END SUBROUTINE CreateArray_NonConformingFacesToNodes
!======================================================================================================================!
!
!
!
!======================================================================================================================!
SUBROUTINE CreateNodesToEdgesHashMaps(Debug)
!----------------------------------------------------------------------------------------------------------------------!
USE MOD_MeshMain_vars,ONLY: NodesToEdgesMap
USE MOD_MeshMain_vars,ONLY: NodesToSharedEdgesMap
!----------------------------------------------------------------------------------------------------------------------!
USE MOD_MeshMain_vars,ONLY: MeshArraysInfo
USE MOD_MeshMain_vars,ONLY: MeshData_EdgesToNodes
!----------------------------------------------------------------------------------------------------------------------!
USE MOD_DataStructures,ONLY: CreateBucket
USE MOD_DataStructures,ONLY: AddDataToBucket
USE MOD_DataStructures,ONLY: AddBucketIDToHashTable
USE MOD_DataStructures,ONLY: PrintHashTable
USE MOD_DataStructures,ONLY: CreateHashTable
!----------------------------------------------------------------------------------------------------------------------!
IMPLICIT NONE
!----------------------------------------------------------------------------------------------------------------------!
! FORMAL ARGUMENTS
!----------------------------------------------------------------------------------------------------------------------!
LOGICAL,INTENT(IN),OPTIONAL :: Debug
!----------------------------------------------------------------------------------------------------------------------!
! LOCAL VARIABLES
!----------------------------------------------------------------------------------------------------------------------!
INTEGER :: iNode
INTEGER :: iEdge
INTEGER :: nNodes
INTEGER :: nEdges
INTEGER :: EdgeID
INTEGER :: NodeID
INTEGER :: nEdgeNodes
LOGICAL :: DebugMode
!----------------------------------------------------------------------------------------------------------------------!
INTEGER,ALLOCATABLE :: EdgeNodes(:)
!----------------------------------------------------------------------------------------------------------------------!

IF (PRESENT(Debug)) THEN
  DebugMode = Debug
ELSE
  DebugMode = .FALSE.
END IF

nNodes = MeshArraysInfo%nNodes
nEdges = MeshArraysInfo%nEdges

!------------------------------------------------------------!
! Constructing NodesToEdgesMap
!------------------------------------------------------------!
nEdgeNodes = 2
IF (ALLOCATED(EdgeNodes) .EQV. .TRUE.) THEN
  DEALLOCATE(EdgeNodes)
END IF
ALLOCATE(EdgeNodes(1:nEdgeNodes))

nEdges = SIZE(MeshData_EdgesToNodes,2)
CALL CreateHashTable(NodesToEdgesMap,nEdges,nEdgeNodes)

DO iEdge=1,nEdges
  EdgeID    = iEdge
  EdgeNodes = MeshData_EdgesToNodes(1:2,iEdge)
  CALL AddBucketIDToHashTable(NodesToEdgesMap,EdgeID,EdgeNodes)
END DO

IF (DebugMode .EQV. .TRUE.) THEN
  WRITE(UNIT_SCREEN,*)
  WRITE(UNIT_SCREEN,"(2X,A)") "======================================================="
  WRITE(UNIT_SCREEN,"(2X,A)") "HashTable of Nodes-to-Edges"
  WRITE(UNIT_SCREEN,"(2X,A)") "======================================================="
  CALL PrintHashTable(NodesToEdgesMap)
END IF

!------------------------------------------------------------!
! Constructing NodesToSharedEdgesMap
!------------------------------------------------------------!
IF (ALLOCATED(NodesToSharedEdgesMap) .EQV. .TRUE.) THEN
  DEALLOCATE(NodesToSharedEdgesMap)
END IF
ALLOCATE(NodesToSharedEdgesMap(1:nNodes))

DO iNode=1,nNodes
  CALL CreateBucket(NodesToSharedEdgesMap(iNode))
END DO

DO iEdge=1,nEdges
  DO iNode=1,nEdgeNodes
    NodeID = MeshData_EdgesToNodes(iNode,iEdge)
    CALL AddDataToBucket(NodesToSharedEdgesMap(NodeID),iEdge)
  END DO
END DO

IF (DebugMode .EQV. .TRUE.) THEN
  WRITE(UNIT_SCREEN,*)
  WRITE(UNIT_SCREEN,"(2X,A)") "======================================================="
  WRITE(UNIT_SCREEN,"(2X,A)") "HashTable of Nodes-to-Shared-Edges"
  WRITE(UNIT_SCREEN,"(2X,A)") "======================================================="
  CALL PrintHashTable(NodesToSharedEdgesMap)
END IF

!----------------------------------------------------------------------------------------------------------------------!
END SUBROUTINE CreateNodesToEdgesHashMaps
!======================================================================================================================!
!
!
!
!======================================================================================================================!
SUBROUTINE CreateNodesToFacesHashMaps(Debug)
!----------------------------------------------------------------------------------------------------------------------!
USE MOD_MeshMain_vars,ONLY: NodesToFacesMap_EDGE
USE MOD_MeshMain_vars,ONLY: NodesToFacesMap_TRI
USE MOD_MeshMain_vars,ONLY: NodesToFacesMap_QUAD
!----------------------------------------------------------------------------------------------------------------------!
! ! ! USE MOD_MeshMain_vars,ONLY: MeshArraysInfo
USE MOD_MeshMain_vars,ONLY: MeshData_FacesToNodes
USE MOD_MeshMain_vars,ONLY: MeshData_FacesElementType
!----------------------------------------------------------------------------------------------------------------------!
USE MOD_MeshMainMethods,ONLY: CheckFaceIDIsInNodesToFacesMap
!----------------------------------------------------------------------------------------------------------------------!
USE MOD_DataStructures,ONLY: CreateBucket
USE MOD_DataStructures,ONLY: AddDataToBucket
USE MOD_DataStructures,ONLY: AddBucketIDToHashTable
USE MOD_DataStructures,ONLY: PrintHashTable
USE MOD_DataStructures,ONLY: CreateHashTable
!----------------------------------------------------------------------------------------------------------------------!
USE MOD_Mesh_CGNS_Definitions
!----------------------------------------------------------------------------------------------------------------------!
IMPLICIT NONE
!----------------------------------------------------------------------------------------------------------------------!
! FORMAL ARGUMENTS
!----------------------------------------------------------------------------------------------------------------------!
LOGICAL,INTENT(IN),OPTIONAL :: Debug
!----------------------------------------------------------------------------------------------------------------------!
! LOCAL VARIABLES
!----------------------------------------------------------------------------------------------------------------------!
INTEGER :: iFace
INTEGER :: FaceID
INTEGER :: nFaces
INTEGER :: FaceType
INTEGER :: nFaceNodes
INTEGER :: nMaxFaceNodes
INTEGER :: nFaces_EDGE
INTEGER :: nFaces_TRI
INTEGER :: nFaces_QUAD
INTEGER :: nFaceNodes_EDGE
INTEGER :: nFaceNodes_TRI
INTEGER :: nFaceNodes_QUAD
LOGICAL :: DebugMode
!----------------------------------------------------------------------------------------------------------------------!
INTEGER,ALLOCATABLE :: FaceNodes(:)
!----------------------------------------------------------------------------------------------------------------------!

IF (PRESENT(Debug)) THEN
  DebugMode = Debug
ELSE
  DebugMode = .FALSE.
END IF

nFaces = SIZE(MeshData_FacesToNodes,2)

nFaces_EDGE = 0
nFaces_TRI  = 0
nFaces_QUAD = 0
DO iFace=1,SIZE(MeshData_FacesToNodes,2)
  FaceType = MeshData_FacesElementType(iFace)
  SELECT CASE(FaceType)
    CASE(ELEMTYPE_EDGE2)
      nFaces_EDGE = nFaces_EDGE+1
    CASE(ELEMTYPE_TRI3)
      nFaces_TRI = nFaces_TRI+1
    CASE(ELEMTYPE_QUAD4)
      nFaces_QUAD = nFaces_QUAD+1
  END SELECT
END DO

IF (nFaces_EDGE .GT. 0) THEN
  CALL GetnElemNodes(ELEMTYPE_EDGE2,nFaceNodes_EDGE)
  CALL CreateHashTable(NodesToFacesMap_EDGE,nFaces_EDGE,nFaceNodes_EDGE)
END IF
IF (nFaces_TRI .GT. 0) THEN
  CALL GetnElemNodes(ELEMTYPE_TRI3,nFaceNodes_TRI)
  CALL CreateHashTable(NodesToFacesMap_TRI,nFaces_TRI,nFaceNodes_TRI)
END IF
IF (nFaces_QUAD .GT. 0) THEN
  CALL GetnElemNodes(ELEMTYPE_QUAD4,nFaceNodes_QUAD)
  CALL CreateHashTable(NodesToFacesMap_QUAD,nFaces_QUAD,nFaceNodes_QUAD)
END IF

! ! ! IF (nFaces_EDGE .GT. 0) THEN
! ! !   CALL PrintAnalyze("nFaces (EDGE2)",FormatNumber(nFaces_EDGE))
! ! ! END IF
! ! ! IF (nFaces_TRI .GT. 0) THEN
! ! !   CALL PrintAnalyze("nFaces (TRI3)",FormatNumber(nFaces_TRI))
! ! ! END IF
! ! ! IF (nFaces_QUAD .GT. 0) THEN
! ! !   CALL PrintAnalyze("nFaces (QUAD4)",FormatNumber(nFaces_QUAD))
! ! ! END IF
! ! ! CALL PrintAnalyze("nFaces (TOTAL)",FormatNumber(nFaces))

!------------------------------------------------------------!
! Constructing NodesToFacesMap
!------------------------------------------------------------!

! For PP_nDims=2, MAX(nMaxFaceNodes)=MAX({2})=2
! For PP_nDims=3, MAX(nMaxFaceNodes)=MAX({3,4})=4
SELECT CASE(PP_nDims)
  CASE(2)
    nMaxFaceNodes = 2
  CASE(3)
    nMaxFaceNodes = 4
END SELECT

DO iFace=1,nFaces
  FaceID   = iFace
  FaceType = MeshData_FacesElementType(iFace)
  CALL GetnElemNodes(FaceType,nFaceNodes)

  IF (ALLOCATED(FaceNodes) .EQV. .TRUE.) THEN
    DEALLOCATE(FaceNodes)
  END IF
  ALLOCATE(FaceNodes(1:nFaceNodes))
  FaceNodes(1:nFaceNodes) = MeshData_FacesToNodes(1:nFaceNodes,iFace)

  SELECT CASE(FaceType)
    CASE(ELEMTYPE_EDGE2)
      CALL AddBucketIDToHashTable(NodesToFacesMap_EDGE,FaceID,FaceNodes)
    CASE(ELEMTYPE_TRI3)
      CALL AddBucketIDToHashTable(NodesToFacesMap_TRI,FaceID,FaceNodes)
    CASE(ELEMTYPE_QUAD4)
      CALL AddBucketIDToHashTable(NodesToFacesMap_QUAD,FaceID,FaceNodes)
  END SELECT
END DO

IF (DebugMode .EQV. .TRUE.) THEN
  WRITE(UNIT_SCREEN,*)
  WRITE(UNIT_SCREEN,"(2X,A)") "======================================================="
  WRITE(UNIT_SCREEN,"(2X,A)") "HashTable of Nodes-to-Faces (EDGE)"
  WRITE(UNIT_SCREEN,"(2X,A)") "======================================================="
  CALL PrintHashTable(NodesToFacesMap_EDGE)
  WRITE(UNIT_SCREEN,*)
  WRITE(UNIT_SCREEN,"(2X,A)") "======================================================="
  WRITE(UNIT_SCREEN,"(2X,A)") "HashTable of Nodes-to-Faces (TRI)"
  WRITE(UNIT_SCREEN,"(2X,A)") "======================================================="
  CALL PrintHashTable(NodesToFacesMap_TRI)
  WRITE(UNIT_SCREEN,*)
  WRITE(UNIT_SCREEN,"(2X,A)") "======================================================="
  WRITE(UNIT_SCREEN,"(2X,A)") "HashTable of Nodes-to-Faces (QUAD)"
  WRITE(UNIT_SCREEN,"(2X,A)") "======================================================="
  CALL PrintHashTable(NodesToFacesMap_QUAD)
END IF

!----------------------------------------------------------------------------------------------------------------------!
END SUBROUTINE CreateNodesToFacesHashMaps
!======================================================================================================================!
!
!
!
!----------------------------------------------------------------------------------------------------------------------!
END MODULE MOD_MeshExportElementsList
!======================================================================================================================!
