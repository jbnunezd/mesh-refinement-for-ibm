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
MODULE MOD_DataExport_TECPLOT
!----------------------------------------------------------------------------------------------------------------------!
USE MOD_GLOBAL_vars
!----------------------------------------------------------------------------------------------------------------------!
IMPLICIT NONE
!----------------------------------------------------------------------------------------------------------------------!
PRIVATE
!----------------------------------------------------------------------------------------------------------------------!
INTEGER,PARAMETER :: ELEMTYPE_EDGE2  = 1
INTEGER,PARAMETER :: ELEMTYPE_TRI3   = 2
INTEGER,PARAMETER :: ELEMTYPE_QUAD4  = 3
INTEGER,PARAMETER :: ELEMTYPE_TETRA4 = 4
INTEGER,PARAMETER :: ELEMTYPE_HEXA8  = 5
INTEGER,PARAMETER :: ELEMTYPE_PRISM6 = 6
INTEGER,PARAMETER :: ELEMTYPE_PYRA5  = 7
!----------------------------------------------------------------------------------------------------------------------!
INTEGER,PARAMETER :: NELEMNODES_EDGE2  = 2
INTEGER,PARAMETER :: NELEMNODES_TRI3   = 3
INTEGER,PARAMETER :: NELEMNODES_QUAD4  = 4
INTEGER,PARAMETER :: NELEMNODES_TETRA4 = 4
INTEGER,PARAMETER :: NELEMNODES_HEXA8  = 8
INTEGER,PARAMETER :: NELEMNODES_PRISM6 = 6
INTEGER,PARAMETER :: NELEMNODES_PYRA5  = 5
!----------------------------------------------------------------------------------------------------------------------!

!----------------------------------------------------------------------------------------------------------------------!
INTERFACE DataExport_TECPLOT_MESH_ASCII
  MODULE PROCEDURE DataExport_TECPLOT_MESH_ASCII
END INTERFACE
!----------------------------------------------------------------------------------------------------------------------!
PUBLIC :: DataExport_TECPLOT_MESH_ASCII
!----------------------------------------------------------------------------------------------------------------------!
CHARACTER(LEN=256) :: ModuleName = "DataExport_TECPLOT"
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
SUBROUTINE DataExport_TECPLOT_MESH_ASCII(&
  FileName,&
  ProjectName,&
  ProgramName,&
  FileVersion,&
  nDims,&
  NGeo,&
  OutputTime,&
  VarNames,&
  ElementsToElementType,&
  ElementsToNodes,&
  ElementsToLevel,&
  ElementsToFlag,&
  NodesCoordinates,&
  BCFacesToNodes,&
  BCFacesToLevel,&
  BCFacesToElementType,&
  BCFacesToMark,&
  BoundaryMark,&
  BoundaryName)
!----------------------------------------------------------------------------------------------------------------------!
IMPLICIT NONE
!----------------------------------------------------------------------------------------------------------------------!
! FORMAL ARGUMENTS
!----------------------------------------------------------------------------------------------------------------------!
CHARACTER(LEN=*),INTENT(IN)   :: FileName
CHARACTER(LEN=*),INTENT(IN)   :: ProjectName
CHARACTER(LEN=*),INTENT(IN)   :: ProgramName
CHARACTER(LEN=*),INTENT(IN)   :: FileVersion
INTEGER,INTENT(IN)            :: nDims
INTEGER,INTENT(IN)            :: NGeo
REAL,INTENT(IN)               :: OutputTime
CHARACTER(LEN=*),INTENT(IN)   :: VarNames(:)
INTEGER,INTENT(IN)            :: ElementsToElementType(:)
INTEGER,INTENT(IN)            :: ElementsToNodes(:,:)
INTEGER,INTENT(IN)            :: ElementsToLevel(:)
INTEGER,INTENT(IN)            :: ElementsToFlag(:)
REAL,INTENT(IN)               :: NodesCoordinates(:,:)
INTEGER,INTENT(IN)            :: BCFacesToNodes(:,:)
INTEGER,INTENT(IN)            :: BCFacesToLevel(:)
INTEGER,INTENT(IN)            :: BCFacesToMark(:)
INTEGER,INTENT(IN)            :: BCFacesToElementType(:)
INTEGER,INTENT(IN)            :: BoundaryMark(:)
CHARACTER(LEN=256),INTENT(IN) :: BoundaryName(:)
!----------------------------------------------------------------------------------------------------------------------!
! LOCAL VARIABLES
!----------------------------------------------------------------------------------------------------------------------!
INTEGER :: nTabIn
INTEGER :: iVar
INTEGER :: iElem
INTEGER :: iNode
INTEGER :: nElems
INTEGER :: nNodes
INTEGER :: NodeID
INTEGER :: StrandID
INTEGER :: Offset
INTEGER :: ElemType
INTEGER :: nOutVars
INTEGER :: nBCFaces
INTEGER :: iBoundary
INTEGER :: nBoundaries
INTEGER :: UNIT_FILE
!----------------------------------------------------------------------------------------------------------------------!
INTEGER :: LastNodeID
INTEGER :: ElemID_EDGE2
INTEGER :: ElemID_TRI3
INTEGER :: ElemID_QUAD4
INTEGER :: ElemID_TETRA4
INTEGER :: ElemID_HEXA8
INTEGER :: ElemID_PRISM6
INTEGER :: ElemID_PYRA5
!----------------------------------------------------------------------------------------------------------------------!
INTEGER :: nElems_EDGE2
INTEGER :: nElems_TRI3
INTEGER :: nElems_QUAD4
INTEGER :: nElems_TETRA4
INTEGER :: nElems_HEXA8
INTEGER :: nElems_PRISM6
INTEGER :: nElems_PYRA5
!----------------------------------------------------------------------------------------------------------------------!
REAL,ALLOCATABLE   :: MeshNodesCoordinates(:,:)
!----------------------------------------------------------------------------------------------------------------------!
CHARACTER(LEN=1)   :: SepStr
CHARACTER(LEN=256) :: StrSolutionTime
CHARACTER(LEN=256) :: StrNodes
CHARACTER(LEN=256) :: StrElems
CHARACTER(LEN=256) :: StrStrandID
CHARACTER(LEN=256) :: StrZoneType
CHARACTER(LEN=256) :: StrZoneTitle
CHARACTER(LEN=256) :: StrDataPacking
!----------------------------------------------------------------------------------------------------------------------!
CHARACTER(LEN=256) :: VarString
CHARACTER(LEN=256) :: FormatString
CHARACTER(LEN=512) :: VariablesNames
CHARACTER(LEN=256) :: FullFileName
CHARACTER(LEN=256) :: FileExtension
!----------------------------------------------------------------------------------------------------------------------!
CHARACTER(LEN=256) :: StrL
CHARACTER(LEN=256) :: StrR
!----------------------------------------------------------------------------------------------------------------------!
CHARACTER(LEN=256) :: ErrorMessage
CHARACTER(LEN=256) :: FunctionName = "DataExport_TECPLOT_ASCII"
!----------------------------------------------------------------------------------------------------------------------!

IF (NGeo .GT. 1) THEN
  ErrorMessage = "NGeo .GT. 1"
  CALL PrintError(__STAMP__,ErrorMessage,ModuleName,FunctionName)
END IF

nOutVars = SIZE(VarNames)

FileExtension = ".dat"
FullFileName  = TRIM(FileName)//TRIM(FileExtension)

nTabIn = 4
StrL = "Writing Mesh"
StrR = TRIM(FullFileName)
CALL PrintAnalyze(StrL,StrR,nTabIn=nTabIn)

VariablesNames = ""
VariablesNames = TRIM(VariablesNames)//'VARIABLES='
Offset = LEN(TRIM(VariablesNames))+1
DO iVar=1,nOutVars
  IF (iVar .NE. nOutVars) THEN
    SepStr = ","
    WRITE(VarString,'(A,A1)') TRIM(VarNames(iVar)), TRIM(SepStr)
  ELSE
    WRITE(VarString,'(A)') TRIM(VarNames(iVar))
  END IF
  VariablesNames(Offset:Offset+LEN(TRIM(VarString))) = TRIM(VarString)
  Offset = Offset + LEN(TRIM(VarString))
END DO

!--------------------------------------------------!
! OPEN THE OUTPUT FILE
!--------------------------------------------------!
OPEN(NEWUNIT=UNIT_FILE,FILE=TRIM(FullFileName),STATUS="REPLACE")

!--------------------------------------------------!
! HEADER
!--------------------------------------------------!
WRITE(UNIT_FILE,"(3(A))") 'TITLE="', TRIM(ProjectName), '"'
WRITE(UNIT_FILE,'(A)') VariablesNames(1:Offset-1)

!--------------------------------------------------!
! TECPLOT ZoneType
!--------------------------------------------------!
! 0: ORDERED
! 1: FELINESEG
! 2: FETRIANGLE
! 3: FEQUADRILATERAL
! 4: FETETRAHEDRON
! 5: FEBRICK
! 6: FEPOLYGON
! 7: FEPOLYHEDRON
!--------------------------------------------------!

!--------------------------------------------------!
! Mesh Elements
!--------------------------------------------------!
SELECT CASE(nDims)
  CASE(2)
    !--------------------------------------------------!
    ! For nDims=2, elements available
    !--------------------------------------------------!
    ! ElemType = ELEMTYPE_TRI3
    ! ElemType = ELEMTYPE_QUAD4
    !--------------------------------------------------!
    nElems_TRI3  = 0
    nElems_QUAD4 = 0

    DO iElem=1,SIZE(ElementsToElementType,1)
      ElemType = ElementsToElementType(iElem)
      SELECT CASE(ElemType)
        CASE(ELEMTYPE_TRI3)
          nElems_TRI3 = nElems_TRI3+1
        CASE(ELEMTYPE_QUAD4)
          nElems_QUAD4 = nElems_QUAD4+1
      END SELECT
    END DO

    nElems = 0
    IF (nElems_TRI3 .GT. 0) THEN
      nElems = nElems + nElems_TRI3
    END IF
    IF (nElems_QUAD4 .GT. 0) THEN
      nElems = nElems + nElems_QUAD4
    END IF

    nNodes = 0
    IF (nElems_TRI3 .GT. 0) THEN
      nNodes = nNodes + nElems_TRI3*NELEMNODES_TRI3
    END IF
    IF (nElems_QUAD4 .GT. 0) THEN
      nNodes = nNodes + nElems_QUAD4*NELEMNODES_QUAD4
    END IF

    StrandID = 0
    StrandID = StrandID+1

    StrZoneTitle   = "Domain"
    StrZoneType    = "FEQUADRILATERAL"
    StrDataPacking = "POINT"
    WRITE(StrNodes,'(I0)') nNodes
    WRITE(StrElems,'(I0)') nElems
    WRITE(StrSolutionTime,'(ES18.12E2)') OutputTime
    WRITE(StrStrandID,'(I0)') StrandID

    WRITE(UNIT_FILE,"(2(A))") 'ZONE T=',      TRIM(StrZoneTitle)
    WRITE(UNIT_FILE,"(2(A))") 'ZONETYPE=',    TRIM(StrZoneType)
    WRITE(UNIT_FILE,"(2(A))") 'DATAPACKING=', TRIM(StrDataPacking)
    WRITE(UNIT_FILE,"(2(A))") 'SOLUTIONTIME=',TRIM(StrSolutionTime)
    WRITE(UNIT_FILE,"(2(A))") 'STRANDID=',    TRIM(StrStrandID)
    WRITE(UNIT_FILE,"(2(A))") 'NODES=',       TRIM(StrNodes)
    WRITE(UNIT_FILE,"(2(A))") 'ELEMENTS=',    TRIM(StrElems)

    ! Mesh Nodes (Written according to DATAPACKING=POINT)
    WRITE(FormatString,'(A,I0,A)') "(", nOutVars, "(SP,ES19.12E2,1X))"
    IF (ALLOCATED(MeshNodesCoordinates)) THEN
      DEALLOCATE(MeshNodesCoordinates)
    END IF
    ALLOCATE(MeshNodesCoordinates(1:nOutVars,1:nNodes))

    NodeID = 0
    ! ELEMTYPE_TRI3
    IF (nElems_TRI3 .GT. 0) THEN
      DO iElem=1,nElems
        SELECT CASE(ElementsToElementType(iElem))
          CASE(ELEMTYPE_TRI3)
            DO iNode=1,NELEMNODES_TRI3
              NodeID = NodeID+1
              MeshNodesCoordinates(1:nDims,NodeID) = NodesCoordinates(1:nDims,ElementsToNodes(iNode,iElem))
              MeshNodesCoordinates(nDims+1,NodeID) = REAL(ElementsToLevel(iElem))
              MeshNodesCoordinates(nDims+2,NodeID) = REAL(ElementsToFlag(iElem))
            END DO
        END SELECT
      END DO
    END IF

    ! ELEMTYPE_QUAD4
    IF (nElems_QUAD4 .GT. 0) THEN
      DO iElem=1,nElems
        SELECT CASE(ElementsToElementType(iElem))
          CASE(ELEMTYPE_QUAD4)
            DO iNode=1,NELEMNODES_QUAD4
              NodeID = NodeID+1
              MeshNodesCoordinates(1:nDims,NodeID) = NodesCoordinates(1:nDims,ElementsToNodes(iNode,iElem))
              MeshNodesCoordinates(nDims+1,NodeID) = REAL(ElementsToLevel(iElem))
              MeshNodesCoordinates(nDims+2,NodeID) = REAL(ElementsToFlag(iElem))
            END DO
        END SELECT
      END DO
    END IF
    
    DO iNode=1,nNodes
      WRITE(UNIT_FILE,FormatString) MeshNodesCoordinates(1:nOutVars,iNode)
    END DO

    IF (ALLOCATED(MeshNodesCoordinates)) THEN
      DEALLOCATE(MeshNodesCoordinates)
    END IF
    
    ! Mesh Connectivity
    FormatString = "(4(I0,1X))"
    NodeID     = 0
    LastNodeID = 0

    ! ELEMTYPE_TRI3
    ElemID_TRI3 = 0
    DO iElem=0,nElems-1
      SELECT CASE(ElementsToElementType(iElem+1))
        CASE(ELEMTYPE_TRI3)
          NodeID = ElemID_TRI3*NELEMNODES_TRI3 + LastNodeID
          DO iNode=1,NGeo
            WRITE(UNIT_FILE,FormatString) &
              NodeID+iNode+0, & !P1
              NodeID+iNode+1, & !P2
              NodeID+iNode+2, & !P3
              NodeID+iNode+2    !P4
          END DO
          ElemID_TRI3 = ElemID_TRI3+1
      END SELECT
    END DO
    IF (ElemID_TRI3 .GT. 0) THEN
      LastNodeID = LastNodeID + ElemID_TRI3*NELEMNODES_TRI3
    END IF

    ! ELEMTYPE_QUAD4
    ElemID_QUAD4 = 0
    DO iElem=0,nElems-1
      SELECT CASE(ElementsToElementType(iElem+1))
        CASE(ELEMTYPE_QUAD4)
          NodeID = ElemID_QUAD4*NELEMNODES_QUAD4 + LastNodeID
          DO iNode=1,NGeo
            WRITE(UNIT_FILE,FormatString) &
              NodeID+iNode+0, & !P1
              NodeID+iNode+1, & !P2
              NodeID+iNode+2, & !P3
              NodeID+iNode+3    !P4
          END DO
          ElemID_QUAD4 = ElemID_QUAD4+1
      END SELECT
    END DO
    IF (ElemID_QUAD4 .GT. 0) THEN
      LastNodeID = LastNodeID + ElemID_QUAD4*NELEMNODES_QUAD4
    END IF

  CASE (3)
    !--------------------------------------------------!
    ! For nDims=3, elements available
    !--------------------------------------------------!
    ! ElemType = ELEMTYPE_TETRA4
    ! ElemType = ELEMTYPE_HEXA8
    ! ElemType = ELEMTYPE_PRISM6
    ! ElemType = ELEMTYPE_PYRA5
    !--------------------------------------------------!
    nElems_TETRA4 = 0
    nElems_HEXA8  = 0
    nElems_PRISM6 = 0
    nElems_PYRA5  = 0

    DO iElem=1,SIZE(ElementsToElementType,1)
      ElemType = ElementsToElementType(iElem)
      SELECT CASE(ElemType)
        CASE(ELEMTYPE_TETRA4)
          nElems_TETRA4 = nElems_TETRA4+1
        CASE(ELEMTYPE_HEXA8)
          nElems_HEXA8 = nElems_HEXA8+1
        CASE(ELEMTYPE_PRISM6)
          nElems_PRISM6 = nElems_PRISM6+1
        CASE(ELEMTYPE_PYRA5)
          nElems_PYRA5 = nElems_PYRA5+1
      END SELECT
    END DO

    nElems = 0
    IF (nElems_TETRA4 .GT. 0) THEN
      nElems = nElems + nElems_TETRA4
    END IF
    IF (nElems_HEXA8 .GT. 0) THEN
      nElems = nElems + nElems_HEXA8
    END IF
    IF (nElems_PRISM6 .GT. 0) THEN
      nElems = nElems + nElems_PRISM6
    END IF
    IF (nElems_PYRA5 .GT. 0) THEN
      nElems = nElems + nElems_PYRA5
    END IF

    nNodes = 0
    IF (nElems_TETRA4 .GT. 0) THEN
      nNodes = nNodes + nElems_TETRA4*NELEMNODES_TETRA4
    END IF
    IF (nElems_HEXA8 .GT. 0) THEN
      nNodes = nNodes + nElems_HEXA8*NELEMNODES_HEXA8
    END IF
    IF (nElems_PRISM6 .GT. 0) THEN
      nNodes = nNodes + nElems_PRISM6*NELEMNODES_PRISM6
    END IF
    IF (nElems_PYRA5 .GT. 0) THEN
      nNodes = nNodes + nElems_PYRA5*NELEMNODES_PYRA5
    END IF

    StrandID = 0
    StrandID = StrandID+1

    StrZoneTitle   = "Domain"
    StrZoneType    = "FEBRICK"
    StrDataPacking = "POINT"
    WRITE(StrNodes,'(I0)') nNodes
    WRITE(StrElems,'(I0)') nElems
    WRITE(StrSolutionTime,'(ES18.12E2)') OutputTime
    WRITE(StrStrandID,'(I0)') StrandID

    WRITE(UNIT_FILE,"(2(A))") 'ZONE T=',      TRIM(StrZoneTitle)
    WRITE(UNIT_FILE,"(2(A))") 'ZONETYPE=',    TRIM(StrZoneType)
    WRITE(UNIT_FILE,"(2(A))") 'DATAPACKING=', TRIM(StrDataPacking)
    WRITE(UNIT_FILE,"(2(A))") 'SOLUTIONTIME=',TRIM(StrSolutionTime)
    WRITE(UNIT_FILE,"(2(A))") 'STRANDID=',    TRIM(StrStrandID)
    WRITE(UNIT_FILE,"(2(A))") 'NODES=',       TRIM(StrNodes)
    WRITE(UNIT_FILE,"(2(A))") 'ELEMENTS=',    TRIM(StrElems)

    ! Mesh Nodes (Written according to DATAPACKING=POINT)
    WRITE(FormatString,'(A,I0,A)') "(", nOutVars, "(SP,ES19.12E2,1X))"
    
    IF (ALLOCATED(MeshNodesCoordinates)) THEN
      DEALLOCATE(MeshNodesCoordinates)
    END IF
    ALLOCATE(MeshNodesCoordinates(1:nOutVars,1:nNodes))
    
    NodeID = 0
    ! ELEMTYPE_TETRA4
    IF (nElems_TETRA4 .GT. 0) THEN
      DO iElem=1,nElems
        SELECT CASE(ElementsToElementType(iElem))
          CASE(ELEMTYPE_TETRA4)
            DO iNode=1,NELEMNODES_TETRA4
              NodeID = NodeID+1
              MeshNodesCoordinates(1:nDims,NodeID) = NodesCoordinates(1:nDims,ElementsToNodes(iNode,iElem))
              MeshNodesCoordinates(nDims+1,NodeID) = REAL(ElementsToLevel(iElem))
              MeshNodesCoordinates(nDims+2,NodeID) = REAL(ElementsToFlag(iElem))
            END DO
        END SELECT
      END DO
    END IF

    ! ELEMTYPE_HEXA8
    IF (nElems_HEXA8 .GT. 0) THEN
      DO iElem=1,nElems
        SELECT CASE(ElementsToElementType(iElem))
          CASE(ELEMTYPE_HEXA8)
            DO iNode=1,NELEMNODES_HEXA8
              NodeID = NodeID+1
              MeshNodesCoordinates(1:nDims,NodeID) = NodesCoordinates(1:nDims,ElementsToNodes(iNode,iElem))
              MeshNodesCoordinates(nDims+1,NodeID) = REAL(ElementsToLevel(iElem))
              MeshNodesCoordinates(nDims+2,NodeID) = REAL(ElementsToFlag(iElem))
            END DO
        END SELECT
      END DO
    END IF

    ! ELEMTYPE_PRISM6
    IF (nElems_PRISM6 .GT. 0) THEN
      DO iElem=1,nElems
        SELECT CASE(ElementsToElementType(iElem))
          CASE(ELEMTYPE_PRISM6)
            DO iNode=1,NELEMNODES_PRISM6
              NodeID = NodeID+1
              MeshNodesCoordinates(1:nDims,NodeID) = NodesCoordinates(1:nDims,ElementsToNodes(iNode,iElem))
              MeshNodesCoordinates(nDims+1,NodeID) = REAL(ElementsToLevel(iElem))
              MeshNodesCoordinates(nDims+2,NodeID) = REAL(ElementsToFlag(iElem))
            END DO
        END SELECT
      END DO
    END IF

    ! ELEMTYPE_PYRA5
    IF (nElems_PYRA5 .GT. 0) THEN
      DO iElem=1,nElems
        SELECT CASE(ElementsToElementType(iElem))
          CASE(ELEMTYPE_PYRA5)
            DO iNode=1,NELEMNODES_PYRA5
              NodeID = NodeID+1
              MeshNodesCoordinates(1:nDims,NodeID) = NodesCoordinates(1:nDims,ElementsToNodes(iNode,iElem))
              MeshNodesCoordinates(nDims+1,NodeID) = REAL(ElementsToLevel(iElem))
              MeshNodesCoordinates(nDims+2,NodeID) = REAL(ElementsToFlag(iElem))
            END DO
        END SELECT
      END DO
    END IF
    
    DO iNode=1,nNodes
      WRITE(UNIT_FILE,FormatString) MeshNodesCoordinates(1:nOutVars,iNode)
    END DO
    
    IF (ALLOCATED(MeshNodesCoordinates)) THEN
      DEALLOCATE(MeshNodesCoordinates)
    END IF

    ! Mesh Connectivity
    FormatString = "(8(I0,1X))"
    NodeID     = 0
    LastNodeID = 0

    ! ELEMTYPE_TETRA4
    ElemID_TETRA4 = 0
    DO iElem=0,nElems-1
      SELECT CASE(ElementsToElementType(iElem+1))
        CASE(ELEMTYPE_TETRA4)
          NodeID = ElemID_TETRA4*NELEMNODES_TETRA4 + LastNodeID
          DO iNode=1,NGeo
            WRITE(UNIT_FILE,FormatString) &
              NodeID+iNode+0, & !P1
              NodeID+iNode+1, & !P2
              NodeID+iNode+2, & !P3
              NodeID+iNode+2, & !P4
              NodeID+iNode+3, & !P5
              NodeID+iNode+3, & !P6
              NodeID+iNode+3, & !P7
              NodeID+iNode+3    !P8
          END DO
          ElemID_TETRA4 = ElemID_TETRA4+1
      END SELECT
    END DO
    IF (ElemID_TETRA4 .GT. 0) THEN
      LastNodeID = LastNodeID + ElemID_TETRA4*NELEMNODES_TETRA4
    END IF

    ! ELEMTYPE_HEXA8
    ElemID_HEXA8 = 0
    DO iElem=0,nElems-1
      SELECT CASE(ElementsToElementType(iElem+1))
        CASE(ELEMTYPE_HEXA8)
          NodeID = ElemID_HEXA8*NELEMNODES_HEXA8 + LastNodeID
          DO iNode=1,NGeo
            WRITE(UNIT_FILE,FormatString) &
              NodeID+iNode+0, & !P1
              NodeID+iNode+1, & !P2
              NodeID+iNode+2, & !P3
              NodeID+iNode+3, & !P4
              NodeID+iNode+4, & !P5
              NodeID+iNode+5, & !P6
              NodeID+iNode+6, & !P7
              NodeID+iNode+7    !P8
          END DO
          ElemID_HEXA8 = ElemID_HEXA8+1
      END SELECT
    END DO
    IF (ElemID_HEXA8 .GT. 0) THEN
      LastNodeID = LastNodeID + ElemID_HEXA8*NELEMNODES_HEXA8
    END IF

    ! ELEMTYPE_PRISM6
    ElemID_PRISM6 = 0
    DO iElem=0,nElems-1
      SELECT CASE(ElementsToElementType(iElem+1))
        CASE(ELEMTYPE_PRISM6)
          NodeID = ElemID_PRISM6*NELEMNODES_PRISM6 + LastNodeID
          DO iNode=1,NGeo
            WRITE(UNIT_FILE,FormatString) &
              NodeID+iNode+0, & !P1
              NodeID+iNode+1, & !P2
              NodeID+iNode+2, & !P3
              NodeID+iNode+2, & !P4
              NodeID+iNode+3, & !P5
              NodeID+iNode+4, & !P6
              NodeID+iNode+5, & !P7
              NodeID+iNode+5    !P8
          END DO
          ElemID_PRISM6 = ElemID_PRISM6+1
      END SELECT
    END DO
    IF (ElemID_PRISM6 .GT. 0) THEN
      LastNodeID = LastNodeID + ElemID_PRISM6*NELEMNODES_PRISM6
    END IF

    ! ELEMTYPE_PYRA5
    ElemID_PYRA5 = 0
    DO iElem=0,nElems-1
      SELECT CASE(ElementsToElementType(iElem+1))
        CASE(ELEMTYPE_PYRA5)
          NodeID = ElemID_PYRA5*NELEMNODES_PYRA5 + LastNodeID
          DO iNode=1,NGeo
            WRITE(UNIT_FILE,FormatString) &
              NodeID+iNode+0, & !P1
              NodeID+iNode+1, & !P2
              NodeID+iNode+2, & !P3
              NodeID+iNode+3, & !P4
              NodeID+iNode+4, & !P5
              NodeID+iNode+4, & !P6
              NodeID+iNode+4, & !P7
              NodeID+iNode+4    !P8
          END DO
          ElemID_PYRA5 = ElemID_PYRA5+1
      END SELECT
    END DO
    IF (ElemID_PYRA5 .GT. 0) THEN
      LastNodeID = LastNodeID + ElemID_PYRA5*NELEMNODES_PYRA5
    END IF

END SELECT

!--------------------------------------------------!
! Mesh Boundaries
!--------------------------------------------------!
SELECT CASE(nDims)
  CASE(2)
    !--------------------------------------------------!
    ! For nDims=2, elements available
    !--------------------------------------------------!
    ! ElemType = ELEMTYPE_EDGE2
    !--------------------------------------------------!

    nBCFaces    = SIZE(BCFacesToNodes,2)
    nBoundaries = SIZE(BoundaryName,1)

    DO iBoundary=1,nBoundaries
      nElems_EDGE2 = 0

      DO iElem=1,nBCFaces
        IF (BCFacesToMark(iElem) .EQ. BoundaryMark(iBoundary)) THEN
          ElemType = BCFacesToElementType(iElem)
          SELECT CASE(ElemType)
            CASE(ELEMTYPE_EDGE2)
              nElems_EDGE2 = nElems_EDGE2+1
          END SELECT
        END IF
      END DO

      nElems = 0
      IF (nElems_EDGE2 .GT. 0) THEN
        nElems = nElems + nElems_EDGE2
      END IF

      nNodes = 0
      IF (nElems_EDGE2 .GT. 0) THEN
        nNodes = nNodes + nElems_EDGE2*NELEMNODES_EDGE2
      END IF

      StrandID = StrandID+1

      StrZoneTitle   = "BC"//TRIM(BoundaryName(iBoundary))
      StrZoneType    = "FELINESEG"
      StrDataPacking = "POINT"
      WRITE(StrNodes,'(I0)') nNodes
      WRITE(StrElems,'(I0)') nElems
      WRITE(StrSolutionTime,'(ES18.12E2)') OutputTime
      WRITE(StrStrandID,'(I0)') StrandID

      WRITE(UNIT_FILE,"(2(A))") 'ZONE T=',      TRIM(StrZoneTitle)
      WRITE(UNIT_FILE,"(2(A))") 'ZONETYPE=',    TRIM(StrZoneType)
      WRITE(UNIT_FILE,"(2(A))") 'DATAPACKING=', TRIM(StrDataPacking)
      WRITE(UNIT_FILE,"(2(A))") 'SOLUTIONTIME=',TRIM(StrSolutionTime)
      WRITE(UNIT_FILE,"(2(A))") 'STRANDID=',    TRIM(StrStrandID)
      WRITE(UNIT_FILE,"(2(A))") 'NODES=',       TRIM(StrNodes)
      WRITE(UNIT_FILE,"(2(A))") 'ELEMENTS=',    TRIM(StrElems)

      ! Mesh Nodes (Written according to DATAPACKING=POINT)
      WRITE(FormatString,'(A,I0,A)') "(", nOutVars, "(SP,ES19.12E2,1X))"

      IF (ALLOCATED(MeshNodesCoordinates)) THEN
        DEALLOCATE(MeshNodesCoordinates)
      END IF
      ALLOCATE(MeshNodesCoordinates(1:nOutVars,1:nNodes))

      NodeID = 0

      ! ELEMTYPE_EDGE2
      DO iElem=1,nBCFaces
        IF (BCFacesToMark(iElem) .EQ. BoundaryMark(iBoundary)) THEN
          SELECT CASE(BCFacesToElementType(iElem))
            CASE(ELEMTYPE_EDGE2)
              DO iNode=1,NELEMNODES_EDGE2
                NodeID = NodeID+1
                MeshNodesCoordinates(1:nDims,NodeID) = NodesCoordinates(1:nDims,BCFacesToNodes(iNode,iElem))
                MeshNodesCoordinates(nDims+1,NodeID) = REAL(BCFacesToLevel(iElem))
                MeshNodesCoordinates(nDims+2,NodeID) = 0.0
              END DO
          END SELECT
        END IF
      END DO

      DO iNode=1,nNodes
        WRITE(UNIT_FILE,FormatString) MeshNodesCoordinates(1:nOutVars,iNode)
      END DO

      IF (ALLOCATED(MeshNodesCoordinates)) THEN
        DEALLOCATE(MeshNodesCoordinates)
      END IF

      ! Mesh Connectivity
      FormatString = "(2(I0,1X))"
      NodeID     = 0
      LastNodeID = 0

      ! ELEMTYPE_EDGE2
      ElemID_EDGE2 = 0
      DO iElem=0,nBCFaces-1
        IF (BCFacesToMark(iElem+1) .EQ. BoundaryMark(iBoundary)) THEN
          SELECT CASE(BCFacesToElementType(iElem+1))
            CASE(ELEMTYPE_EDGE2)
              NodeID = ElemID_EDGE2*NELEMNODES_EDGE2 + LastNodeID
              DO iNode=1,NGeo
                WRITE(UNIT_FILE,FormatString) &
                  NodeID+iNode+0, & !P1
                  NodeID+iNode+1    !P2
              END DO
              ElemID_EDGE2 = ElemID_EDGE2+1
          END SELECT
        END IF
      END DO
      IF (ElemID_EDGE2 .GT. 0) THEN
        LastNodeID = LastNodeID + ElemID_EDGE2*NELEMNODES_EDGE2
      END IF
    END DO
  CASE(3)
    !--------------------------------------------------!
    ! For nDims=3, elements available
    !--------------------------------------------------!
    ! ElemType = ELEMTYPE_TRI3
    ! ElemType = ELEMTYPE_QUAD4
    !--------------------------------------------------!

    nBCFaces    = SIZE(BCFacesToNodes,2)
    nBoundaries = SIZE(BoundaryName,1)

    DO iBoundary=1,nBoundaries
      nElems_TRI3  = 0
      nElems_QUAD4 = 0

      DO iElem=1,nBCFaces
        IF (BCFacesToMark(iElem) .EQ. BoundaryMark(iBoundary)) THEN
          ElemType = BCFacesToElementType(iElem)
          SELECT CASE(ElemType)
            CASE(ELEMTYPE_TRI3)
              nElems_TRI3 = nElems_TRI3+1
            CASE(ELEMTYPE_QUAD4)
              nElems_QUAD4 = nElems_QUAD4+1
          END SELECT
        END IF
      END DO

      nElems = 0
      IF (nElems_TRI3 .GT. 0) THEN
        nElems = nElems + nElems_TRI3
      END IF
      IF (nElems_QUAD4 .GT. 0) THEN
        nElems = nElems + nElems_QUAD4
      END IF

      nNodes = 0
      IF (nElems_TRI3 .GT. 0) THEN
        nNodes = nNodes + nElems_TRI3*NELEMNODES_TRI3
      END IF
      IF (nElems_QUAD4 .GT. 0) THEN
        nNodes = nNodes + nElems_QUAD4*NELEMNODES_QUAD4
      END IF

      StrandID = StrandID+1

      StrZoneTitle   = "BC"//TRIM(BoundaryName(iBoundary))
      StrZoneType    = "FEQUADRILATERAL"
      StrDataPacking = "POINT"
      WRITE(StrNodes,'(I0)') nNodes
      WRITE(StrElems,'(I0)') nElems
      WRITE(StrSolutionTime,'(ES18.12E2)') OutputTime
      WRITE(StrStrandID,'(I0)') StrandID

      WRITE(UNIT_FILE,"(2(A))") 'ZONE T=',      TRIM(StrZoneTitle)
      WRITE(UNIT_FILE,"(2(A))") 'ZONETYPE=',    TRIM(StrZoneType)
      WRITE(UNIT_FILE,"(2(A))") 'DATAPACKING=', TRIM(StrDataPacking)
      WRITE(UNIT_FILE,"(2(A))") 'SOLUTIONTIME=',TRIM(StrSolutionTime)
      WRITE(UNIT_FILE,"(2(A))") 'STRANDID=',    TRIM(StrStrandID)
      WRITE(UNIT_FILE,"(2(A))") 'NODES=',       TRIM(StrNodes)
      WRITE(UNIT_FILE,"(2(A))") 'ELEMENTS=',    TRIM(StrElems)

      ! Mesh Nodes (Written according to DATAPACKING=POINT)
      WRITE(FormatString,'(A,I0,A)') "(", nOutVars, "(SP,ES19.12E2,1X))"

      IF (ALLOCATED(MeshNodesCoordinates)) THEN
        DEALLOCATE(MeshNodesCoordinates)
      END IF
      ALLOCATE(MeshNodesCoordinates(1:nOutVars,1:nNodes))

      NodeID = 0

      ! ELEMTYPE_TRI3
      DO iElem=1,nBCFaces
        IF (BCFacesToMark(iElem) .EQ. BoundaryMark(iBoundary)) THEN
          SELECT CASE(BCFacesToElementType(iElem))
            CASE(ELEMTYPE_TRI3)
              DO iNode=1,NELEMNODES_TRI3
                NodeID = NodeID+1
                MeshNodesCoordinates(1:nDims,NodeID) = NodesCoordinates(1:nDims,BCFacesToNodes(iNode,iElem))
                MeshNodesCoordinates(nDims+1,NodeID) = REAL(BCFacesToLevel(iElem))
                MeshNodesCoordinates(nDims+2,NodeID) = 0.0
              END DO
          END SELECT
        END IF
      END DO

      ! ELEMTYPE_QUAD4
      DO iElem=1,nBCFaces
        IF (BCFacesToMark(iElem) .EQ. BoundaryMark(iBoundary)) THEN
          SELECT CASE(BCFacesToElementType(iElem))
            CASE(ELEMTYPE_QUAD4)
              DO iNode=1,NELEMNODES_QUAD4
                NodeID = NodeID+1
                MeshNodesCoordinates(1:nDims,NodeID) = NodesCoordinates(1:nDims,BCFacesToNodes(iNode,iElem))
                MeshNodesCoordinates(nDims+1,NodeID) = REAL(BCFacesToLevel(iElem))
                MeshNodesCoordinates(nDims+2,NodeID) = 0.0
              END DO
          END SELECT
        END IF
      END DO

      DO iNode=1,nNodes
        WRITE(UNIT_FILE,FormatString) MeshNodesCoordinates(1:nOutVars,iNode)
      END DO

      IF (ALLOCATED(MeshNodesCoordinates)) THEN
        DEALLOCATE(MeshNodesCoordinates)
      END IF

      ! Mesh Connectivity
      FormatString = "(4(I0,1X))"
      NodeID     = 0
      LastNodeID = 0

      ! ELEMTYPE_TRI3
      ElemID_TRI3 = 0
      DO iElem=0,nBCFaces-1
        IF (BCFacesToMark(iElem+1) .EQ. BoundaryMark(iBoundary)) THEN
          SELECT CASE(BCFacesToElementType(iElem+1))
            CASE(ELEMTYPE_TRI3)
              NodeID = ElemID_TRI3*NELEMNODES_TRI3 + LastNodeID
              DO iNode=1,NGeo
                WRITE(UNIT_FILE,FormatString) &
                  NodeID+iNode+0, & !P1
                  NodeID+iNode+1, & !P2
                  NodeID+iNode+2, & !P3
                  NodeID+iNode+2    !P4
              END DO
              ElemID_TRI3 = ElemID_TRI3+1
          END SELECT
        END IF
      END DO
      IF (ElemID_TRI3 .GT. 0) THEN
        LastNodeID = LastNodeID + ElemID_TRI3*NELEMNODES_TRI3
      END IF

      ! ELEMTYPE_QUAD4
      ElemID_QUAD4 = 0
      DO iElem=0,nBCFaces-1
        IF (BCFacesToMark(iElem+1) .EQ. BoundaryMark(iBoundary)) THEN
          SELECT CASE(BCFacesToElementType(iElem+1))
            CASE(ELEMTYPE_QUAD4)
              NodeID = ElemID_QUAD4*NELEMNODES_QUAD4 + LastNodeID
              DO iNode=1,NGeo
                WRITE(UNIT_FILE,FormatString) &
                  NodeID+iNode+0, & !P1
                  NodeID+iNode+1, & !P2
                  NodeID+iNode+2, & !P3
                  NodeID+iNode+3    !P4
              END DO
              ElemID_QUAD4 = ElemID_QUAD4+1
          END SELECT
        END IF
      END DO
      IF (ElemID_QUAD4 .GT. 0) THEN
        LastNodeID = LastNodeID + ElemID_QUAD4*NELEMNODES_QUAD4
      END IF
    END DO
END SELECT

CLOSE(UNIT_FILE)

!----------------------------------------------------------------------------------------------------------------------!
END SUBROUTINE DataExport_TECPLOT_MESH_ASCII
!======================================================================================================================!
!
!
!
!======================================================================================================================!
END MODULE MOD_DataExport_TECPLOT
!======================================================================================================================!
