!     Last change:  MLE   6 Aug 98    9:26 am
      MODULE FILLSTYLES
      INTEGER, PARAMETER :: FILLTODEAD    = -12535
      INTEGER, PARAMETER :: FILLTOMIN     = -12536
      INTEGER, PARAMETER :: FILLTOAVERAGE = -12537
      INTEGER, PARAMETER :: FILLTOMODE    = -12538
      INTEGER, PARAMETER :: FILLTOMAX     = -12539
      INTEGER, PARAMETER :: FILLTOREVERSE = -12540
      INTEGER, PARAMETER :: FILLTOTILE    = -12541
      INTEGER, PARAMETER :: FILLNOTDEFINED= -12542
      INTEGER, PARAMETER :: FILLMINUS  = -12635
      INTEGER, PARAMETER :: FILLEQUAL  = -12636
      INTEGER, PARAMETER :: FILLPLUS   = -12637
      END MODULE

      MODULE TRANSFORMTYPES
      INTEGER, PARAMETER :: ROTATECCLOCK   = -12535
      INTEGER, PARAMETER :: ROTATECLOCK    = -12536
      INTEGER, PARAMETER :: FLIPVERT       = -12537
      INTEGER, PARAMETER :: FLIPHORIZ      = -12538
      END MODULE

      MODULE GameOfLife
      IMPLICIT NONE
      TYPE LifeRules
        INTEGER, DIMENSION(24) :: RuleSet
        REAL, DIMENSION(8) :: BoundryRuleSet
      END TYPE LifeRules
      TYPE LifeOptions
! GridState.EQ.0 when the grid is NOT Displayed, otherwise grid
! is displayed in that Linetype
        INTEGER GridState
! ShowCellsAs.EQ.-1 when cell color is dependant on age, otherwise
! it indicates the age to show all cells as
        INTEGER ShowCellsAs
! ShowCounter.EQ.0 when the counter dialog is NOT displayed
        INTEGER ShowCounter
! ClearBoardTo = the cell age to clear board to
        INTEGER ClearBoardTo
! AllowPatDelete.EQ.0 when the user is to be prevented from deleting
! any patterns from the list
        INTEGER AllowPatDelete
! ProtectOriginal.NE.0 when the original xxxx patterns are to be
! preserved, even though others may be deleted.
        INTEGER ProtectOriginal
! FloatingHelp.NE.0 when the help dialogs are allowed to stay up
! while the user proceeds to do other stuff
        INTEGER FloatingHelp
        CHARACTER (LEN=255) :: SearchDir
        CHARACTER (LEN=255) :: PatternFileName
        CHARACTER (LEN=255) :: OtherFileName
      END TYPE LifeOptions
      INTEGER, PARAMETER :: OptionNotSet          = -12346
      INTEGER, PARAMETER :: UnknownOption         = -12347
      INTEGER, PARAMETER :: GridStateOption       = 1001
      INTEGER, PARAMETER :: ShowCellsAsOption     = 1002
      INTEGER, PARAMETER :: ShowCounterOption     = 1003
      INTEGER, PARAMETER :: ClearBoardToOption    = 1004
      INTEGER, PARAMETER :: AllowPatDeleteOption  = 1005
      INTEGER, PARAMETER :: ProtectOriginalOption = 1006
      INTEGER, PARAMETER :: FloatingHelpOption    = 1007
      TYPE(LifeOptions) :: PrimaryOptions
      TYPE(LifeRules) :: PrimaryRules
      CONTAINS
        INTEGER FUNCTION GetLifeOption(WhichOption)
        IMPLICIT NONE
        INTEGER :: WhichOption
        SELECT CASE (WhichOption)
          CASE (GridStateOption)
            GetLifeOption=PrimaryOptions%GridState
          CASE(ShowCellsAsOption)
            GetLifeOption=PrimaryOptions%ShowCellsAs
          CASE(ShowCounterOption)
            GetLifeOption=PrimaryOptions%ShowCounter
          CASE(ClearBoardToOption)
            GetLifeOption=PrimaryOptions%ClearBoardTo
          CASE(AllowPatDeleteOption)
            GetLifeOption=PrimaryOptions%AllowPatDelete
          CASE(ProtectOriginalOption)
            GetLifeOption=PrimaryOptions%ProtectOriginal
          CASE(FloatingHelpOption)
            GetLifeOption=PrimaryOptions%FloatingHelp
          CASE DEFAULT
            GetLifeOption=UnknownOption
        END SELECT
        RETURN
        END FUNCTION

        INTEGER FUNCTION GetDefaultOption(WhichOption)
        IMPLICIT NONE
        INTEGER :: WhichOption
        SELECT CASE (WhichOption)
          CASE (GridStateOption)
            GetDefaultOption=4
          CASE(ShowCellsAsOption)
            GetDefaultOption=-1
          CASE(ShowCounterOption)
            GetDefaultOption=0
          CASE(ClearBoardToOption)
            GetDefaultOption=0
          CASE(AllowPatDeleteOption)
            GetDefaultOption=1
          CASE(ProtectOriginalOption)
            GetDefaultOption=1
          CASE(FloatingHelpOption)
            GetDefaultOption=1
          CASE DEFAULT
            GetDefaultOption=UnknownOption
        END SELECT
        RETURN
        END FUNCTION

        INTEGER FUNCTION SetLifeOption(WhichOption,OptionValue)
        IMPLICIT NONE
        INTEGER :: WhichOption
        INTEGER :: OptionValue
        SELECT CASE (WhichOption)
          CASE (GridStateOption)
            SetLifeOption=PrimaryOptions%GridState
            PrimaryOptions%GridState=OptionValue
          CASE(ShowCellsAsOption)
            SetLifeOption=PrimaryOptions%ShowCellsAs
            PrimaryOptions%ShowCellsAs=OptionValue
          CASE(ShowCounterOption)
            SetLifeOption=PrimaryOptions%ShowCounter
            PrimaryOptions%ShowCounter=OptionValue
          CASE(ClearBoardToOption)
            SetLifeOption=PrimaryOptions%ClearBoardTo
            PrimaryOptions%ClearBoardTo=OptionValue
          CASE(AllowPatDeleteOption)
            SetLifeOption=PrimaryOptions%AllowPatDelete
            PrimaryOptions%AllowPatDelete=OptionValue
          CASE(ProtectOriginalOption)
            SetLifeOption=PrimaryOptions%ProtectOriginal
            PrimaryOptions%ProtectOriginal=OptionValue
          CASE(FloatingHelpOption)
            SetLifeOption=PrimaryOptions%FloatingHelp
            PrimaryOptions%FloatingHelp=OptionValue
          CASE DEFAULT
            SetLifeOption=UnknownOption
        END SELECT
        END FUNCTION

!        INTEGER FUNCTION SetOptionsToDefault()
!        IMPLICIT NONE
!        INTEGER junk
!        junk=SetLifeOption(GridStateOption,GetDefaultOption(GridStateOption))
!        junk=SetLifeOption(ShowCellaAsOption,GetDefaultOption(ShowCellaAsOption))
!        junk=SetLifeOption(ShowCounterOption,GetDefaultOption(ShowCounterOption))
!        junk=SetLifeOption(ClearBoardToOption,GetDefaultOption(ClearBoardToOption))
!        junk=SetLifeOption(AllowPatDeleteOption,GetDefaultOption(AllowPatDeleteOption))
!        junk=SetLifeOption(ProtectOriginalOption,GetDefaultOption(ProtectOriginalOption))
!        junk=SetLifeOption(FloatingHelpOption,GetDefaultOption(FloatingHelpOption))
!        SetOptionsToDefault=1
!        END FUNCTION
      END MODULE

      PROGRAM THEGAME
      USE WINTERACTER
!      USE GameOfLife
      IMPLICIT NONE
      INCLUDE 'resource.INC'
      INTEGER icode
      INTEGER counter
      INTEGER IsFirstTime
      INTEGER IsRunning
      INTEGER DONE
      INTEGER whichearth
      INTEGER X,Y,Xsave,Ysave
      INTEGER, DIMENSION(2,0:81,0:61) :: earth
      INTEGER, DIMENSION(1:80,1:60) :: ChangeMap
      INTEGER, DIMENSION(3) :: RuleSet
      REAL, DIMENSION(8) :: BoundryRuleSet
      TYPE(WIN_STYLE) :: WINDOW
      TYPE(WIN_MESSAGE) :: MESSAGE
      CALL YIELD()
      IsFirstTime=1
      whichearth=1
      IsRunning=0
      counter=0
      DONE=0
      RuleSet(1)=3
      RuleSet(2)=2
      RuleSet(3)=4
      CALL InitEarth(earth(1,:,:),ChangeMap)
      CALL InitEarth(earth(2,:,:),ChangeMap)
      CALL WInitialise(' ')
      WINDOW%FLAGS=SysMenuOn+MinButton+MaxButton
      WINDOW%x=-1
      WINDOW%Y=-1
      WINDOW%WIDTH=401
      WINDOW%HEIGHT=301
      WINDOW%MENUID=0
      WINDOW%TITLE='THE GAME OF LIFE'
      CALL WMessageEnable(MouseButUp,1)
      CALL WindowOpen(WINDOW)
      CALL WDialogLoad(IDD_DIALOG01)
      CALL WDialogLoad(IDD_DIALOG02)
      CALL WDialogLoad(IDD_DIALOG03)
      CALL WDialogLoad(IDD_DIALOG04)
      CALL WDialogLoad(IDD_DIALOG05)
      CALL WDialogLoad(IDD_DIALOG06)
      CALL WDialogLoad(IDD_DIALOG10)
      CALL WDialogLoad(IDD_DIALOG11)
      CALL WDialogLoad(IDD_DIALOG12)
      CALL WDialogLoad(IDD_DIALOG13)
      CALL WDialogLoad(IDD_DIALOG14)
      CALL WDialogLoad(IDD_DIALOG15)
      CALL WDialogSelect(IDD_DIALOG03)
      CALL WDialogShow(-1,0,0,2)
      CALL DrawFastLifeBoard
      icode=NoMessage
      MESSAGE%VALUE1=IDSTOP
      DO WHILE (DONE.EQ.0)
        DO WHILE (IsRunning.NE.0)
          IF (IsRunning.EQ.1) THEN
            CALL WMessage(icode,MESSAGE)
          ELSE
            CALL WMessagePeek(icode,MESSAGE)
          END IF
          SELECT CASE (icode)
            CASE (CloseRequest)
              IF (MESSAGE%Win.EQ.0) THEN
                IsRunning=0
                DONE=1
              END IF
            CASE (Resize)
              IF (MESSAGE%Win.EQ.0) THEN
                CALL DrawLifeBoard(earth(whichearth,:,:),0,0,80,60)
              END IF
            CASE (Expose)
              CALL ReDrawUnderExpose(earth(whichearth,:,:),MESSAGE)
            CASE (MouseButDown)
              CALL GetCellPos(MESSAGE%X,MESSAGE%Y,X,Y)
              IF (MESSAGE%VALUE1.EQ.1) THEN
                CALL StretchCellBox(earth(whichearth,:,:),ChangeMap,X,Y,-1,3)
              ELSE
                CALL StretchCellBox(earth(whichearth,:,:),ChangeMap,X,Y,0,1)
              END IF
            CASE (KeyDown)
              SELECT CASE (MESSAGE%VALUE1)
                CASE (KeyDelete)
                  CALL GetCellPos(MESSAGE%X,MESSAGE%Y,X,Y)
                  IF (X.LE.80 .AND. X.GE.1 .AND. Y.LE.60 .AND. Y.GE.1) THEN
                    earth(whichearth,X,Y)=0
                    CALL DrawCell(X,Y,earth(whichearth,X,Y))
                  END IF
                CASE (KeyF1)
                  CALL WDialogLoad(IDD_DIALOG07)
                  CALL WDialogShow(-1,-1,0,2)
                CASE (KeyF2)
                  CALL GetCellPos(MESSAGE%X,MESSAGE%Y,X,Y)
                  IF (X.LE.80 .AND. X.GE.1 .AND. Y.LE.60 .AND. Y.GE.1) THEN
                    CALL SaveCellArea(earth(whichearth,:,:),X,Y,3)
                  ELSE
!                    CALL HandleLifeError('Cell Properties','Error finding cell')
                  END IF
                CASE (KeyF4)
                  CALL GetCellPos(MESSAGE%X,MESSAGE%Y,X,Y)
                  IF (X.LE.80 .AND. X.GE.1 .AND. Y.LE.60 .AND. Y.GE.1) THEN
                    CALL WDialogSelect(IDD_DIALOG02)
                    Xsave=WInfoDialog(DialogXPos)
                    Ysave=WInfoDialog(DialogYPos)
                    CALL WDialogHide()
                    CALL StretchCellBoundingBox(earth(whichearth,:,:),ChangeMap,X,Y,3)
                    CALL WDialogSelect(IDD_DIALOG02)
                    CALL WDialogShow(Xsave,Ysave,0,2)
                  ELSE
!                    CALL HandleLifeError('Cell Properties','Error finding cell')
                  END IF
                CASE (KeyF3)
                  CALL GetCellPos(MESSAGE%X,MESSAGE%Y,X,Y)
                  IF (X.LE.80 .AND. X.GE.1 .AND. Y.LE.60 .AND. Y.GE.1) THEN
                    CALL HandleShowCellAge(MESSAGE%X,MESSAGE%Y,earth(whichearth,:,:),X,Y)
                  ELSE
!                    CALL HandleLifeError('Cell Properties','Error finding cell')
                  END IF
              END SELECT
            CASE DEFAULT
              IF (icode.EQ.PushButton) THEN
                SELECT CASE (MESSAGE%VALUE1)
                  CASE (2)
                    SELECT CASE (MESSAGE%Win)
                      CASE (0)
                        IsRunning=0
                        DONE=0
                      CASE (IDD_DIALOG02)
                        IsRunning=0
                      CASE (IDD_DIALOG03)
                        CALL WDialogSelect(MESSAGE%Win)
                        CALL WDialogHide()
                      CASE DEFAULT
                        CALL WDialogSelect(MESSAGE%Win)
                        CALL WDialogHide()
                    END SELECT
                  CASE (IDSTEP)
                    IsRunning=1
                  CASE (IDCONT)
                    IsRunning=2
                  CASE (IDSTOP)
                    IsRunning=0
                END SELECT
              END IF
              IF (icode.NE.MouseButDown.AND.IsRunning.NE.0) THEN
                CALL UpdateCounterDialog(counter)
                IF (whichearth.EQ.1) THEN
                  CALL UpdateEarth(earth(1,:,:),earth(2,:,:),ChangeMap,RuleSet,BoundryRuleSet)
                  CALL UpdateLifeBoard(earth(2,:,:),ChangeMap)
                  whichearth=2
                ELSE
                  CALL UpdateEarth(earth(2,:,:),earth(1,:,:),ChangeMap,RuleSet,BoundryRuleSet)
                  CALL UpdateLifeBoard(earth(1,:,:),ChangeMap)
                  whichearth=1
                END IF
              END IF
          END SELECT
        END DO
        IF (DONE.EQ.0) THEN
          IsRunning=0
          IF (IsFirstTime.NE.1) THEN
            CALL WDialogSelect(IDD_DIALOG02)
            CALL WDialogHide()
          ELSE
            IsFirstTime=0
          END IF
!          CALL DrawLifeBoard(earth(whichearth,:,:),0,0,80,60)
          CALL WDialogSelect(IDD_DIALOG01)
          CALL WDialogShow(-1,-1,0,2)
        END IF
        DO WHILE (DONE.EQ.0.AND.IsRunning.EQ.0)
          CALL WMessage(icode,MESSAGE)
          SELECT CASE (icode)
            CASE (CloseRequest)
              IF (MESSAGE%Win.EQ.0) THEN
                DONE=1
              END IF
            CASE (Resize)
              IF (MESSAGE%Win.EQ.0) THEN
                CALL DrawLifeBoard(earth(whichearth,:,:),0,0,80,60)
              END IF
            CASE (Expose)
              CALL ReDrawUnderExpose(earth(whichearth,:,:),MESSAGE)
            CASE (MouseButDown)
              CALL GetCellPos(MESSAGE%X,MESSAGE%Y,X,Y)
              IF (MESSAGE%VALUE1.EQ.1) THEN
                CALL StretchCellBox(earth(whichearth,:,:),ChangeMap,X,Y,-1,3)
              ELSE
                CALL StretchCellBox(earth(whichearth,:,:),ChangeMap,X,Y,0,1)
              END IF
            CASE (KeyDown)
              SELECT CASE (MESSAGE%VALUE1)
                CASE (KeyF1)
                  CALL WDialogLoad(IDD_DIALOG07)
                  CALL WDialogShow(-1,-1,0,2)
                CASE (KeyF2)
                  CALL GetCellPos(MESSAGE%X,MESSAGE%Y,X,Y)
                  IF (X.LE.80 .AND. X.GE.1 .AND. Y.LE.60 .AND. Y.GE.1) THEN
                    CALL SaveCellArea(earth(whichearth,:,:),X,Y,3)
                  ELSE
!                    CALL HandleLifeError('Cell Properties','Error finding cell')
                  END IF
                CASE (KeyF4)
                  CALL GetCellPos(MESSAGE%X,MESSAGE%Y,X,Y)
                  IF (X.LE.80 .AND. X.GE.1 .AND. Y.LE.60 .AND. Y.GE.1) THEN
                    CALL WDialogSelect(IDD_DIALOG01)
                    Xsave=WInfoDialog(DialogXPos)
                    Ysave=WInfoDialog(DialogYPos)
                    CALL WDialogHide()
                    CALL StretchCellBoundingBox(earth(whichearth,:,:),ChangeMap,X,Y,3)
                    CALL WDialogSelect(IDD_DIALOG01)
                    CALL WDialogShow(Xsave,Ysave,0,2)
                  ELSE
!                    CALL HandleLifeError('Cell Properties','Error finding cell')
                  END IF
                CASE (KeyF3)
                  CALL GetCellPos(MESSAGE%X,MESSAGE%Y,X,Y)
                  IF (X.LE.80 .AND. X.GE.1 .AND. Y.LE.60 .AND. Y.GE.1) THEN
                    CALL HandleShowCellAge(MESSAGE%X,MESSAGE%Y,earth(whichearth,:,:),X,Y)
                  ELSE
!                    CALL HandleLifeError('Cell Properties','Error finding cell')
                  END IF
              END SELECT
            CASE (PushButton)
              SELECT CASE (MESSAGE%VALUE1)
                  CASE (2)
                    SELECT CASE (MESSAGE%Win)
                      CASE (0)
                        CALL TerminateLifeNicely
                        STOP
                      CASE (IDD_DIALOG01)
                        DONE=1
                      CASE (IDD_DIALOG03)
                        CALL WDialogSelect(MESSAGE%Win)
                        CALL WDialogHide()
                      CASE DEFAULT
                        CALL WDialogSelect(MESSAGE%Win)
                        CALL WDialogHide()
                    END SELECT
                CASE (IDBoard)
                  CALL WDialogSelect(IDD_DIALOG01)
                  CALL HandleBoardMenu(earth(whichearth,:,:),&
                                       ChangeMap,counter,&
                                       WInfoDialog(DialogXPos),&
                                       WInfoDialog(DialogYPos),&
                                       WInfoDialog(DialogWidth),&
                                       WInfoDialog(DialogHeight),&
                                       103)
                  CALL WDialogSelect(IDD_DIALOG01)
                CASE (IDOptions)
                  CALL WDialogSelect(IDD_DIALOG01)
                  CALL HandleOptionsMenu(earth(whichearth,:,:),&
                                         WInfoDialog(DialogXPos),&
                                         WInfoDialog(DialogYPos),&
                                         WInfoDialog(DialogWidth),&
                                         WInfoDialog(DialogHeight),&
                                         125)
                  CALL WDialogSelect(IDD_DIALOG01)
                CASE (IDRuleSets)
                  CALL WDialogSelect(IDD_DIALOG01)
                  CALL HandleRuleSetMenu(earth(whichearth,:,:),&
                                         WInfoDialog(DialogXPos),&
                                         WInfoDialog(DialogYPos),&
                                         WInfoDialog(DialogWidth),&
                                         WInfoDialog(DialogHeight),&
                                         87)
                  CALL WDialogSelect(IDD_DIALOG01)
                CASE (IDAddPat)
                  CALL WDialogSelect(IDD_DIALOG01)
                  Xsave=WInfoDialog(DialogXPos)
                  Ysave=WInfoDialog(DialogYPos)
                  CALL WDialogHide()
!                  X1=INT(MESSAGE%X*8/1000)+1
!                  Y2=INT((10000-MESSAGE%Y)*6/1000)+1
!                  X2=INT(MESSAGE%VALUE1*8/1000)+1
!                  Y1=INT((10000-MESSAGE%VALUE2)*6/1000)+1
!                 CALL DrawLifeBoard(earth(whichearth,:,:),X1,Y1,X2,Y2)
                  CALL HandleAddingPattern(earth(whichearth,:,:),ChangeMap)
                  CALL WDialogSelect(IDD_DIALOG01)
                  CALL WDialogShow(Xsave,Ysave,0,2)
                CASE (IDEXIT)
                  DONE=1
                CASE (IDSTART)
                  IsRunning=1
              END SELECT
            CASE DEFAULT
          END SELECT
        END DO
        CALL WdialogSelect(IDD_DIALOG01)
        CALL WDialogHide()
        CALL WDialogSelect(IDD_DIALOG02)
        CALL WDialogShow(1,1,0,2)
      END DO
      CALL TerminateLifeNicely
      END PROGRAM

      SUBROUTINE UpdateLifeBoard(earth,ChangeMap)
      USE WINTERACTER
      IMPLICIT NONE
      INTEGER i,j
      INTEGER, DIMENSION(0:81,0:61) :: earth
      INTEGER, DIMENSION(1:80,1:60) :: ChangeMap
!      CALL WindowSelect(0)
      CALL IGrArea(0.0,0.0,1.0,1.0)
      CALL IGrUnits(0.0,0.0,800.0,600.0)
      DO i = 1,80,1
        DO j = 1,60,1
          IF (ChangeMap(i,j).NE.0) THEN
            CALL DrawCell(i,j,earth(i,j))
          END IF
        END DO
      END DO
      RETURN
      END SUBROUTINE

      SUBROUTINE ShowChangePattern(DispPattern,earth,ChangeMap)
      USE WINTERACTER
      IMPLICIT NONE
      INTEGER i,j
      INTEGER, DIMENSION(0:81,0:61) :: earth
      INTEGER, DIMENSION(0:81,0:61) :: DispPattern
      INTEGER, DIMENSION(1:80,1:60) :: ChangeMap
!      CALL WindowSelect(0)
      CALL IGrArea(0.0,0.0,1.0,1.0)
      CALL IGrUnits(0.0,0.0,800.0,600.0)
      DO i = 1,80,1
        DO j = 1,60,1
          IF (ChangeMap(i,j).NE.0) THEN
            CALL DrawChangeCell(i,j,DispPattern(i,j),earth(i,j))
          END IF
        END DO
      END DO
      RETURN
      END SUBROUTINE


      SUBROUTINE SetChangeMap(ChangeMap,minx,miny,maxx,maxy,value)
      IMPLICIT NONE
      INTEGER i,j,minx,miny,maxx,maxy,value
      INTEGER, DIMENSION(1:80,1:60) :: ChangeMap
      IF (minx.LT.1) THEN
        minx=1
      END IF
      IF (miny.LT.1) THEN
        miny=1
      END IF
      IF (maxx.GT.80) THEN
        maxx=80
      END IF
      IF (maxy.GT.60) THEN
        maxy=60
      END IF
      DO i=1,80,1
        DO j=1,60,1
          IF (i.GE.minx.AND.i.LE.maxx.AND.j.GE.miny.AND.j.LE.maxy) THEN
            ChangeMap(i,j)=value
          END IF
        END DO
      END DO
      END SUBROUTINE

      SUBROUTINE SetChangeMapWClear(ChangeMap,minx,miny,maxx,maxy,value)
      IMPLICIT NONE
      INTEGER i,j,minx,miny,maxx,maxy,value
      INTEGER, DIMENSION(1:80,1:60) :: ChangeMap
      IF (minx.LT.1) THEN
        minx=1
      END IF
      IF (miny.LT.1) THEN
        miny=1
      END IF
      IF (maxx.GT.80) THEN
        maxx=80
      END IF
      IF (maxy.GT.60) THEN
        maxy=60
      END IF
      DO i=1,80,1
        DO j=1,60,1
          IF (i.GE.minx.AND.i.LE.maxx.AND.j.GE.miny.AND.j.LE.maxy) THEN
            ChangeMap(i,j)=value
          ELSE
            ChangeMap(i,j)=0
          END IF
        END DO
      END DO
      END SUBROUTINE

      SUBROUTINE SetChangeMapToPattern(ChangeMap,pattern,value)
      IMPLICIT NONE
      INTEGER value
      INTEGER, DIMENSION(0:81,0:61) :: pattern
      INTEGER, DIMENSION(1:80,1:60) :: ChangeMap
      INTEGER i,j
      DO i=1,80,1
        DO j=1,60,1
          IF (pattern(i,j).GT.0) THEN
            ChangeMap(i,j)=value
          END IF
        END DO
      END DO
      RETURN
      END SUBROUTINE
             
      SUBROUTINE SetChangeMapToPatternWClear(ChangeMap,pattern,value)
      IMPLICIT NONE
      INTEGER value
      INTEGER, DIMENSION(0:81,0:61) :: pattern
      INTEGER, DIMENSION(1:80,1:60) :: ChangeMap
      INTEGER i,j
      DO i=1,80,1
        DO j=1,60,1
          IF (pattern(i,j).GT.0) THEN
            ChangeMap(i,j)=value
          ELSE
            ChangeMap(i,j)=0
          END IF
        END DO
      END DO
      RETURN
      END SUBROUTINE

      SUBROUTINE SetEarthToPattern(earth,pattern,ChangeMap)
      IMPLICIT NONE
      INTEGER, DIMENSION(0:81,0:61) :: earth
      INTEGER, DIMENSION(0:81,0:61) :: pattern
      INTEGER, DIMENSION(1:80,1:60) :: ChangeMap
      INTEGER i,j
      DO i=1,80,1
        DO j=1,60,1
          IF (ChangeMap(i,j).NE.0) THEN
            earth(i,j)=pattern(i,j)
          END IF
        END DO
      END DO
      RETURN
      END SUBROUTINE

      SUBROUTINE SetBothToPattern(earth,ChangeMap,pattern)
      IMPLICIT NONE
      INTEGER, DIMENSION(0:81,0:61) :: earth
      INTEGER, DIMENSION(0:81,0:61) :: pattern
      INTEGER, DIMENSION(1:80,1:60) :: ChangeMap
      INTEGER i,j
      DO i=1,80,1
        DO j=1,60,1
          IF (Pattern(i,j).GT.0) THEN
            earth(i,j)=pattern(i,j)
            ChangeMap(i,j)=1
          ELSE
            ChangeMap(i,j)=0
          END IF
        END DO
      END DO
      RETURN
      END SUBROUTINE

      SUBROUTINE DrawChangeMap(ChangeMap)
      USE WINTERACTER
      IMPLICIT NONE
      INTEGER i,j
      INTEGER, DIMENSION(1:80,1:60) :: ChangeMap
!      CALL WindowSelect(0)
      CALL IGrArea(0.0,0.0,1.0,1.0)
      CALL IGrUnits(0.0,0.0,800.0,600.0)
      CALL IGrAreaClear
      DO i = 1,80,1
        DO j = 1,60,1
          CALL DrawCell(i,j,ChangeMap(i,j))
        END DO
      END DO
      RETURN
      END SUBROUTINE

      SUBROUTINE RedrawUnderExpose(earth,MESSAGE)
      USE WINTERACTER
      IMPLICIT NONE
      INTEGER, DIMENSION(0:81,0:61) :: earth
      TYPE(WIN_Message) :: MESSAGE
      INTEGER X1,Y1,X2,Y2
      IF (MESSAGE%Win.EQ.0) THEN
        X1=FLOOR(REAL(MESSAGE%X)/1000.0*8.0)
        X2=CEILING(REAL(MESSAGE%VALUE1)/1000.0*8.0)+X1+1
        Y2=CEILING(REAL(10000-MESSAGE%Y)/1000.0*6.0)
        Y1=Y2-FLOOR(REAL(MESSAGE%VALUE2)/1000.0*6.0)-1
        CALL DrawLifeBoard(earth,X1,Y1,X2,Y2)
      END IF
      RETURN
      END SUBROUTINE

      SUBROUTINE RedrawExposeWithCM(earth,ChangeMap,MESSAGE)
      USE WINTERACTER
      IMPLICIT NONE
      INTEGER, DIMENSION(0:81,0:61) :: earth
      INTEGER, DIMENSION(1:80,1:60) :: ChangeMap
      TYPE(WIN_Message) :: MESSAGE
!      INTEGER X1,Y1,X2,Y2
!      IF (MESSAGE%Win.EQ.0) THEN
!        X1=FLOOR(REAL(MESSAGE%X)/1000.0*8.0)
!        X2=CEILING(REAL(MESSAGE%VALUE1)/1000.0*8.0)+X1
!        Y2=CEILING(REAL(10000-MESSAGE%Y)/1000.0*6.0)
!        Y1=Y2-FLOOR(REAL(MESSAGE%VALUE2)/1000.0*6.0)
!        CALL UpdateLifeBoard(earth,ChangeMap)
!      END IF
      RETURN
      END SUBROUTINE

      SUBROUTINE DrawLifeBoard(earth,minx,miny,maxx,maxy)
      USE WINTERACTER
      IMPLICIT NONE
      INTEGER i,j,minx,miny,maxx,maxy
      INTEGER, DIMENSION(0:81,0:61) :: earth
      IF (minx.LT.1) THEN
        minx=1
      END IF
      IF (miny.LT.1) THEN
        miny=1
      END IF
      IF (maxx.GT.80) THEN
        maxx=80
      END IF
      IF (maxy.GT.60) THEN
        maxy=60
      END IF
!      CALL WindowSelect(0)
      CALL IGrArea(0.0,0.0,1.0,1.0)
      CALL IGrUnits(0.0,0.0,800.0,600.0)
!      CALL IGrAreaClear
      DO i = minx,maxx,1
        DO j = miny,maxy,1
          CALL DrawCell(i,j,earth(i,j))
        END DO
      END DO
      RETURN
      END SUBROUTINE

      SUBROUTINE DrawFastLifeBoard
      USE WINTERACTER
      IMPLICIT NONE
      INTEGER i
      CALL IGrArea(0.0,0.0,1.0,1.0)
      CALL IGrUnits(0.0,0.0,800.0,600.0)
      CALL IGrAreaClear
      CALL IGrLineType(0)
      CALL IGrColourN(208)
      DO i=0,800,10
        CALL IGrMoveTo(REAL(i),0.0)
        CALL IGrLineTo(REAL(i),600.0)
      END DO
      DO i=0,600,10
        CALL IGrMoveTo(0.0,REAL(i))
        CALL IGrLineTo(800.0,REAL(i))
      END DO
      RETURN
      END SUBROUTINE

      SUBROUTINE DrawBoxAroundCells(Xmin,Ymin,Xmax,Ymax,LineStyle,LineColor)
      USE WINTERACTER
      IMPLICIT NONE
      REAL, DIMENSION(4,2) :: squarevert
      INTEGER Xmin,Ymin,Xmax,Ymax,LineStyle,LineColor
      INTEGER localXmin,localYmin
      localXmin=Xmin-1
      localYmin=Ymin-1
      CALL IGrLineType(LineStyle)
      CALL IGrColourN(1)
      CALL IGrColourN(LineColor)
      CALL IGrFillPattern(0)
      squarevert(1,1)=REAL(localXmin*10)
      squarevert(1,2)=REAL(localYmin*10)
      squarevert(2,1)=REAL(localXmin*10)
      squarevert(2,2)=REAL(Ymax*10)
      squarevert(3,1)=REAL(Xmax*10)
      squarevert(3,2)=REAL(Ymax*10)
      squarevert(4,1)=REAL(Xmax*10)
      squarevert(4,2)=REAL(localYmin*10)
      CALL IGrPolygonComplex(squarevert(:,1),squarevert(:,2),4)
      RETURN
      END SUBROUTINE

      SUBROUTINE DrawFinderAroundCells(Xmin,Ymin,Xmax,Ymax,LineStyle,LineColor)
      USE WINTERACTER
      IMPLICIT NONE
      REAL, DIMENSION(8,2) :: squarevert
      INTEGER Xmin,Ymin,Xmax,Ymax,LineStyle,LineColor
      INTEGER localXmin,localYmin,i
      localXmin=Xmin-1
      localYmin=Ymin-1
      CALL IGrLineType(LineStyle)
      CALL IGrColourN(1)
      CALL IGrColourN(LineColor)
      CALL IGrFillPattern(0)
      squarevert(1,1)=0.0
      squarevert(1,2)=REAL(localYmin*10)
      squarevert(2,1)=0.0
      squarevert(2,2)=REAL(Ymax*10)
      squarevert(3,1)=REAL(localXmin*10)
      squarevert(3,2)=0.0
      squarevert(4,1)=REAL(Xmax*10)
      squarevert(4,2)=0.0
      squarevert(5,1)=800.0
      squarevert(5,2)=REAL(localYmin*10)
      squarevert(6,1)=800.0
      squarevert(6,2)=REAL(Ymax*10)
      squarevert(7,1)=REAL(localXmin*10)
      squarevert(7,2)=600.0
      squarevert(8,1)=REAL(Xmax*10)
      squarevert(8,2)=600.0
      DO i=1,4,1
        CALL IGrMoveTo(squarevert(i,1),squarevert(i,2))
        CALL IGrLineTo(squarevert(i+4,1),squarevert(i+4,2))
      END DO
      RETURN
      END SUBROUTINE

      SUBROUTINE DrawCell(i,j,cellval)
      USE WINTERACTER
      IMPLICIT NONE
      INCLUDE 'resource.INC'
      REAL, DIMENSION(4,2) :: squarevert
      INTEGER i,j,cellval,locali,localj
      locali=i-1
      localj=j-1
      squarevert(1,1)=REAL((locali*10))
      squarevert(1,2)=REAL((localj*10))
      squarevert(2,1)=REAL((locali*10))
      squarevert(2,2)=REAL((localj*10)+10)
      squarevert(3,1)=REAL((locali*10)+10)
      squarevert(3,2)=REAL((localj*10)+10)
      squarevert(4,1)=REAL((locali*10)+10)
      squarevert(4,2)=REAL((localj*10))
      CALL WDialogSelect(IDD_DIALOG11)
      CALL IGrFillPattern(4)
      IF (cellval.NE.0) THEN
        CALL WDialogGetCheckBox(IDShowAge,locali)
        IF (locali.EQ.1) THEN
          IF (ABS(cellval).GT.8) THEN
            CALL IGrColourN(8*16)
            CALL IGrColourN(8*16)
          ELSE
            CALL IGrColourN(ABS(cellval)*16)
            CALL IGrColourN(ABS(cellval)*16)
          END IF
        ELSE
          CALL WDialogGetMenu(IDShowAgeAs,localj)
          CALL IGrColourN(localj*16)
          CALL IGrColourN(localj*16)
        END IF
      ELSE
        CALL IGrColourN(0)
        CALL IGrColourN(0)
      END IF
      CALL IGrFillPattern(4)
      CALL IGrPolygonComplex(squarevert(:,1),squarevert(:,2),4)
      CALL WDialogGetCheckBox(IDGridState,locali)
      IF (locali.EQ.1) THEN
        CALL IGrLineType(0)
        CALL IGrColourN(208)
        CALL IGrFillPattern(0)
        CALL IGrPolygonComplex(squarevert(:,1),squarevert(:,2),4)
      END IF
      RETURN
      END SUBROUTINE

      SUBROUTINE DrawChangeCell(i,j,cellval,altcellval)
      USE WINTERACTER
      IMPLICIT NONE
      INCLUDE 'resource.INC'
      REAL, DIMENSION(4,2) :: squarevert
      INTEGER i,j,cellval,altcellval,locali,localj,localaltcellval
      locali=i-1
      localj=j-1
      IF (altcellval.LT.0) THEN
        localaltcellval=ABS(altcellval)-1
      ELSE
        localaltcellval=altcellval
      END IF
      squarevert(1,1)=REAL((locali*10))
      squarevert(1,2)=REAL((localj*10))
      squarevert(2,1)=REAL((locali*10))
      squarevert(2,2)=REAL((localj*10)+10)
      squarevert(3,1)=REAL((locali*10)+10)
      squarevert(3,2)=REAL((localj*10))
      squarevert(4,1)=REAL((locali*10)+10)
      squarevert(4,2)=REAL((localj*10)+10)
      CALL WDialogSelect(IDD_DIALOG11)
      CALL IGrFillPattern(4)
      IF (cellval.NE.0) THEN
        CALL WDialogGetCheckBox(IDShowAge,locali)
        IF (locali.EQ.1) THEN
          IF (ABS(cellval).GT.8) THEN
            CALL IGrColourN(0)
            CALL IGrPolygonComplex(squarevert(1:3,1),squarevert(1:3,2),3)
            CALL IGrColourN(8*16)
            CALL IGrPolygonComplex(squarevert(2:4,1),squarevert(2:4,2),3)
          ELSE
            CALL IGrColourN(localaltcellval*16)
            CALL IGrPolygonComplex(squarevert(1:3,1),squarevert(1:3,2),3)
            CALL IGrColourN(ABS(cellval)*16)
            CALL IGrPolygonComplex(squarevert(2:4,1),squarevert(2:4,2),3)
          END IF
        ELSE
          CALL WDialogGetMenu(IDShowAgeAs,localj)
          CALL IGrColourN(localj*16)
          CALL IGrPolygonComplex(squarevert(1:3,1),squarevert(1:3,2),3)
          CALL IGrPolygonComplex(squarevert(2:4,1),squarevert(2:4,2),3)
        END IF
!        CALL IGrFillPattern(3,2,45)
!        CALL IGrPolygonComplex(squarevert(:,1),squarevert(:,2),4)
      ELSE
        CALL IGrColourN(localaltcellval)
        CALL IGrPolygonComplex(squarevert(1:3,1),squarevert(1:3,2),3)
        CALL IGrColourN(0)
        CALL IGrPolygonComplex(squarevert(2:4,1),squarevert(2:4,2),3)
!        CALL IGrColourN(1)
!        CALL IGrColourN(1)
!        CALL IGrArea(squarevert(1,1)/800.0,squarevert(1,2)/600.0,squarevert(3,1)/800.0,squarevert(3,2)/600.0)
!        CALL IGrAreaClear
!        CALL IGrArea(0.0,0.0,1.0,1.0)
      END IF
!      CALL IGrPolygonComplex(squarevert(:,1),squarevert(:,2),4)
      CALL WDialogGetCheckBox(IDGridState,locali)
      IF (locali.EQ.1) THEN
        CALL SwapREALS(squarevert(3,1),squarevert(4,1))
        CALL SwapREALS(squarevert(3,2),squarevert(4,2))
        CALL IGrLineType(0)
        CALL IGrColourN(208)
        CALL IGrFillPattern(0)
        CALL IGrPolygonComplex(squarevert(:,1),squarevert(:,2),4)
      END IF
      RETURN
      END SUBROUTINE

      SUBROUTINE InitEarth(earth,ChangeMap)
      IMPLICIT NONE
      INTEGER, DIMENSION(0:81,0:61) :: earth
      INTEGER, DIMENSION(1:80,1:60) :: ChangeMap
      INTEGER i,j
      DO i=0,81,1
        DO j=0,61,1
          earth(i,j)=0
          IF (i.GE.1.AND.i.LE.80.AND.j.GE.1.AND.j.LE.60) THEN
            ChangeMap(i,j)=1
          END IF
        END DO
      END DO
      RETURN
      END SUBROUTINE

      SUBROUTINE SetLifeRules(RuleSet,BoundryRuleSet)
      USE WINTERACTER
      IMPLICIT NONE
      INCLUDE 'resource.INC'
      INTEGER, DIMENSION(3) :: RuleSet
      REAL, DIMENSION(8) :: BoundryRuleSet
      INTEGER option
      CHARACTER (LEN=30) :: cvalue
      CALL WDialogSelect(IDD_DIALOG10)
      CALL WDialogGetMenu(IDRuleMenu,option)
      SELECT CASE (option)
        CASE (1)
          CALL WDialogGetString(IDBIRTH,cvalue)
          CALL IStringToInteger(cvalue,RuleSet(1))
          CALL WDialogGetString(IDSURVIVE,cvalue)
          CALL IStringToInteger(cvalue,RuleSet(2))
          CALL WDialogGetString(IDDEATH,cvalue)
          CALL IStringToInteger(cvalue,RuleSet(3))
        CASE (2)
        CASE (3)
        CASE DEFAULT
      END SELECT
      RETURN
      END SUBROUTINE

      SUBROUTINE InitRandomEarth(earth,ChangeMap)
      IMPLICIT NONE
      INTEGER, DIMENSION(0:81,0:61) :: earth
      INTEGER, DIMENSION(1:80,1:60) :: ChangeMap
      REAL rannum
      REAL CellDensity
      INTEGER i,j,CellPrev
      CellDensity=75.0
      DO i = 0,81,1
        earth(i,0)=0
        earth(i,61)=0
      END DO
      DO j = 0,61,1
        earth(0,j)=0
        earth(81,j)=0
      END DO
      DO i=1,80,1
        DO j=1,60,1
          CellPrev=earth(i,j)
          CALL RANDOM_NUMBER(rannum)
          IF (FLOOR(rannum*CellDensity/50.0).GT.0) THEN
            CALL RANDOM_NUMBER(rannum)
            earth(i,j)=FLOOR(rannum*7.99)
          ELSE
            earth(i,j)=0
          END IF
          IF (earth(i,j).NE.CellPrev) THEN
            ChangeMap(i,j)=1
          ELSE
            ChangeMap(i,j)=0
          END IF
        END DO
      END DO
      RETURN
      END SUBROUTINE

      SUBROUTINE UpdateEarth(oldearth,newearth,ChangeMap,RuleSet,BoundryRuleSet)
      USE WINTERACTER
      IMPLICIT NONE
      INTEGER, DIMENSION(0:81,0:61) :: oldearth
      INTEGER, DIMENSION(0:81,0:61) :: newearth
      INTEGER, DIMENSION(1:80,1:60) :: ChangeMap
      INTEGER, DIMENSION(3) :: RuleSet
      REAL, DIMENSION(8) :: BoundryRuleSet
      INTEGER, DIMENSION(8) :: boundry
      INTEGER BoundrySum
      INTEGER i,j,k,IsNonZero
      DO i = 1,80,1
        DO j = 1,60,1
          boundry(1)=oldearth(i-1,j-1)
          boundry(2)=oldearth(i-1,j)
          boundry(3)=oldearth(i-1,j+1)
          boundry(4)=oldearth(i,j+1)
          boundry(5)=oldearth(i+1,j+1)
          boundry(6)=oldearth(i+1,j)
          boundry(7)=oldearth(i+1,j-1)
          boundry(8)=oldearth(i,j-1)
          BoundrySum=0
          DO k=1,8,1
            BoundrySum=BoundrySum+IsNonZero(boundry(k))
          END DO
          IF (oldearth(i,j).EQ.0) THEN
            IF (BoundrySum.EQ.RuleSet(1)) THEN
              newearth(i,j)=1
              ChangeMap(i,j)=1
            ELSE
              newearth(i,j)=0
              ChangeMap(i,j)=0
            END IF
          ELSE
            IF (BoundrySum.GE.RuleSet(2).AND.BoundrySum.LT.RuleSet(3)) THEN
              ChangeMap(i,j)=0
              IF (oldearth(i,j).LT.32767) THEN
                newearth(i,j)=oldearth(i,j)+1
                IF (oldearth(i,j).LT.8) THEN
                  ChangeMap(i,j)=1
                END IF
              ELSE
                newearth(i,j)=oldearth(i,j)
              END IF
            ELSE
              newearth(i,j)=0
              ChangeMap(i,j)=1
            END IF
          END IF
        END DO
      END DO
      RETURN
      END SUBROUTINE

      INTEGER FUNCTION IsNonZero(x)
      INTEGER x
      IF (x.NE.0) THEN
        IsNonZero=1
      ELSE
        IsNonZero=0
      END IF
      RETURN
      END

      SUBROUTINE UpdateCounterDialog(counter)
      USE WINTERACTER
      IMPLICIT NONE
      INCLUDE 'resource.INC'
      INTEGER counter
      CHARACTER (LEN=20) :: CountString
      counter=counter+1
      IF (counter.GT.32760) THEN
        counter=0
      END IF
      CALL IntegerToString(counter,CountString,'(I5)')
      CALL WDialogSelect(IDD_DIALOG03)
      CALL WDialogPutString(IDCounter,CountString)
      RETURN
      END SUBROUTINE

      SUBROUTINE HandleLifeError(DlgTitle,ErrMessage)
      USE WINTERACTER
      IMPLICIT NONE
      CHARACTER*(*) DlgTitle
      CHARACTER*(*) :: ErrMessage
      CHARACTER (LEN=255) :: PrtDlgTitle
      IF (DlgTitle(1:1).NE.' ') THEN
        PrtDlgTitle=DlgTitle
      ELSE
        PrtDlgTitle='Error'
      END IF
      CALL WMessageBox(RetryAbortIgnore,ExclamationIcon,CommonRetry,ErrMessage,PrtDlgTitle)
      IF (WInfoDialog(ExitButtonCommon).EQ.CommonAbort) THEN
        CALL TerminateLifeNicely
        STOP
      END IF
      RETURN
      END SUBROUTINE

      SUBROUTINE HandleDialogError(DlgTitle)
      USE WINTERACTER
      IMPLICIT NONE
      CHARACTER*(*) DlgTitle
      INTEGER NOERROR
      INTEGER DialogErrorCode
      CHARACTER (LEN=5) :: ERRCODE
      CHARACTER (LEN=255) :: ErrMessage
      CHARACTER (LEN=255) :: PrtDlgTitle
      PrtDlgTitle=DlgTitle
      NOERROR=0
      DialogErrorCode=InfoError(1)
      SELECT CASE (DialogErrorCode)
          CASE (0)
            NOERROR=1
          CASE (ErrLoadDialog)
            PrtDlgTitle='Error'
            ErrMessage='Error Loading Dialog'
          CASE (ErrSelDialog)
            PrtDlgTitle='Error'
            ErrMessage='Error Selecting Dialog'
          CASE (ErrCurDialog)
            PrtDlgTitle='Error'
            ErrMessage='No Current Dialog Selected'
          CASE (ErrFieldNum)
            PrtDlgTitle='Error'
            ErrMessage='Invalid Field Identifier'
          CASE (ErrOptionNum)
            PrtDlgTitle='Error'
            ErrMessage='Option Number Out of Range'
          CASE (ErrFieldUndefined)
            PrtDlgTitle='Error'
            ErrMessage='Field value is Undefined'
          CASE (ErrFileOpen)
            PrtDlgTitle='Error'
            ErrMessage='Error Opening a File'
          CASE (ErrFileIO)
            PrtDlgTitle='Error'
            ErrMessage='Error reading from a File'
          CASE (ErrFileClose)
            PrtDlgTitle='Error'
            ErrMessage='Error closing a File or Device'
          CASE (ErrLargeNum)
            PrtDlgTitle='Error'
            ErrMessage='Number to Large in String to Numeric conversion'
          CASE (ErrNoSubstring)
            PrtDlgTitle='Error'
            ErrMessage='No Substring Found'
          CASE (ErrBadChar)
            PrtDlgTitle='Error'
            ErrMessage='Invalid character detected'
          CASE (ErrBadUnits)
            PrtDlgTitle='Error'
            ErrMessage='X or Y graphics unit range is invalid:  default of 0-1 used.'
          CASE (ErrNumToStr)
            PrtDlgTitle='Error'
            ErrMessage='Numeric to String Conversion error:  string filled with''*'''
          CASE (ErrBadRadius)
            PrtDlgTitle='Error'
            ErrMessage='Radius of Circle <= 0'
          CASE (ErrBadColour)
            PrtDlgTitle='Error'
            ErrMessage='Unknown color name or number specified to IGrColourN:  Current color unchanged'
          CASE (ErrBadArea)
            PrtDlgTitle='Error'
            ErrMessage='X or Y range specified to IGrArea is invalid:  default of 0-1 used'
          CASE (ErrFillComplex)
            PrtDlgTitle='Error'
            ErrMessage='Fill too complex in IGrPolygonComplex'
          CASE (ErrNoSoftFont)
            PrtDlgTitle='Error'
            ErrMessage='Unable to find software font to substitute for unavailbale hardware font'
          CASE (ErrMenuItem)
            PrtDlgTitle='Error'
            ErrMessage='Invalid Menu Item'
          CASE (ErrWinHandle)
            PrtDlgTitle='Error'
            ErrMessage='Invalid Window Handle'
          CASE (ErrCommonDlg)
            PrtDlgTitle='Error'
            ErrMessage='General Error in Common Dialog'
          CASE DEFAULT
            PrtDlgTitle='Error'
            CALL IntegerToString(DialogErrorCode,ERRCODE,'(I5)')
            ErrMessage='Error Unknown:  code=' // ERRCODE
      END SELECT
      IF (NOERROR.EQ.1) THEN
      ELSE
        CALL WMessageBox(RetryAbortIgnore,ExclamationIcon,CommonRetry,ErrMessage,PrtDlgTitle)
        IF (WInfoDialog(ExitButtonCommon).EQ.CommonAbort) THEN
          CALL TerminateLifeNicely
          STOP
        END IF
      END IF
      RETURN
      END SUBROUTINE

      SUBROUTINE TerminateLifeNicely()
      USE WINTERACTER
      IMPLICIT NONE
      INCLUDE 'resource.INC'
      CALL WDialogSelect(IDD_DIALOG01)
      CALL WDialogUnload()
      CALL WDialogSelect(IDD_DIALOG02)
      CALL WDialogUnload()
      CALL WDialogSelect(IDD_DIALOG03)
      CALL WDialogUnload()
      CALL WDialogSelect(IDD_DIALOG04)
      CALL WDialogUnload()
      CALL WDialogSelect(IDD_DIALOG05)
      CALL WDialogUnload()
      CALL WDialogSelect(IDD_DIALOG06)
      CALL WDialogUnload()
      CALL WDialogSelect(IDD_DIALOG10)
      CALL WDialogUnload()
      CALL WDialogSelect(IDD_DIALOG11)
      CALL WDialogUnload()
      CALL WDialogSelect(IDD_DIALOG12)
      CALL WDialogUnload()
      CALL WDialogSelect(IDD_DIALOG13)
      CALL WDialogUnload()
      CALL WDialogSelect(IDD_DIALOG14)
      CALL WDialogUnload()
      CALL WDialogSelect(IDD_DIALOG15)
      CALL WDialogUnload()
      CALL WindowSelect(0)
      CALL WindowClose()
      STOP
      END SUBROUTINE

      SUBROUTINE ClearErrorList()
      USE WINTERACTER
      IMPLICIT NONE
      INTEGER junk
      junk=InfoError(1)
      junk=junk+1
      RETURN
      END SUBROUTINE

      SUBROUTINE UpdateDisplayPattern(earth,Pattern,DisplayPattern,posx,posy,sizex,sizey,Transparent)
      IMPLICIT NONE
      INTEGER posx,posy,sizex,sizey,i,j,Transparent
      INTEGER, DIMENSION(0:81,0:61) :: earth
      INTEGER, DIMENSION(0:81,0:61) :: Pattern
      INTEGER, DIMENSION(0:81,0:61) :: DisplayPattern
      DO i=1,80,1
        DO j=1,60,1
          DisplayPattern(i,j)=0
        END DO
      END DO
      DO i=1,sizex,1
        DO j=1,sizey,1
          IF ((i+posx-1).LE.80.AND.(j+posy-1).LE.60) THEN
            IF (Transparent.EQ.1) THEN
              DisplayPattern(i+posx-1,j+posy-1)=Pattern(i,j)+earth(i+posx-1,j+posy-1)
            ELSE
              DisplayPattern(i+posx-1,j+posy-1)=Pattern(i,j)
            END IF
          END IF
        END DO
      END DO
      RETURN
      END SUBROUTINE

      SUBROUTINE HandleAddingPattern(earth,ChangeMap)
      USE WINTERACTER
      IMPLICIT NONE
      INCLUDE 'resource.INC'
      INTEGER, DIMENSION(0:81,0:61) :: earth
      INTEGER, DIMENSION(0:81,0:61) :: Pattern
      INTEGER, DIMENSION(0:81,0:61) :: DispPattern
      INTEGER, DIMENSION(1:80,1:60) :: ChangeMap
!      CHARACTER (LEN=20), DIMENSION(255) :: PatternNameList
!      INTEGER NumPatterns
      LOGICAL fexiststat
      INTEGER i,j,PatternID,icode,DONE,DoneDrop,TransparentFill,RedrawDisp
      INTEGER DragHelpUp,HelpUp
      INTEGER X,Y,Xprev,Yprev
      CHARACTER (LEN=255) :: infilename
      CHARACTER, DIMENSION(255) :: LineOfFile
      CHARACTER (LEN=5) :: OutString
!      CHARACTER, DIMENSION(239) :: OutLineOfFile
      INTEGER NumLines,NumCols
      CHARACTER SingleChar
      TYPE(WIN_MESSAGE) :: MESSAGE
!      INTEGER LastHighlight
!      SAVE LastHighlight
!      DATA LastHighlight /1/
      infilename='D:\epley\temp\life\lifepat.gol.txt'
      DONE=0
      HelpUp=0
      TransparentFill=1
      DragHelpUp=0
!      i=1
!      OPEN(UNIT=102,FILE='d:\epley\temp\life\patindex.gol.txt',STATUS='OLD')
!      DO WHILE (i.LT.255)
!        READ(102,'(A19)',END=1001,ERR=1000) PatternNameList(i)
!        i=i+1
!      END DO
!1000  CALL HandleLifeError('Adding Pattern','Error Reading File'&
!                                             infilename// )
!1001  CONTINUE
!      NumPatterns=i-1
!      CLOSE(102)
      CALL ClearErrorList
      CALL WDialogSelect(IDD_DIALOG04)
!      CALL WDialogPutMenu(IDAddMenu,PatternNameList,NumPatterns,LastHighlight)
!      CALL WDialogPutOption(IDAddMenu,LastHighlight)
      PatternID=1
      CALL HandleDialogError(' ')
      CALL WDialogShow(0,0,0,2)
      DO WHILE (DONE.EQ.0)
        CALL WMessage(icode,MESSAGE)
        SELECT CASE (icode)
          CASE (CloseRequest)
            IF (MESSAGE%Win.EQ.0) THEN
              CALL TerminateLifeNicely
              STOP
            END IF
          CASE (Expose)
            CALL ReDrawUnderExpose(earth,MESSAGE)
            CALL ReDrawExposeWithCM(DispPattern,ChangeMap,MESSAGE)
          CASE (Resize)
            CALL DrawLifeBoard(earth,0,0,80,60)
            CALL UpdateLifeBoard(DispPattern,ChangeMap)
          CASE (PushButton)
            SELECT CASE (MESSAGE%VALUE1)
              CASE (2)
                SELECT CASE (MESSAGE%Win)
                  CASE (0)
                    CALL TerminateLifeNicely
                    STOP
                  CASE (IDD_DIALOG03)
                    CALL WDialogSelect(MESSAGE%Win)
                    CALL WDialogHide()
                  CASE (IDD_DIALOG04)
                    DONE=1
                  CASE (IDD_DIALOG08)
                    CALL WDialogSelect(IDD_DIALOG08)
                    CALL WDialogHide()
                    HelpUp=0
                  CASE (IDD_DIALOG09)
                    CALL WDialogSelect(IDD_DIALOG09)
                    CALL WDialogHide()
                    DragHelpUp=0
                  CASE DEFAULT
                    CALL WDialogSelect(MESSAGE%Win)
                    CALL WDialogHide()
                END SELECT
              CASE (IDAddOK)
                DONE=1
            END SELECT
          CASE (KeyDown)
            SELECT CASE (MESSAGE%VALUE1)
                CASE (KeyF1)
                  IF (HelpUp.EQ.1) THEN
                    CALL WDialogSelect(IDD_DIALOG08)
                    CALL WDialogHide()
                    HelpUp=0
                  ELSE
                    CALL WDialogLoad(IDD_DIALOG08)
                    CALL WDialogShow(-1,-1,0,2)
                    HelpUp=1
                  END IF
                CASE (KeyEscape)
                  DONE=1
            END SELECT
          CASE (MouseButDown)
            IF (MESSAGE%VALUE1.EQ.1) THEN
              CALL WDialogSelect(IDD_DIALOG04)
              CALL WDialogGetMenu(IDAddMenu,PatternID)
!              LastHighlight=PatternID
              IF (PatternID.LT.0.OR.PatternID.GT.999) THEN
                CALL IntegerToString(PatternID,OutString,'(I5)')
                CALL HandleLifeError('Add Pattern','Pattern ID not Found'&
                                                    // CHAR(10) // CHAR(13)&
                                                    // 'ID=' // OutString)
              END IF
!FIND PATTERN IN FILE, and read into Pattern
              INQUIRE(FILE=infilename,EXIST=fexiststat)
              IF (fexiststat) THEN
                i=i+1
              ELSE
                CALL WSelectFile(IDFileFilter,LoadDialog+PromptOn+DirChange,infilename,'Pattern File')
                IF (WInfoDialog(ExitButton).NE.1) THEN
                  DONE=1
                END IF
              END IF
              IF (DONE.EQ.0) THEN
                OPEN(UNIT=101,FILE=infilename,STATUS='OLD')
!                OPEN(UNIT=102,FILE='D:\epley\temp\life\patindex.gol.txt',STATUS='REPLACE')
                DO i=1,PatternID,1
                  SingleChar=' '
                  DO WHILE (SingleChar.NE.'#')
                    READ(101,'(A1)',ERR=1001,END=1000) SingleChar
                  END DO
                END DO
                READ(101,'(I5)',ERR=1001,END=1000) NumLines
                DO j=1,NumLines,1
                  READ(101,'(255A1)',ERR=1001,END=1000) LineOfFile
                  SingleChar='.'
                  i=1
                  DO WHILE (SingleChar.EQ.'*'.OR.SingleChar.EQ.'.')
                    SingleChar=LineOfFile(i)
                    IF (SingleChar.EQ.'*') THEN
                      Pattern(i,NumLines-j+1)=1
                    ELSE
                      Pattern(i,NumLines-j+1)=0
                    END IF
                    i=i+1
                  END DO
                END DO
                NumCols=i-2
                CLOSE(101)
!Place Pattern on earth by dragging with left, right button to cancel
                CALL WMessageEnable(MouseMove,1)
                CALL WMessageEnable(MouseButUp,1)
                CALL InitEarth(DispPattern,ChangeMap)
                CALL SetChangeMap(ChangeMap,0,0,80,60,0)
                X=0
                Y=0
                Xprev=0
                Yprev=0
                DoneDrop=0
                RedrawDisp=0
                DO WHILE (DoneDrop.EQ.0)
                  CALL WMessage(icode,MESSAGE)
                  SELECT CASE (icode)
                    CASE (KeyDown)
                      SELECT CASE (MESSAGE%VALUE1)
                        CASE (KeyF1)
                          IF (DragHelpUp.EQ.0) THEN
                            CALL WDialogLoad(IDD_DIALOG09)
                            CALL WDialogShow(-1,-1,0,2)
                            DragHelpUp=1
                          ELSE
                            CALL WDialogSelect(IDD_DIALOG09)
                            CALL WDialogHide()
                            DragHelpUp=0
                          END IF
                        CASE (KeyEscape)
                          CALL UpdateLifeBoard(earth,ChangeMap)
                          DoneDrop=1
                        CASE (KeyHome,KeyEnd,KeyDelete,KeyInsert)
                          IF (TransparentFill.EQ.1) THEN
                            TransparentFill=0
                          ELSE
                            TransparentFill=1
                          END IF
                          RedrawDisp=1
                        CASE (KeyCursorUp,KeyCursorDown)
                          DO i=1,NumCols,1
                            DO j=1,NumLines,1
                              DispPattern(i,NumLines-j+1)=Pattern(i,j)
                            END DO
                          END DO
                          DO i=1,NumCols,1
                            DO j=1,NumLines,1
                              Pattern(i,j)=DispPattern(i,j)
                            END DO
                          END DO
                          RedrawDisp=1
                        CASE (KeyCursorRight,KeyCursorLeft)
                          DO i=1,NumCols,1
                            DO j=1,NumLines,1
                              DispPattern(NumCols-i+1,j)=Pattern(i,j)
                            END DO
                          END DO
                          DO i=1,NumCols,1
                            DO j=1,NumLines,1
                              Pattern(i,j)=DispPattern(i,j)
                            END DO
                          END DO
                          RedrawDisp=1
                        CASE (KeyPageUp)
                          IF (NumCols.GT.60) THEN
                            NumCols=60
                          END IF
                          IF (NumLines.GT.80) THEN
                            NumLines=80
                          END IF
                          DO i=1,NumCols,1
                            DO j=1,NumLines,1
                              DispPattern(j,i)=Pattern(i,j)
                            END DO
                          END DO
                          i=NumCols
                          NumCols=NumLines
                          NumLines=i
                          DO i=1,NumCols,1
                            DO j=1,NumLines,1
                              Pattern(i,j)=DispPattern(i,j)
                            END DO
                          END DO
                          RedrawDisp=1
                        CASE (KeyPageDown)
                          IF (NumCols.GT.60) THEN
                            NumCols=60
                          END IF
                          IF (NumLines.GT.80) THEN
                            NumLines=80
                          END IF
                          DO i=NumCols,1,-1
                            DO j=NumLines,1,-1
                              DispPattern(j,i)=Pattern(i,j)
                            END DO
                          END DO
                          i=NumCols
                          NumCols=NumLines
                          NumLines=i
                          DO i=1,NumCols,1
                            DO j=1,NumLines,1
                              Pattern(i,j)=DispPattern(i,j)
                            END DO
                          END DO
                          RedrawDisp=1
                      END SELECT
                    CASE (Resize)
                      IF (MESSAGE%Win.EQ.0) THEN
                        CALL DrawLifeBoard(earth,0,0,80,60)
                      END IF
                    CASE (Expose)
                      CALL RedrawUnderExpose(earth,MESSAGE)
                      CALL ReDrawExposeWithCM(DispPattern,ChangeMap,MESSAGE)
                    CASE (MouseMove)
                      CALL GetCellPos(MESSAGE%X,MESSAGE%Y,X,Y)
                      IF (X.LE.80 .AND. X.GE.1 .AND. Y.LE.60.AND.Y.GE.1.AND.(X.NE.Xprev.OR.Y.NE.Yprev)) THEN
                        RedrawDisp=1
                      END IF
                    CASE (MouseButDown)
                      IF (MESSAGE%VALUE1.EQ.3) THEN
                        CALL UpdateLifeBoard(earth,ChangeMap)
                        DoneDrop=1
                      END IF
                    CASE (MouseButUp)
                      IF (MESSAGE%VALUE1.EQ.1) THEN
                        IF (TransparentFill.EQ.0) THEN
                          DO i=1,NumCols,1
                            DO j=1,NumLines,1
                              IF ((X+i-1).LE.80.AND.(Y+j-1).LE.60) THEN
                                earth(X+i-1,Y+j-1)=Pattern(i,j)
                              END IF
                            END DO
                          END DO
                        ELSE
                          CALL SetBothToPattern(earth,ChangeMap,DispPattern)
                        END IF
                        CALL UpdateLifeBoard(earth,ChangeMap)
                        DoneDrop=1
                      END IF
                  END SELECT
                  IF (RedrawDisp.EQ.1) THEN
                    RedrawDisp=0
                    CALL UpdateLifeBoard(earth,ChangeMap)
                    CALL SetChangeMapToPattern(ChangeMap,DispPattern,0)
                    CALL UpdateDisplayPattern(earth,Pattern,DispPattern,X,Y,NumCols,NumLines,TransparentFill)
                    IF (TransparentFill.EQ.0) THEN
                      CALL SetChangeMapWClear(ChangeMap,X,Y,X+NumCols-1,Y+NumLines-1,1)
                    ELSE
                      CALL SetChangeMapToPatternWClear(ChangeMap,DispPattern,1)
                    END IF
                    IF (TransparentFill.EQ.0) THEN
                      CALL UpdateLifeBoard(DispPattern,ChangeMap)
                    ELSE
                      CALL ShowChangePattern(DispPattern,earth,ChangeMap)
                    END IF
                    Xprev=X
                    Yprev=Y
                  END IF
                END DO
                CALL WMessageEnable(MouseMove,0)
                CALL WMessageEnable(MouseButUp,0)
              END IF
            END IF
        END SELECT
      END DO
 1002 CONTINUE
      IF (HelpUp.EQ.1) THEN
        CALL WDialogSelect(IDD_DIALOG08)
        CALL WDialogHide()
      END IF
      IF (DragHelpUp.EQ.1) THEN
        CALL WDialogSelect(IDD_DIALOG09)
        CALL WDialogHide()
      END IF
      CALL WDialogSelect(IDD_DIALOG04)
      CALL WDialogHide()
      RETURN
 1000 CALL HandleLifeError('Adding Pattern','Unexpected end of file in'&
                                            // CHAR(10) // CHAR(13)&
                                            // infilename)
      GO TO 1002
 1001 CALL HandleLifeError('Adding Pattern','Error reading file '&
                                            // CHAR(10) // CHAR(13)&
                                            // infilename)
      GO TO 1002
      END SUBROUTINE

      SUBROUTINE StretchCellBox(earth,ChangeMap,Xinit,Yinit,ToCellAge,CancelButton)
      USE WINTERACTER
      IMPLICIT NONE
      INCLUDE 'resource.INC'
      INTEGER, DIMENSION(0:81,0:61) :: earth
      INTEGER, DIMENSION(1:80,1:60) :: ChangeMap
      INTEGER, DIMENSION(0:81,0:61) :: DispPattern
      INTEGER i,j,Xinit,Yinit,Xfinal,Yfinal,Xprev,Yprev,DoneDrop
      INTEGER temp
      INTEGER ToCellAge,CancelButton,HollowFill,IsMoved,OrigToAge
      SAVE OrigToAge
      DATA OrigToAge /1/
      INTEGER DeletePressed
      INTEGER icode
      TYPE(WIN_Message) :: MESSAGE
      DeletePressed=0
      CALL WMessageEnable(MouseMove,1)
      CALL WMessageEnable(MouseButUp,1)
      CALL InitEarth(DispPattern,ChangeMap)
      CALL SetChangeMap(ChangeMap,1,1,80,60,0)
      Xprev=Xinit
      Yprev=Yinit
      Xfinal=Xinit
      Yfinal=Yinit
      ChangeMap(Xinit,Yinit)=1
      DispPattern(Xinit,Yinit)=ToCellAge
      IsMoved=0
      DoneDrop=0
      HollowFill=1
      HollowFill=HollowFill+1
      DO WHILE (DoneDrop.EQ.0)
        CALL WMessage(icode,MESSAGE)
        SELECT CASE (icode)
          CASE (Resize)
            IF (MESSAGE%Win.EQ.0) THEN
              CALL DrawLifeBoard(earth,0,0,80,60)
              CALL ShowChangePattern(DispPattern,earth,ChangeMap)
            END IF
          CASE (Expose)
            CALL ReDrawUnderExpose(earth,MESSAGE)
            CALL ReDrawExposeWithCM(DispPattern,ChangeMap,MESSAGE)
          CASE (MouseMove)
            CALL GetCellPos(MESSAGE%X,MESSAGE%Y,Xfinal,Yfinal)
            IF (Xfinal.GT.80) THEN
              Xfinal=80
            END IF
            IF (Xfinal.LT.1) THEN
              Xfinal=1
            END IF
            IF (Yfinal.GT.60) THEN
              Yfinal=60
            END IF
            IF (Yfinal.LT.1) THEN
              Yfinal=1
            END IF
            IF (Xfinal.NE.Xprev.OR.Yfinal.NE.Yprev) THEN
              IF (IsMoved.EQ.0) THEN
                IsMoved=1
              END IF
              CALL SetChangeMap(ChangeMap,1,1,80,60,0)
              IF (Xfinal.LT.Xprev.OR.Yfinal.NE.Yprev) THEN
                IF (Yfinal.NE.Yprev) THEN
                  temp=Xinit
                ELSE
                  temp=Xfinal
                END IF
                DO i=temp,Xprev,1
                  IF (DispPattern(i,Yprev).NE.earth(i,Yprev)) THEN
                    DispPattern(i,Yprev)=earth(i,Yprev)
                    ChangeMap(i,Yprev)=1
                  END IF
                END DO
              END IF
              IF (Xfinal.LT.Xprev) THEN
                DO i=Xfinal,Xprev,1
                  IF (DispPattern(i,Yinit).NE.earth(i,Yinit)) THEN
                    DispPattern(i,Yinit)=earth(i,Yinit)
                    ChangeMap(i,Yinit)=1
                  END IF
                END DO
              END IF
              IF (Yfinal.LT.Yprev.OR.Xfinal.NE.Xprev) THEN
                IF (Xfinal.NE.Xprev) THEN
                  temp=Yinit
                ELSE
                  temp=Yfinal
                END IF
                DO j=temp,Yprev,1
                  IF (DispPattern(Xprev,j).NE.earth(Xprev,j)) THEN
                    DispPattern(Xprev,j)=earth(Xprev,j)
                    ChangeMap(Xprev,j)=1
                  END IF
                END DO
              END IF
              IF (Yfinal.LT.Yprev) THEN
                DO j=Yfinal,Yprev,1
                  IF (DispPattern(Xinit,j).NE.earth(Xinit,j)) THEN
                    DispPattern(Xinit,j)=earth(Xinit,j)
                    ChangeMap(Xinit,j)=1
                  END IF
                END DO
              END IF
              IF (Yfinal.NE.Yprev.OR.&
                  (Xfinal.GT.Xinit.AND.Xfinal.GT.Xprev)) THEN
                IF (Yfinal.NE.Yprev) THEN
                  temp=Xinit
                ELSE
                  IF (Xprev.GT.Xinit) THEN
                    temp=Xprev
                  ELSE
                    temp=Xinit
                  END IF
                END IF
                DO i=temp,Xfinal,1
                  IF (ToCellAge.GE.0) THEN
                    DispPattern(i,Yfinal)=-1*(ToCellAge)
                    IF (earth(i,Yfinal).NE.ToCellAge) THEN
                      ChangeMap(i,Yfinal)=1
                    END IF
                  ELSE
                    ChangeMap(i,Yfinal)=1
                    DispPattern(i,Yfinal)=-1*(earth(i,Yfinal)+1)
                  END IF
                END DO
              END IF
              IF ((Xfinal.GT.Xinit.AND.Xfinal.GT.Xprev).OR.Yprev.EQ.Yinit) THEN
                IF (Yprev.EQ.Yinit) THEN
                  temp=Xinit
                ELSE
                  IF (Xprev.GT.Xinit) THEN
                    temp=Xprev
                  ELSE
                    temp=Xinit
                  END IF
                END IF
                DO i=temp,Xfinal,1
                  IF (ToCellAge.GE.0) THEN
                    DispPattern(i,Yinit)=ToCellAge
                    IF (earth(i,Yinit).NE.ToCellAge) THEN
                      ChangeMap(i,Yinit)=1
                    END IF
                  ELSE
                    ChangeMap(i,Yinit)=1
                    DispPattern(i,Yinit)=earth(i,Yinit)+1
                  END IF
                END DO
              END IF
              IF (Xfinal.NE.Xprev.OR.&
                  (Yfinal.GT.Yinit.AND.Yfinal.GT.Yprev)) THEN
                IF (Xfinal.NE.Xprev) THEN
                  temp=Yinit
                ELSE
                  IF (Yprev.GT.Yinit) THEN
                    temp=Yprev
                  ELSE
                    temp=Yinit
                  END IF
                END IF
                DO j=temp,Yfinal,1
                  IF (ToCellAge.GE.0) THEN
                    DispPattern(Xfinal,j)=ToCellAge
                    IF (earth(Xfinal,j).NE.ToCellAge) THEN
                      ChangeMap(Xfinal,j)=1
                    END IF
                  ELSE
                    ChangeMap(Xfinal,j)=1
                    DispPattern(Xfinal,j)=earth(Xfinal,j)+1
                  END IF
                END DO
              END IF
              IF ((Yfinal.GT.Yinit.AND.Yfinal.GT.Yprev).OR.Xprev.EQ.Xinit) THEN
                IF (Xprev.EQ.Xinit) THEN
                  temp=Yinit
                ELSE
                  IF (Yprev.GT.Yinit) THEN
                    temp=Yprev
                  ELSE
                    temp=Yinit
                  END IF
                END IF
                DO j=temp,Yfinal,1
                  IF (ToCellAge.GE.0) THEN
                    DispPattern(Xinit,j)=ToCellAge
                    IF (earth(Xinit,j).NE.ToCellAge) THEN
                      ChangeMap(Xinit,j)=1
                    END IF
                  ELSE
                    ChangeMap(Xinit,j)=1
                    DispPattern(Xinit,j)=earth(Xinit,j)+1
                  END IF
                END DO
              END IF
              CALL ShowChangePattern(DispPattern,earth,ChangeMap)
              Xprev=Xfinal
              Yprev=Yfinal
            END IF
          CASE (MouseButDown)
            IF (MESSAGE%VALUE1.EQ.CancelButton) THEN
              DoneDrop=-1
            END IF
          CASE (MouseButUp)
            DoneDrop=1
          CASE (KeyDown)
            SELECT CASE (MESSAGE%Value1)
              CASE (KeyDelete)
                IF (DeletePressed.EQ.1) THEN
                  DoneDrop=1
                ELSE
                  IF (ToCellAge.NE.0) THEN
                    OrigToAge=ToCellAge
                    ToCellAge=0
                  ELSE
                    ToCellAge=OrigToAge
                  END IF
                END IF
                DeletePressed=1
              CASE (KeyEscape)
                DoneDrop=-1
            END SELECT
        END SELECT
      END DO
      SELECT CASE (DoneDrop)
        CASE (-1)
          DO i=Xinit,Xfinal,1
            ChangeMap(i,Yinit)=1
            ChangeMap(i,Yfinal)=1
          END DO
            DO j=Yinit,Yfinal,1
            ChangeMap(Xinit,j)=1
            ChangeMap(Xfinal,j)=1
          END DO
          CALL UpdateLifeBoard(earth,ChangeMap)
        CASE (1)
          IF (IsMoved.EQ.1) THEN
            IF (Xfinal.GE.1.AND.Xfinal.LE.80.AND.Yfinal.GE.0.AND.Yfinal.LE.60) THEN
              DO i=Xinit,Xfinal,1
                ChangeMap(i,Yinit)=1
                ChangeMap(i,Yfinal)=1
                earth(i,Yinit)=ABS(DispPattern(i,Yinit))
                earth(i,Yfinal)=ABS(DispPattern(i,Yfinal))
              END DO
              DO j=Yinit,Yfinal,1
                ChangeMap(Xinit,j)=1
                ChangeMap(Xfinal,j)=1
                earth(Xinit,j)=ABS(DispPattern(Xinit,j))
                earth(Xfinal,j)=ABS(DispPattern(Xfinal,j))
              END DO
              CALL UpdateLifeBoard(earth,ChangeMap)
            END IF
          ELSE
            IF (ToCellAge.GE.0) THEN
              earth(Xinit,Yinit)=ToCellAge
            ELSE
              earth(Xinit,Yinit)=earth(Xinit,Yinit)+1
            END IF
            CALL DrawCell(Xinit,Yinit,earth(Xinit,Yinit))
          END IF
      END SELECT
      CALL WMessageEnable(MouseMove,0)
      CALL WMessageEnable(MouseButUp,0)
      RETURN
      END SUBROUTINE

      SUBROUTINE StretchCellBoundingBox(earth,ChangeMap,Xinit,Yinit,CancelButton)
      USE WINTERACTER
      USE FILLSTYLES
      USE TRANSFORMTYPES
      IMPLICIT NONE
      INCLUDE 'resource.INC'
      INTEGER, DIMENSION(0:81,0:61) :: earth
      INTEGER, DIMENSION(1:80,1:60) :: ChangeMap
      INTEGER Xinit,Yinit,Xfinal,Yfinal,Xprev,Yprev
      INTEGER icode,DONE
      INTEGER CancelButton,IsMoved,DoneDrag,MoveBox
      INTEGER isSquareBox
      INTEGER FillOp,FillMode
      CHARACTER (LEN=5) :: PosString
      TYPE(WIN_MESSAGE) :: MESSAGE
      IsMoved=0
      isSquareBox=0
      CALL WMessageEnable(MouseMove,1)
      CALL WMessageEnable(MouseButUp,1)
      Xprev=Xinit
      Yprev=Yinit
      Xfinal=Yinit
      Yfinal=Yinit
      DONE=0
      DoneDrag=0
      DO WHILE (DONE.EQ.0)
        MoveBox=0
        CALL WMessage(icode,MESSAGE)
        SELECT CASE (icode)
          CASE (Expose)
            CALL ReDrawUnderExpose(earth,MESSAGE)
            MoveBox=1
          CASE (CloseRequest)
            IF (MESSAGE%Win.EQ.0) THEN
              CALL TerminateLifeNicely
              STOP
            END IF
          CASE (MouseMove)
            IF (DoneDrag.EQ.0) THEN
              CALL GetCellPos(MESSAGE%X,MESSAGE%Y,Xfinal,Yfinal)
              MoveBox=1
            END IF
          CASE (MouseButDown)
            IF (MESSAGE%Value1.NE.CancelButton) THEN
              IF (DoneDrag.EQ.1) THEN
                CALL WMessageEnable(MouseButUp,1)
                DoneDrag=0
                MoveBox=1
              END IF
            ELSE
              IF (DoneDrag.EQ.0) THEN
                isSquareBox=1
              ELSE
                DONE=1
              END IF
              MoveBox=1
            END IF
          CASE (MouseButUp)
            IF (MESSAGE%VALUE1.NE.CancelButton) THEN
              IF (IsMoved.EQ.1) THEN
                CALL WMessageEnable(MouseButUp,0)
                DoneDrag=1
              ELSE
                MoveBox=1
                DONE=1
              END IF
            ELSE
              IF (DoneDrag.EQ.0) THEN
                isSquareBox=0
                MoveBox=1
              END IF
            END IF
          CASE (KeyDown)
            SELECT CASE (MESSAGE%Value1)
              CASE (KeyEscape, KeyDelete)
                DONE=1
              CASE (KeyHome,KeyEnd)
                IF (isSquareBox.EQ.0) THEN
                  isSquareBox=1
                ELSE
                  isSquareBox=0
                END IF
              CASE (KeyCursorRight)
                IF (Xfinal.LT.80) THEN
                  Xfinal=Xfinal+1
                END IF
              CASE (KeyCursorLeft)
                IF (Xfinal.GT.1) THEN
                  Xfinal=Xfinal-1
                END IF
              CASE (KeyCursorUp)
                IF (Yfinal.LT.60) THEN
                  Yfinal=Yfinal+1
                END IF
              CASE (KeyCursorDown)
                IF (Yfinal.GT.1) THEN
                  Yfinal=Yfinal-1
                END IF
              CASE (Keypad2)
                IF (Yinit.GT.1) THEN
                  Yinit=Yinit-1
                END IF
              CASE (Keypad4)
                IF (Xinit.GT.1) THEN
                  Xinit=Xinit-1
                END IF
              CASE (Keypad6)
                IF (Xinit.LT.80) THEN
                  Xinit=Xinit+1
                END IF
              CASE (Keypad8)
                IF (Yinit.LT.60) THEN
                  Yinit=Yinit+1
                END IF
              CASE DEFAULT
            END SELECT
            MoveBox=1
          CASE (PushButton)
            IF (MESSAGE%Value1.EQ.2) THEN
              IF (MESSAGE%Win.EQ.0) THEN
                CALL TerminateLifeNicely
                STOP
              ELSE
                CALL WDialogSelect(MESSAGE%Win)
                CALL WDialogHide()
              END IF
            END IF
            IF (MESSAGE%Win.EQ.IDD_DIALOG15) THEN
              SELECT CASE (MESSAGE%VALUE1)
                CASE (IDRotAreaCClock)
                  CALL TransfromCellArea(earth,ChangeMap,Xinit,Yinit,Xfinal,Yfinal,ROTATECCLOCK)
                  MoveBox=1
                  DoneDrag=1
                CASE (IDRotAreaClock)
                  CALL TransfromCellArea(earth,ChangeMap,Xinit,Yinit,Xfinal,Yfinal,ROTATECLOCK)
                  MoveBox=1
                  DoneDrag=1
                CASE (IDFlipAreaVert)
                  CALL TransfromCellArea(earth,ChangeMap,Xinit,Yinit,Xfinal,Yfinal,FLIPVERT)
                  MoveBox=1
                  DoneDrag=1
                CASE (IDFlipAreaHoriz)
                  CALL TransfromCellArea(earth,ChangeMap,Xinit,Yinit,Xfinal,Yfinal,FLIPHORIZ)
                  MoveBox=1
                  DoneDrag=1
                CASE (IDSelectCancel)
                  MoveBox=1
                  DoneDrag=0
                  DONE=1
                CASE (IDSaveAreaBut)
                  CALL SaveCellAreaAlt(earth,Xinit,Yinit,Xfinal,Yfinal)
                  MoveBox=1
                  DoneDrag=0
                  DONE=1
                CASE (IDFillAreaToBut)
                  CALL WDialogGetMenu(IDChangeAgeMenu,FillOp)
                  FillOp=FILLMINUS-FillOp+1
                  CALL WDialogGetMenu(IDFillAreaToMenu,FillMode)
                  SELECT CASE (FillMode)
                    CASE (2)
                      FillMode=FILLTODEAD
                    CASE (1)
                      FillMode=FILLTOMIN
                    CASE (13)
                      FillMode=FILLTOAVERAGE
                    CASE (11)
                      FillMode=FILLTOMAX
                    CASE (14)
                      FillMode=FILLTOREVERSE
                    CASE (15)
                      FillMode=FILLTOTILE
                    CASE (12)
                      FillMode=FILLNOTDEFINED
                    CASE (3,4,5,6,7,8,9,10)
                      FillMode=FillMode-2
                    CASE DEFAULT
                      CALL WMessageBox(OKOnly,InformationIcon,CommonOK,'Feature Not Yet Supported','Sorry')
                  END SELECT
                  CALL FillCellAreaTo(earth,ChangeMap,Xinit,Yinit,Xfinal,Yfinal,FillOp,FillMode)
                  MoveBox=1
                  DoneDrag=0
                  DONE=1
              END SELECT
            END IF
        END SELECT
        IF (DONE.EQ.1) THEN
          CALL DrawFinderAroundCells(Xinit,Yinit,Xprev,Yprev,0,208)
        ELSE IF (MoveBox.EQ.1) THEN
          MoveBox=0
          IF (isSquareBox.EQ.1) THEN
            IF ((Xfinal-Xinit).GT.(Yfinal-Yinit)) THEN
              Xfinal=Yfinal-Yinit+Xinit
            ELSE
              Yfinal=Xfinal-Xinit+Yinit
            END IF
          END IF
          IF (Xfinal.LE.80 .AND. Xfinal.GE.1 .AND. Yfinal.LE.60.AND.Yfinal.GE.1) THEN
            IF (Xfinal.NE.Xprev.OR.Yfinal.NE.Yprev) THEN
              CALL DrawFinderAroundCells(Xinit,Yinit,Xprev,Yprev,0,208)
              CALL DrawFinderAroundCells(Xinit,Yinit,Xfinal,Yfinal,1,176)
              CALL WDialogSelect(IDD_DIALOG15)
              IF (IsMoved.EQ.0) THEN
                IsMoved=1
                CALL WDialogShow(100,1,0,2)
              END IF
              IF (Xfinal-Xinit+1.GT.1) THEN
                CALL IntegerToString(Xfinal-Xinit+1,PosString,'(I5)')
                CALL WDialogPutString(IDPatXLab,PosString)
              END IF
              IF (Yfinal-Yinit+1.GT.1) THEN
                CALL IntegerToString(Yfinal-Yinit+1,PosString,'(I5)')
                CALL WDialogPutString(IDPatYLab,PosString)
              END IF
              Xprev=Xfinal
              Yprev=Yfinal
            END IF
          END IF
        END IF
      END DO
      CALL WMessageEnable(MouseMove,0)
      CALL WMessageEnable(MouseButUp,0)
      IF (IsMoved.EQ.1) THEN
        CALL WDialogSelect(IDD_DIALOG15)
        CALL WDialogHide()
      END IF
      RETURN
      END SUBROUTINE

      SUBROUTINE TransfromCellArea(earth,ChangeMap,X1,Y1,X2,Y2,TransformOp)
      USE TRANSFORMTYPES
      INTEGER, DIMENSION(0:81,0:61) :: earth
      INTEGER, DIMENSION(1:80,1:60) :: ChangeMap
      INTEGER X1,Y1,X2,Y2,Xinit,Yinit,Xfinal,Yfinal,TransformOp
      INTEGER i,j
      CHARACTER (LEN=5) :: TypeString
      Xinit=X1
      Yinit=Y1
      Xfinal=X2
      Yfinal=Y2
      IF (Xinit.GT.Xfinal) THEN
        CALL SwapINTS(Xinit,Xfinal)
      END IF
      IF (Yinit.GT.Yfinal) THEN
        CALL SwapINTS(Yinit,Yfinal)
      END IF
      SELECT CASE (TransformOp)
        CASE (FLIPVERT)
          DO j=Yinit,(Yfinal-Yinit)/2
            DO i=Xinit,Xfinal
              IF (earth(i,j).NE.earth(i,Yfinal-j)) THEN
                CALL SwapINTS(earth(i,j),earth(i,Yfinal-j))
                ChangeMap(i,j)=1
              END IF
            END DO
          END DO
          CALL UpdateLifeBoard(earth,ChangeMap)
        CASE (FLIPHORIZ)
          DO i=Xinit,(Xfinal-Xinit)/2
            DO j=Yinit,Yfinal
              IF (earth(i,j).NE.earth(i,Yfinal-j)) THEN
                CALL SwapINTS(earth(i,j),earth(Xfinal-i,j))
                ChangeMap(i,j)=1
              END IF
            END DO
          END DO
          CALL UpdateLifeBoard(earth,ChangeMap)
        CASE DEFAULT
          WRITE(TypeString,'(I5)') TransformOp
          CALL HandleLifeError('Transform Cell Area','Unknown Transform Type: ' // TypeString)
      END SELECT
      RETURN
      END SUBROUTINE

      SUBROUTINE FillCellAreaTo(earth,ChangeMap,X1,Y1,X2,Y2,FillOp,FillMode)
      USE FILLSTYLES
      INTEGER, DIMENSION(0:81,0:61) :: earth
      INTEGER, DIMENSION(1:80,1:60) :: ChangeMap
      INTEGER X1,Y1,X2,Y2,Xinit,Yinit,Xfinal,Yfinal,FillOp,FillMode
      INTEGER i,j,MAXage,MINage,AVEage,SUMage
      CHARACTER (LEN=5) :: TypeString
      Xinit=X1
      Yinit=Y1
      Xfinal=X2
      Yfinal=Y2
      IF (Xinit.GT.Xfinal) THEN
        CALL SwapINTS(Xinit,Xfinal)
      END IF
      IF (Yinit.GT.Yfinal) THEN
        CALL SwapINTS(Yinit,Yfinal)
      END IF
      SELECT CASE (FillMode)
        CASE(FILLTODEAD)
          DO i=Xinit,Xfinal
            DO j=Yinit,Yfinal
              IF (earth(i,j).NE.0) THEN
                earth(i,j)=0
                ChangeMap(i,j)=1
              END IF
            END DO
          END DO
          CALL UpdateLifeBoard(earth,ChangeMap)
        CASE (FILLTOMIN)
          MINage=earth(Xinit,Yinit)
          DO i=Xinit,Xfinal
            DO j=Yinit,Yfinal
              IF (earth(i,j).LT.MINage) THEN
                MINage=earth(i,j)
              END IF
            END DO
          END DO
          SELECT CASE (FillOp)
            CASE (FILLMINUS)
              DO i=Xinit,Xfinal
                DO j=Yinit,Yfinal
                  IF (earth(i,j)-MINage.GT.0) THEN
                    earth(i,j)=earth(i,j)-MINAge
                    ChangeMap(i,j)=1
                  ELSE
                    IF (earth(i,j).NE.0) THEN
                      earth(i,j)=0
                      ChangeMap(i,j)=1
                    END IF
                  END IF
                END DO
              END DO
              CALL UpdateLifeBoard(earth,ChangeMap)
            CASE (FILLEQUAL)
              DO i=Xinit,Xfinal
                DO j=Yinit,Yfinal
                  IF (earth(i,j).NE.MINAge) THEN
                    earth(i,j)=MINAge
                    ChangeMap(i,j)=1
                  END IF
                END DO
              END DO
              CALL UpdateLifeBoard(earth,ChangeMap)
            CASE (FILLPLUS)
              DO i=Xinit,Xfinal
                DO j=Yinit,Yfinal
                  IF (earth(i,j)+MINage.LE.32760) THEN
                    earth(i,j)=earth(i,j)+MINage
                    ChangeMap(i,j)=1
                  END IF
                END DO
              END DO
              CALL UpdateLifeBoard(earth,ChangeMap)
            CASE DEFAULT
              WRITE(TypeString,'(I5)') FillOp
              CALL HandleLifeError('Fill Cell Area','Unknown Fill Operation: ' // TypeString)
          END SELECT
        CASE(FILLTOAVERAGE)
          SUMage=SUM(earth(Xinit:Xfinal,Yinit:Yfinal))
          AVEage=SUMage/((Xfinal-Xinit)*(Xfinal-Yinit))
          SELECT CASE (FillOp)
            CASE (FILLMINUS)
              DO i=Xinit,Xfinal
                DO j=Yinit,Yfinal
                  IF (earth(i,j)-AVEage.GT.0) THEN
                    earth(i,j)=earth(i,j)-AVEAge
                    ChangeMap(i,j)=1
                  ELSE
                    IF (earth(i,j).NE.0) THEN
                      earth(i,j)=0
                      ChangeMap(i,j)=1
                    END IF
                  END IF
                END DO
              END DO
              CALL UpdateLifeBoard(earth,ChangeMap)
            CASE (FILLEQUAL)
              DO i=Xinit,Xfinal
                DO j=Yinit,Yfinal
                  IF (earth(i,j).NE.AVEAge) THEN
                    earth(i,j)=AVEAge
                    ChangeMap(i,j)=1
                  END IF
                END DO
              END DO
              CALL UpdateLifeBoard(earth,ChangeMap)
            CASE (FILLPLUS)
              DO i=Xinit,Xfinal
                DO j=Yinit,Yfinal
                  IF (earth(i,j)+AVEage.LE.32760) THEN
                    earth(i,j)=earth(i,j)+AVEage
                    ChangeMap(i,j)=1
                  END IF
                END DO
              END DO
              CALL UpdateLifeBoard(earth,ChangeMap)
            CASE DEFAULT
              WRITE(TypeString,'(I5)') FillOp
              CALL HandleLifeError('Fill Cell Area','Unknown Fill Operation: ' // TypeString)
          END SELECT
        CASE (FILLTOMODE)
        CASE (FILLTOMAX)
          MAXage=earth(Xinit,Yinit)
          DO i=Xinit,Xfinal
            DO j=Yinit,Yfinal
              IF (earth(i,j).GT.MAXage) THEN
                MAXage=earth(i,j)
              END IF
            END DO
          END DO
          SELECT CASE (FillOp)
            CASE (FILLMINUS)
              DO i=Xinit,Xfinal
                DO j=Yinit,Yfinal
                  IF (earth(i,j).LT.MAXage) THEN
                    earth(i,j)=0
                  ELSE
                    earth(i,j)=1
                  END IF
                  ChangeMap(i,j)=1
                END DO
              END DO
              CALL UpdateLifeBoard(earth,ChangeMap)
            CASE (FILLEQUAL)
              DO i=Xinit,Xfinal
                DO j=Yinit,Yfinal
                  IF (earth(i,j).NE.MAXage) THEN
                    earth(i,j)=MAXage
                    ChangeMap(i,j)=1
                  END IF
                END DO
              END DO
              CALL UpdateLifeBoard(earth,ChangeMap)
            CASE (FILLPLUS)
              DO i=Xinit,Xfinal
                DO j=Yinit,Yfinal
                  IF (earth(i,j).LT.32760-MAXage) THEN
                    earth(i,j)=earth(i,j)+MAXage
                  ELSE
                    earth(i,j)=1
                  END IF
                  ChangeMap(i,j)=1
                END DO
              END DO
              CALL UpdateLifeBoard(earth,ChangeMap)
            CASE DEFAULT
              WRITE(TypeString,'(I5)') FillOp
              CALL HandleLifeError('Fill Cell Area','Unknown Fill Operation: ' // TypeString)
          END SELECT
        CASE (1,2,3,4,5,6,7,8)
          SELECT CASE (FillOp)
            CASE (FILLMINUS)
              DO i=Xinit,Xfinal
                DO j=Yinit,Yfinal
                  IF (earth(i,j)-FillMode.GT.0) THEN
                    earth(i,j)=earth(i,j)-FillMode
                    ChangeMap(i,j)=1
                  ELSE
                    IF (earth(i,j).NE.0) THEN
                      earth(i,j)=0
                      ChangeMap(i,j)=1
                    END IF
                  END IF
                END DO
              END DO
              CALL UpdateLifeBoard(earth,ChangeMap)
            CASE (FILLEQUAL)
              DO i=Xinit,Xfinal
                DO j=Yinit,Yfinal
                  IF (earth(i,j).NE.FillMode) THEN
                    earth(i,j)=FillMode
                    ChangeMap(i,j)=1
                  END IF
                END DO
              END DO
              CALL UpdateLifeBoard(earth,ChangeMap)
            CASE (FILLPLUS)
              DO i=Xinit,Xfinal
                DO j=Yinit,Yfinal
                  IF (earth(i,j)+FillMode.LE.32760) THEN
                    earth(i,j)=earth(i,j)+FillMode
                    ChangeMap(i,j)=1
                  END IF
                END DO
              END DO
              CALL UpdateLifeBoard(earth,ChangeMap)
            CASE DEFAULT
              WRITE(TypeString,'(I5)') FillOp
              CALL HandleLifeError('Fill Cell Area','Unknown Fill Operation: ' // TypeString)
          END SELECT
        CASE (FILLNOTDEFINED)
        CASE DEFAULT
          WRITE(TypeString,'(I5)') FillMode
          CALL HandleLifeError('Fill Cell Area','Unknown Fill Style: ' // TypeString)
      END SELECT
      RETURN
      END SUBROUTINE

      SUBROUTINE SaveCellAreaAlt(earth,Xinit,Yinit,Xfinal,Yfinal)
      USE WINTERACTER
      IMPLICIT NONE
      INCLUDE 'resource.INC'
      INTEGER, DIMENSION(0:81,0:61) :: earth
      INTEGER Xinit,Yinit,Xfinal,Yfinal
      CHARACTER (LEN=255) :: outfilename
      CHARACTER (LEN=80)  :: ANewLineOfFile
      CHARACTER (LEN=255) :: PatternName
      LOGICAL fexiststat
      INTEGER i,j,DONE
      SAVE outfilename
      DATA outfilename /'D:\epley\temp\life\lifepat.gol.txt'/
      CALL WDialogSelect(IDD_DIALOG05)
      CALL WDialogShow(100,0,0,1)
      SELECT CASE (WInfoDialog(ExitButton))
        CASE (IDPatSaveIt)
          CALL WDialogGetString(IDSavePatName,PatternName)
! Check for uniqueness of pattern name, ask to replace it if not unique
          INQUIRE(FILE=outfilename,EXIST=fexiststat)
          DONE=0
          IF (fexiststat) THEN
            i=i+1
          ELSE
            CALL WSelectFile(IDFileFilter,SaveDialog+PromptOn+DirChange,outfilename,'Pattern File')
            IF (WInfoDialog(ExitButton).NE.1) THEN
              DONE=1
            END IF
          END IF
          IF (DONE.EQ.0) THEN
            IF (Xinit.GT.Xfinal) THEN
              i=Xinit
              Xinit=Xfinal
              Xfinal=i
            END IF
            IF (Yinit.GT.Yfinal) THEN
              j=Yinit
              Yinit=Yfinal
              Yfinal=j
            END IF
            OPEN(UNIT=101,FILE=outfilename,STATUS='OLD',POSITION='APPEND')
            CALL WDialogGetString(IDSavePatName,PatternName)
            WRITE(101,*) '#',PatternName
            WRITE(101,'(I5.5)') (Yfinal-Yinit+1)
            DO j=Yfinal,Yinit,-1
              DO i=1,(Xfinal-Xinit+1),1
                IF (earth(i+Xinit-1,j).GT.0) THEN
                  CALL DrawCell(i+Xinit-1,j,0)
                  ANewLineOfFile(i:i)='*'
                ELSE
                  ANewLineOfFile(i:i)='.'
                CALL DrawCell(i+Xinit-1,j,1)
                END IF
              END DO
              WRITE(101,*) ANewLineOfFile(1:(Xfinal-Xinit+1))
            END DO
            CLOSE(101)
          END IF
      END SELECT
      CALL WDialogSelect(IDD_DIALOG05)
      CALL WDialogHide()
      RETURN
      END SUBROUTINE

      SUBROUTINE SaveCellArea(earth,Xinit,Yinit,CancelButton)
      USE WINTERACTER
      IMPLICIT NONE
      INCLUDE 'resource.INC'
      INTEGER, DIMENSION(0:81,0:61) :: earth
      INTEGER Xinit,Yinit,Xfinal,Yfinal,Xprev,Yprev
      CHARACTER (LEN=255) :: outfilename
      CHARACTER (LEN=80)  :: ANewLineOfFile
      CHARACTER (LEN=255) :: PatternName,PosString
      LOGICAL fexiststat
      INTEGER i,j,icode,DONE
      INTEGER CancelButton,IsMoved,DoneDrag
      TYPE(WIN_MESSAGE) :: MESSAGE
      SAVE outfilename
      DATA outfilename /'D:\epley\temp\life\lifepat.gol.txt'/
      IsMoved=0
      CALL WMessageEnable(MouseMove,1)
      CALL WMessageEnable(MouseButUp,1)
      Xprev=Xinit
      Yprev=Yinit
      Xfinal=Yinit
      Yfinal=Yinit
      DONE=0
      DoneDrag=0
      DO WHILE (DONE.EQ.0)
        CALL WMessage(icode,MESSAGE)
        SELECT CASE (icode)
          CASE (Expose)
            CALL ReDrawUnderExpose(earth,MESSAGE)
          CASE (CloseRequest)
            IF (MESSAGE%Win.EQ.0) THEN
              CALL TerminateLifeNicely
              STOP
            END IF
          CASE (MouseMove,MouseButDown)
            IF (DoneDrag.EQ.1.AND.icode.EQ.MouseButDown.AND.MESSAGE%VALUE1.NE.CancelButton) THEN
              CALL WMessageEnable(MouseButUp,1)
              DoneDrag=0
            END IF
            IF (DoneDrag.EQ.0) THEN
              CALL GetCellPos(MESSAGE%X,MESSAGE%Y,Xfinal,Yfinal)
              IF (Xfinal.LE.80 .AND. Xfinal.GE.1 .AND. Yfinal.LE.60.AND.Yfinal.GE.1) THEN
                IF (Xfinal.NE.Xprev.OR.Yfinal.NE.Yprev) THEN
                  CALL DrawFinderAroundCells(Xinit,Yinit,Xprev,Yprev,0,208)
                  CALL DrawFinderAroundCells(Xinit,Yinit,Xfinal,Yfinal,1,176)
                  IF (IsMoved.EQ.0) THEN
                    IsMoved=1
                    CALL WDialogLoad(IDD_DIALOG05)
                    CALL WDialogShow(100,1,0,2)
                  END IF
                  IF (Xfinal-Xinit+1.GT.1) THEN
                    CALL IntegerToString(Xfinal-Xinit+1,PosString,'(I5)')
                    CALL WDialogPutString(IDPatXLab,PosString)
                  END IF
                  IF (Yfinal-Yinit+1.GT.1) THEN
                    CALL IntegerToString(Yfinal-Yinit+1,PosString,'(I5)')
                    CALL WDialogPutString(IDPatYLab,PosString)
                  END IF
                  Xprev=Xfinal
                  Yprev=Yfinal
                END IF
              END IF
            END IF
          CASE (MouseButUp)
            IF (MESSAGE%VALUE1.EQ.CancelButton) THEN
              CALL DrawFinderAroundCells(Xinit,Yinit,Xfinal,Yfinal,0,208)
              DONE=1
            ELSE
              IF (IsMoved.EQ.1) THEN
                DoneDrag=1
                CALL WMessageEnable(MouseButUp,0)
              ELSE
                CALL DrawFinderAroundCells(Xinit,Yinit,Xfinal,Yfinal,0,208)
                DONE=1
              END IF
            END IF
          CASE (PushButton)
            SELECT CASE (MESSAGE%VALUE1)
              CASE (2)
                IF (MESSAGE%Win.EQ.0) THEN
                  CALL TerminateLifeNicely
                  STOP
                ELSE
                  CALL WDialogSelect(MESSAGE%Win)
                  CALL WDialogHide()
                END IF
              CASE (IDCancelSave)
                DONE=1
              CASE (IDPatSaveIt)
                CALL WDialogGetString(IDSavePatName,PatternName)
! Check for uniqueness of pattern name, ask to replace it if not unique
                INQUIRE(FILE=outfilename,EXIST=fexiststat)
                IF (fexiststat) THEN
                  i=i+1
                ELSE
                  CALL WSelectFile(IDFileFilter,SaveDialog+PromptOn+DirChange,outfilename,'Pattern File')
                  IF (WInfoDialog(ExitButton).NE.1) THEN
                    DONE=1
                  END IF
                END IF
                IF (DONE.EQ.0) THEN
                  IF (Xinit.GT.Xfinal) THEN
                    i=Xinit
                    Xinit=Xfinal
                    Xfinal=i
                  END IF
                  IF (Yinit.GT.Yfinal) THEN
                    j=Yinit
                    Yinit=Yfinal
                    Yfinal=j
                  END IF
                  OPEN(UNIT=101,FILE=outfilename,STATUS='OLD',POSITION='APPEND')
                  CALL WDialogGetString(IDSavePatName,PatternName)
                  WRITE(101,*) '#',PatternName
                  WRITE(101,'(I5.5)') (Yfinal-Yinit+1)
                  DO j=Yfinal,Yinit,-1
                    DO i=1,(Xfinal-Xinit+1),1
                      IF (earth(i+Xinit-1,j).GT.0) THEN
                        CALL DrawCell(i+Xinit-1,j,0)
                        ANewLineOfFile(i:i)='*'
                      ELSE
                        ANewLineOfFile(i:i)='.'
                        CALL DrawCell(i+Xinit-1,j,1)
                      END IF
                    END DO
                    WRITE(101,*) ANewLineOfFile(1:(Xfinal-Xinit+1))
                  END DO
                  CLOSE(101)
                END IF
                CALL DrawFinderAroundCells(Xinit,Yinit,Xfinal,Yfinal,0,208)
                DONE=1
            END SELECT
        END SELECT
      END DO
      CALL WMessageEnable(MouseMove,0)
      CALL WMessageEnable(MouseButUp,0)
      IF (IsMoved.EQ.1) THEN
        CALL WDialogUnload()
      END IF
      RETURN
      END SUBROUTINE

      SUBROUTINE SWAPINTS(i,j)
      IMPLICIT NONE
      INTEGER i,j,temp
      temp=i
      i=j
      j=temp
      RETURN
      END SUBROUTINE

      SUBROUTINE SWAPREALS(i,j)
      IMPLICIT NONE
      REAL i,j,temp
      temp=i
      i=j
      j=temp
      RETURN
      END SUBROUTINE

      SUBROUTINE OrderInts(i,j)
      IMPLICIT NONE
      INTEGER i,j,temp
      IF (i.GT.j) THEN
        temp=i
        i=j
        j=temp
      END IF
      END SUBROUTINE

      SUBROUTINE HandleBoardMenu(earth,ChangeMap,counter,&
                                 mainX,mainY,mainWidth,mainHeight,OffsetY)
      USE WINTERACTER
      IMPLICIT NONE
      INCLUDE 'resource.INC'
      INTEGER, DIMENSION(0:81,0:61) :: earth
      INTEGER, DIMENSION(1:80,1:60) :: ChangeMap
      INTEGER counter
      INTEGER mainX,mainY,mainWidth,mainHeight,OffsetY
      INTEGER scrMaxX,scrMaxY
      INTEGER placeX,placeY
      INTEGER width,height
      INTEGER i,j,DONE,icode
      TYPE(WIN_Message) :: MESSAGE
      INTEGER, DIMENSION(1:80) :: temp
      CHARACTER (LEN=255) :: BoardFileName
      CALL WDialogSelect(IDD_DIALOG13)
      width=WInfoDialog(DialogWidth)
      height=WinfoDialog(DialogHeight)
      scrMaxX=WInfoScreen(ScreenWidth)
      scrMaxY=WInfoScreen(ScreenHeight)
      IF (mainX+mainWidth+width.GT.scrMaxX) THEN
        placeX=mainX-width
      ELSE
        placeX=mainX+mainWidth
      END IF
      IF (mainY+OffsetY+height.GT.scrMaxY) THEN
        placeY=mainY-height+OffsetY+20
        IF (placeY.LT.0) THEN
          placeY=0
        END IF
      ELSE
        placeY=mainY+OffsetY
      END IF
      CALL WDialogShow(placeX,placeY,0,2)
      CALL WMessageEnable(FieldChanged,1)
      CALL WMessageEnable(MouseMove,1)
      DONE=0
      DO WHILE (DONE.EQ.0)
        CALL WMessage(icode,MESSAGE)
        CALL WDialogSelect(IDD_DIALOG13)
        SELECT CASE (icode)
          CASE (Expose)
            CALL ReDrawUnderExpose(earth,MESSAGE)
          CASE (CloseRequest)
            IF (MESSAGE%Win.EQ.0) THEN
              CALL TerminateLifeNicely
              STOP
            END IF
          CASE (MouseMove)
            CALL WMessageEnable(FieldChanged,0)
            CALL WMessageEnable(MouseMove,0)
            CALL WDialogSelect(IDD_DIALOG13)
            CALL WDialogHide()
            DONE=1
          CASE (KeyDown)
            IF (MESSAGE%Value1.EQ.KeyEscape.OR.MESSAGE%Value1.EQ.KeyDelete) THEN
              CALL WMessageEnable(FieldChanged,0)
              CALL WMessageEnable(MouseMove,0)
              CALL WDialogSelect(IDD_DIALOG13)
              CALL WDialogHide()
              DONE=1
              CALL ShowMeXY(MESSAGE%X,MESSAGE%Y)
            END IF
          CASE (PushButton)
            IF (MESSAGE%Value1.EQ.2) THEN
              SELECT CASE (MESSAGE%Win)
                CASE (IDD_DIALOG13)
                  CALL WMessageEnable(FieldChanged,0)
                  CALL WMessageEnable(MouseMove,0)
                  CALL WDialogSelect(IDD_DIALOG13)
                  CALL WDialogHide()
                  DONE=1
                CASE DEFAULT
                  CALL WDialogSelect(MESSAGE%Win)
                  CALL WDialogHide()
                  CALL WDialogSelect(IDD_DIALOG13)
              END SELECT
            ELSE
              IF (MESSAGE%Win.EQ.IDD_DIALOG13) THEN
                SELECT CASE (MESSAGE%Value1)
                  CASE (IDSaveBoard)
                    CALL WMessageEnable(FieldChanged,0)
                    CALL WMessageEnable(MouseMove,0)
                    CALL WDialogSelect(IDD_DIALOG13)
                    CALL WDialogHide()
                    BoardFileName=' '
                    CALL WSelectFile(0,SaveDialog+PromptOn+DirChange,BoardFileName,'Save Board As')
                    IF (WInfoDialog(ExitButtonCommon).EQ.1) THEN
                      OPEN(UNIT=101,FILE=BoardFileName,STATUS='UNKNOWN')
                      WRITE(101,*) counter
                      DO j=1,60,1
                        WRITE(101,'(80(I3))') (earth(i,j),i=1,80)
                      END DO
                      CLOSE(101)
                    END IF
                    DONE=1
                  CASE (IDLoadBoard)
                    CALL WMessageEnable(FieldChanged,0)
                    CALL WMessageEnable(MouseMove,0)
                    CALL WDialogSelect(IDD_DIALOG13)
                    CALL WDialogHide()
                    BoardFileName=' '
                    CALL WSelectFile(0,LoadDialog+PromptOn+DirChange,BoardFileName,'Save Board As')
                    IF (WInfoDialog(ExitButtonCommon).EQ.1) THEN
                      OPEN(UNIT=101,FILE=BoardFileName,STATUS='OLD')
                      READ(101,*) counter
                      DO j=1,60,1
                        READ(101,*) (temp(i),i=1,80)
                        DO i=1,80,1
                          IF (earth(i,j).NE.temp(i)) THEN
                            earth(i,j)=temp(i)
                            ChangeMap(i,j)=1
                          ELSE
                            ChangeMap(i,j)=0
                          END IF
                        END DO
                      END DO
                      CLOSE(101)
                      CALL UpdateCounterDialog(counter)
                      CALL UpdateLifeBoard(earth,ChangeMap)
                    END IF
                    DONE=1
                  CASE (IDCLEAR)
                    CALL WMessageEnable(FieldChanged,0)
                    CALL WMessageEnable(MouseMove,0)
                    CALL WDialogSelect(IDD_DIALOG13)
                    CALL WDialogHide()
                    counter=0
                    CALL UpdateCounterDialog(counter)
                    DO i=1,80,1
                      DO j=1,60,1
                        earth(i,j)=0
                      END DO
                    END DO
                    CALL DrawFastLifeBoard
                    DONE=1
                  CASE (IDRANDOM)
                    CALL WMessageEnable(FieldChanged,0)
                    CALL WMessageEnable(MouseMove,0)
                    CALL WDialogSelect(IDD_DIALOG13)
                    CALL WDialogHide()
                    counter=0
                    CALL UpdateCounterDialog(counter)
                    CALL InitRandomEarth(earth,ChangeMap)
                    CALL UpdateLifeBoard(earth,ChangeMap)
                    DONE=1
                  CASE DEFAULT
                    CALL WMessageEnable(FieldChanged,0)
                    CALL WMessageEnable(MouseMove,0)
                    CALL WMessageBox(OKOnly,InformationIcon,CommonOK,'Feature Not Yet Supported','Sorry')
                    CALL WDialogSelect(IDD_DIALOG13)
                    CALL WMessageEnable(FieldChanged,1)
                    CALL WMessageEnable(MouseMove,1)
                END SELECT
              END IF
            END IF
          CASE (FieldChanged)
            IF (MESSAGE%Win.NE.IDD_DIALOG13.AND.MESSAGE%Value2.NE.IDBoard) THEN
              CALL WMessageEnable(FieldChanged,0)
              CALL WMessageEnable(MouseMove,0)
              CALL WDialogSelect(IDD_DIALOG13)
              CALL WDialogHide()
              DONE=1
            ELSE
              IF (MESSAGE%Value1.EQ.MESSAGE%Value2.AND.MESSAGE%Win.EQ.IDD_DIALOG13) THEN
                SELECT CASE (MESSAGE%Value2)
                  CASE (IDSaveBoard)
                  CASE (IDLoadBoard)
                  CASE (IDCLEAR)
                  CASE (IDRANDOM)
                  CASE DEFAULT
                    CALL WMessageEnable(FieldChanged,0)
                    CALL WMessageEnable(MouseMove,0)
                    CALL HandleDialogError('Error')
                    CALL WDialogSelect(IDD_DIALOG13)
                    CALL WMessageEnable(MouseMove,1)
                    CALL WMessageEnable(FieldChanged,1)
                    icode=NoMessage
                END SELECT
              END IF
            END IF
        END SELECT
      END DO
      RETURN
      END SUBROUTINE

      SUBROUTINE HandleOptionsMenu(earth,mainX,mainY,mainWidth,mainHeight,OffsetY)
      USE WINTERACTER
      IMPLICIT NONE
      INCLUDE 'resource.INC'
      INTEGER, DIMENSION(0:81,0:61) :: earth
      INTEGER mainX,mainY,mainWidth,mainHeight,OffsetY
      INTEGER scrMaxX,scrMaxY,X1,Y1,X2,Y2
      INTEGER placeX,placeY
      INTEGER width,height
      TYPE(WIN_Message) :: MESSAGE
      INTEGER icode,DONE
      INTEGER istate
      INTEGER PrevCellAge
      SAVE PrevCellAge
      DATA PrevCellAge /0/
      CALL WDialogSelect(IDD_DIALOG11)
      width=WInfoDialog(DialogWidth)
      height=WinfoDialog(DialogHeight)
      scrMaxX=WInfoScreen(ScreenWidth)
      scrMaxY=WInfoScreen(ScreenHeight)
      IF (mainX+mainWidth+width.GT.scrMaxX) THEN
        placeX=mainX-width
      ELSE
        placeX=mainX+mainWidth
      END IF
      IF (mainY+OffsetY+height.GT.scrMaxY) THEN
        placeY=mainY-height+OffsetY+20
        IF (placeY.LT.0) THEN
          placeY=0
        END IF
      ELSE
        placeY=mainY+OffsetY
      END IF
      CALL WDialogShow(placeX,placeY,0,2)
      CALL WMessageEnable(FieldChanged,1)
      CALL WMessageEnable(MouseMove,1)
      DONE=0
      DO WHILE (DONE.EQ.0)
        CALL WMessage(icode,MESSAGE)
        CALL WDialogSelect(IDD_DIALOG11)
        SELECT CASE (icode)
          CASE (Expose)
            CALL ReDrawUnderExpose(earth,MESSAGE)
          CASE (CloseRequest)
            IF (MESSAGE%Win.EQ.0) THEN
              CALL TerminateLifeNicely
              STOP
            END IF
          CASE (MouseMove)
            DONE=1
          CASE (KeyDown)
            IF (MESSAGE%Value1.EQ.KeyEscape.OR.MESSAGE%Value1.EQ.KeyDelete) THEN
              DONE=1
              CALL ShowMeXY(MESSAGE%X,MESSAGE%Y)
            END IF
          CASE (PushButton)
            IF (MESSAGE%Value1.EQ.2) THEN
              SELECT CASE (MESSAGE%Win)
                CASE (IDD_DIALOG03)
                  CALL WDialogSelect(IDD_DIALOG11)
                  CALL WDialogPutCheckBox(IDShowCounter,0)
                  CALL WDialogPutString(IDShowCounter,'Counter Box Off')
                  CALL WDialogSelect(MESSAGE%Win)
                  CALL WDialogHide()
                  CALL WDialogSelect(IDD_DIALOG11)
                CASE (IDD_DIALOG11)
                  DONE=1
                CASE DEFAULT
                  CALL WDialogSelect(MESSAGE%Win)
                  CALL WDialogHide()
                  CALL WDialogSelect(IDD_DIALOG11)
              END SELECT
            ELSE
              IF (MESSAGE%Win.EQ.IDD_DIALOG11) THEN
                SELECT CASE (MESSAGE%Value1)
                  CASE (IDOKOptions)
                    DONE=1
                  CASE (IDCancelOptions)
                    DONE=1
                  CASE (IDCounterXY)
                    CALL WMessageEnable(FieldChanged,0)
                    CALL WDialogSelect(IDD_DIALOG11)
                    X1=WinfoDialog(DialogXPos)
                    Y1=WinfoDialog(DialogYPos)
                    X2=WInfoDialog(DialogWidth)
                    Y2=WinfoDialog(DialogHeight)
                    Y2=Y2+1
                    scrMaxX=WInfoScreen(ScreenWidth)
                    scrMaxY=WInfoScreen(ScreenHeight)
                    width=80
                    height=60
                    IF (X1+X2+width.GT.scrMaxX) THEN
                      placeX=X1-width
                    ELSE
                      placeX=X1+X2
                    END IF
                    IF (Y1+45+height.GT.scrMaxY) THEN
                      placeY=Y1-height+45+20
                      IF (placeY.LT.0) THEN
                        placeY=0
                      END IF
                    ELSE
                      placeY=Y1+45
                    END IF
                    CALL SelectScreenXY(earth,X1,Y1,placeX,placeY)
                    CALL WDialogGetCheckBox(IDShowCounter,istate)
                    IF (istate.EQ.1.AND.X1.NE.-1.AND.Y1.NE.-1) THEN
                      CALL WDialogSelect(IDD_DIALOG03)
                      CALL WDialogHide()
                      CALL WDialogSelect(IDD_DIALOG03)
                      CALL WDialogShow(X1,Y1,0,2)
                      CALL WDialogSelect(IDD_DIALOG11)
                    END IF
                    CALL WMessageEnable(FieldChanged,1)
                  CASE (IDSetFileNames)
                    CALL WDialogSelect(IDD_DIALOG11)
                    CALL HandleFileOptions(earth,&
                                           WInfoDialog(DialogXPos),&
                                           WInfoDialog(DialogYPos),&
                                           WInfoDialog(DialogWidth),&
                                           WInfoDialog(DialogHeight),&
                                           146)
                    CALL WDialogSelect(IDD_DIALOG11)
!                  CASE (IDSaveOptions)
                  CASE DEFAULT
                    CALL WMessageEnable(FieldChanged,0)
                    CALL WMessageEnable(MouseMove,0)
                    CALL WMessageBox(OKOnly,InformationIcon,CommonOK,'Feature Not Yet Supported','Sorry')
                    CALL WDialogSelect(IDD_DIALOG11)
                    CALL WMessageEnable(MouseMove,1)
                    CALL WMessageEnable(FieldChanged,1)
                    icode=NoMessage
                END SELECT
              END IF
            END IF
          CASE (FieldChanged)
            IF (MESSAGE%Win.NE.IDD_DIALOG11.AND.MESSAGE%Value2.NE.IDOptions) THEN
              DONE=1
            ELSE
              IF (MESSAGE%Value1.EQ.MESSAGE%Value2.AND.MESSAGE%Win.EQ.IDD_DIALOG11) THEN
                SELECT CASE (MESSAGE%Value2)
                  CASE (IDOKOptions)
                  CASE (IDCancelOptions)
                  CASE (IDSaveOptions)
                  CASE (IDSetFileNames)
                  CASE (IDCounterXY)
                  CASE (IDGridState)
                    CALL WDialogGetCheckBox(MESSAGE%Value2,istate)
                    IF (istate.EQ.1) THEN
                      CALL WDialogPutString(MESSAGE%Value2,'Grid On')
                    ELSE
                      CALL WDialogPutString(MESSAGE%Value2,'Grid Off')
                    END IF
                  CASE (IDShowAge)
                    CALL WDialogGetCheckBox(MESSAGE%Value2,istate)
                    IF (istate.EQ.1) THEN
                      CALL WDialogPutString(MESSAGE%Value2,'Cell Age')
                      CALL WDialogGetMenu(IDShowAgeAs,PrevCellAge)
                      CALL WDialogPutOption(IDShowAgeAs,0)
                    ELSE
                      CALL WDialogPutString(MESSAGE%Value2,'Cells are')
                      IF (PrevCellAge.EQ.0) THEN
                        PrevCellAge=1
                      END IF
                      CALL WDialogPutOption(IDShowAgeAs,PrevCellAge)
                    END IF
                  CASE (IDShowAgeAs)
                    CALL WDialogGetMenu(IDShowAgeAs,istate)
                    PrevCellAge=istate
                    IF (istate.NE.0) THEN
                      CALL WDialogGetCheckBox(IDShowAge,istate)
                      IF (istate.EQ.1) THEN
                        CALL WDialogPutCheckBox(IDShowAge,0)
                        CALL WDialogPutString(IDShowAge,'Cells are')
                      END IF
                    END IF
                  CASE (IDShowCounter)
                    CALL WMessageEnable(FieldChanged,0)
                    CALL WMessageEnable(MouseMove,0)
                    CALL WDialogGetCheckBox(MESSAGE%Value2,istate)
                    IF (istate.EQ.1) THEN
!                      CALL WDialogSelect(IDD_DIALOG11)
!                      CALL WDialogPutCheckBox(MESSAGE%Value2,0)
                      CALL WDialogPutString(MESSAGE%Value2,'Counter Box On')
!                      CALL WDialogSelect(IDD_DIALOG03)
!                      CALL WDialogShow(-1,1,0,2)
!                      CALL WDialogSelect(IDD_DIALOG11)
                    ELSE
!                      CALL WDialogSelect(IDD_DIALOG11)
!                      CALL WDialogPutCheckBox(MESSAGE%Value2,1)
                      CALL WDialogPutString(MESSAGE%Value2,'Counter Box Off')
!                      CALL WDialogSelect(IDD_DIALOG03)
!                      CALL WDialogHide()
!                      CALL WDialogSelect(IDD_DIALOG11)
                    END IF
                    CALL WMessageEnable(MouseMove,1)
                    CALL WMessageEnable(FieldChanged,1)
                  CASE (IDAllowDeletePat)
                    CALL WDialogGetCheckBox(MESSAGE%Value2,istate)
                    IF (istate.EQ.1) THEN
                      CALL WDialogPutString(MESSAGE%Value2,'Allow Delete')
                    ELSE
                      CALL WDialogPutString(MESSAGE%Value2,'Disallow Delete')
                    END IF
                  CASE (IDProtectPatterns)
                    CALL WDialogGetCheckBox(MESSAGE%Value2,istate)
                    IF (istate.EQ.1) THEN
                      CALL WDialogPutString(MESSAGE%Value2,'Protect Original Patterns')
                    ELSE
                      CALL WDialogPutString(MESSAGE%Value2,'Don''t Protect Patterns')
                    END IF
                  CASE (IDFloatingHelp)
                    CALL WDialogGetCheckBox(MESSAGE%Value2,istate)
                    IF (istate.EQ.1) THEN
                      CALL WDialogPutString(MESSAGE%Value2,'Floating Help')
                    ELSE
                      CALL WDialogPutString(MESSAGE%Value2,'Static Help')
                    END IF
                  CASE DEFAULT
                    CALL WMessageEnable(FieldChanged,0)
                    CALL WMessageEnable(MouseMove,0)
                    CALL HandleDialogError('Error')
                    CALL WDialogSelect(IDD_DIALOG11)
                    CALL WMessageEnable(MouseMove,1)
                    CALL WMessageEnable(FieldChanged,1)
                    icode=NoMessage
                END SELECT
              END IF
            END IF
        END SELECT
      END DO
      CALL WMessageEnable(MouseMove,0)
      CALL WMessageEnable(FieldChanged,0)
      CALL WDialogSelect(IDD_DIALOG11)
      CALL WDialogHide()
      RETURN
      END SUBROUTINE

      SUBROUTINE HandleFileOptions(earth,mainX,mainY,mainWidth,mainHeight,OffsetY)
      USE WINTERACTER
      IMPLICIT NONE
      INCLUDE 'resource.INC'
      INTEGER, DIMENSION(0:81,0:61) :: earth
      INTEGER mainX,mainY,mainWidth,mainHeight,OffsetY
      INTEGER scrMaxX,scrMaxY
      INTEGER placeX,placeY
      INTEGER width,height
      TYPE(WIN_Message) :: MESSAGE
      INTEGER icode,DONE
      CHARACTER*255 NewFileName
      CALL WDialogSelect(IDD_DIALOG12)
      width=WInfoDialog(DialogWidth)
      height=WinfoDialog(DialogHeight)
      scrMaxX=WInfoScreen(ScreenWidth)
      scrMaxY=WInfoScreen(ScreenHeight)
      IF (mainX+mainWidth+width.GT.scrMaxX) THEN
        placeX=mainX-width
      ELSE
        placeX=mainX+mainWidth
      END IF
      IF (mainY+OffsetY+height.GT.scrMaxY) THEN
        placeY=mainY-height+OffsetY+20
        IF (placeY.LT.0) THEN
          placeY=0
        END IF
      ELSE
        placeY=mainY+OffsetY
      END IF
      CALL WDialogShow(placeX,placeY,0,2)
      DONE=0
      DO WHILE (DONE.EQ.0)
        CALL WMessage(icode,MESSAGE)
        SELECT CASE (icode)
          CASE (Expose)
            CALL ReDrawUnderExpose(earth,MESSAGE)
          CASE (CloseRequest)
            IF (MESSAGE%Win.EQ.0) THEN
              CALL TerminateLifeNicely
              STOP
            END IF
          CASE (PushButton)
            SELECT CASE (MESSAGE%Value1)
              CASE (2)
                IF (Message%Win.EQ.IDD_DIALOG12) THEN
                  DONE=1
                END IF
              CASE (IDSetFilesOK)
                DONE=1
              CASE (IDSetFilesCancel)
                DONE=1
              CASE (IDSetSearchDir)
                NewFileName=' '
                CALL WSelectFile(0,LoadDialog+DirChange+PromptOn,NewFileName,'Search Directory')
                IF (WInfoDialog(4).EQ.CommonOK) THEN
                  CALL WDialogPutString(IDSearchDir,NewFileName)
                END IF
              CASE (IDSetPatternFile)
                NewFileName=' '
                CALL WSelectFile(0,LoadDialog+DirChange+PromptOn,NewFileName,'Pattern File')
                IF (WInfoDialog(4).EQ.CommonOK) THEN
                  CALL WDialogPutString(IDPatternFileName,NewFileName)
                END IF
              CASE (IDSetOptionFile)
                NewFileName=' '
                CALL WSelectFile(0,LoadDialog+DirChange+PromptOn,NewFileName,'Options File')
                IF (WInfoDialog(4).EQ.CommonOK) THEN
                  CALL WDialogPutString(IDOptionFileName,NewFileName)
                END IF
              CASE (IDSetOtherFile)
                NewFileName=' '
                CALL WSelectFile(0,LoadDialog+DirChange+PromptOn,NewFileName,'Other File')
                IF (WInfoDialog(4).EQ.CommonOK) THEN
                  CALL WDialogPutString(IDOtherFileName,NewFileName)
                END IF
              CASE DEFAULT
                CALL WMessageBox(OKOnly,InformationIcon,CommonOK,'Feature Not Yet Supported','Sorry')
                CALL WDialogSelect(IDD_DIALOG11)
                icode=NoMessage
            END SELECT
        END SELECT
      END DO
      CALL WDialogSelect(IDD_DIALOG12)
      CALL WDialogHide()
      RETURN
      END SUBROUTINE

      SUBROUTINE HandleRuleSetMenu(earth,mainX,mainY,mainWidth,mainHeight,OffsetY)
      USE WINTERACTER
      IMPLICIT NONE
      INCLUDE 'resource.INC'
      INTEGER, DIMENSION(0:81,0:61) :: earth
      INTEGER mainX,mainY,mainWidth,mainHeight,OffsetY
      INTEGER scrMaxX,scrMaxY
      INTEGER placeX,placeY
      INTEGER width,height
      INTEGER BackScan
      TYPE(WIN_Message) :: MESSAGE
      INTEGER icode,DONE
      INTEGER istate,i
      LOGICAL FileState
      CHARACTER*255 RuleFileName
      CHARACTER*255 LineOfFile
!      CHARACTER*20, DIMENSION(64) :: RuleSetList
      SAVE RuleFileName
      DATA RuleFileName /'d:\epley\temp\life\ruleset.txt'/
!      INTEGER LastHighLight
!      SAVE LastHighLight
!      DATA LastHighLight /2/
      CALL WDialogSelect(IDD_DIALOG10)
      width=WInfoDialog(DialogWidth)
      height=WinfoDialog(DialogHeight)
      scrMaxX=WInfoScreen(ScreenWidth)
      scrMaxY=WInfoScreen(ScreenHeight)
      IF (mainX+mainWidth+width.GT.scrMaxX) THEN
        placeX=mainX-width
      ELSE
        placeX=mainX+mainWidth
      END IF
      IF (mainY+OffsetY+height.GT.scrMaxY) THEN
        placeY=mainY-height+OffsetY+20
        IF (placeY.LT.0) THEN
          placeY=0
        END IF
      ELSE
        placeY=mainY+OffsetY
      END IF
      INQUIRE(FILE=RuleFileName,EXIST=FileState)
      IF (FileState) THEN
        DONE=0
      ELSE
        CALL WSelectFile(0,LoadDialog+PromptOn+DirChange,RuleFileName,'Pattern File')
        IF (WInfoDialog(ExitButton).NE.1) THEN
          DONE=1
        END IF
      END IF
      IF (DONE.EQ.0) THEN
        OPEN(UNIT=101,FILE=RuleFileName,STATUS='OLD')
        i=2
!        RuleSetList(1)='User Defined'
        DO WHILE (1.EQ.1)
          READ(101,'(A20)',END=1002,ERR=1000) LineOfFile
          IF (LineOfFile(1:1).NE.'#') THEN
!            RuleSetList(i)=LineOfFile(1:19)
            READ(101,*,END=1002,ERR=1000) LineOfFile
            READ(101,*,END=1002,ERR=1000) LineOfFile
            READ(101,*,END=1002,ERR=1000) LineOfFile
            READ(101,*,END=1002,ERR=1000) LineOfFile
            i=i+1
          END IF
        END DO
      END IF
 1000 CALL HandleLifeError('Rule Set','Error Reading file'&
                                      // CHAR(13) // CHAR(10)&
                                      // RuleFileName)
 1002 CLOSE(101)
!      CALL WDialogPutMenu(IDRuleMenu,RuleSetList,i-1,LastHighLight)
      CALL WDialogShow(placeX,placeY,0,2)
      CALL WMessageEnable(FieldChanged,1)
      CALL WMessageEnable(MouseMove,1)
      DO WHILE (DONE.EQ.0)
        CALL WMessage(icode,MESSAGE)
        CALL WDialogSelect(IDD_DIALOG10)
        SELECT CASE (icode)
          CASE (Expose)
            CALL ReDrawUnderExpose(earth,MESSAGE)
          CASE (CloseRequest)
            IF (MESSAGE%Win.EQ.0) THEN
              CALL TerminateLifeNicely
              STOP
            END IF
          CASE (MouseMove)
            DONE=1
          CASE (KeyDown)
            IF (MESSAGE%Value1.EQ.KeyEscape.OR.MESSAGE%Value1.EQ.KeyDelete) THEN
              DONE=1
              CALL ShowMeXY(MESSAGE%X,MESSAGE%Y)
            END IF
          CASE (PushButton)
            IF (MESSAGE%Value1.EQ.2) THEN
              SELECT CASE (MESSAGE%Win)
                CASE (IDD_DIALOG10)
                  DONE=1
                CASE DEFAULT
                  CALL WDialogSelect(MESSAGE%Win)
                  CALL WDialogHide()
                  CALL WDialogSelect(IDD_DIALOG10)
              END SELECT
            END IF
            IF (MESSAGE%Win.EQ.IDD_DIALOG10) THEN
              SELECT CASE (MESSAGE%Value1)
                CASE (IDOKRules)
                  DONE=1
                CASE (IDCancelRules)
                  DONE=1
!                CASE (IDSaveRule)
!                CASE (IDDeleteRule)
                CASE DEFAULT
                  CALL WMessageEnable(FieldChanged,0)
                  CALL WMessageEnable(MouseMove,0)
                  CALL WMessageBox(OKOnly,InformationIcon,CommonOK,'Feature Not Yet Supported','Sorry')
                  CALL WDialogSelect(IDD_DIALOG10)
                  CALL WMessageEnable(MouseMove,1)
                  CALL WMessageEnable(FieldChanged,1)
                  icode=NoMessage
              END SELECT
            END IF
          CASE (FieldChanged)
            IF (MESSAGE%Win.NE.IDD_DIALOG10.AND.MESSAGE%Value2.NE.IDRuleSets) THEN
              DONE=1
            ELSE
              SELECT CASE (MESSAGE%Value2)
                CASE (IDUSER1,IDUSER2,IDUSER3,IDUSER4,IDUSER5,IDUSER6,IDUSER7,IDUSER8,IDBIRTH,IDSURVIVE,IDDEATH)
                  CALL WDialogPutOption(IDRuleMenu,1)
              END SELECT
              IF (MESSAGE%Value1.EQ.MESSAGE%Value2.AND.MESSAGE%Win.EQ.IDD_DIALOG10) THEN
                SELECT CASE (MESSAGE%Value2)
                  CASE (IDOKRules)
                  CASE (IDCancelRules)
                  CASE (IDSaveRule)
                  CASE (IDDeleteRule)
                  CASE (IDUSER1,IDUSER2,IDUSER3,IDUSER4,IDUSER5,IDUSER6,IDUSER7,IDUSER8,IDBIRTH,IDSURVIVE,IDDEATH)
                  CASE (IDGroup,IDF_Group2)
                  CASE (IDRULES,IDBIRLAB,IDSURLAB,IDDELAB)
                  CASE (IDRuleMenu)
                    CALL WMessageEnable(FieldChanged,0)
                    CALL WMessageEnable(MouseMove,0)
                    CALL WDialogGetMenu(IDRuleMenu,istate)
                    IF (istate.NE.1) THEN
                      INQUIRE(FILE=RuleFileName,EXIST=FileState)
                      IF (FileState) THEN
                      ELSE
                        CALL WSelectFile(0,LoadDialog+PromptOn+DirChange,RuleFileName,'Pattern File')
                        IF (WInfoDialog(ExitButton).NE.1) THEN
                          DONE=1
                        END IF
                      END IF
                      IF (DONE.EQ.0) THEN
                        OPEN(UNIT=101,FILE=RuleFileName,STATUS='OLD')
                        LineOfFile=' '
                        i=1
                        DO WHILE (i.LT.istate)
                          READ(101,*,END=1001,ERR=1001) LineOfFile
                          IF (LineOfFile.NE.'#') THEN
                            i=i+1
                            READ(101,*,END=1001,ERR=1001) LineOfFile
                            READ(101,*,END=1001,ERR=1001) LineOfFile
                            READ(101,*,END=1001,ERR=1001) LineOfFile
                            READ(101,*,END=1001,ERR=1001) LineOfFile
                          END IF
                        END DO
                        READ(101,*,END=1001,ERR=1001) LineOfFile
                        DO WHILE (LineOfFile(1:1).EQ.'#')
                          READ(101,*,END=1001,ERR=1001) LineOfFile
                        END DO
                        READ(101,'(A20)',END=1001,ERR=1001) LineOfFile
                        CALL WDialogPutString(IDBIRTH,LineOfFile(1:BackScan(LineOfFile,' ')))
                        READ(101,'(A20)',END=1001,ERR=1001) LineOfFile
                        CALL WDialogPutString(IDSURVIVE,LineOfFile(1:BackScan(LineOfFile,' ')))
                        READ(101,'(A20)',END=1001,ERR=1001) LineOfFile
                        CALL WDialogPutString(IDDEATH,LineOfFile(1:BackScan(LineOfFile,' ')))
                        READ(101,'(A20)',END=1001,ERR=1001) LineOfFile
                        DONE=1
 1001                   CLOSE(101)
                        IF (DONE.EQ.0) THEN
                          CALL HandleLifeError('Rule Set','Unexpected End-of-File in' // RuleFileName )
                        END IF
                      END IF
                      DONE=0
                    END IF
                    CALL WMessageEnable(MouseMove,1)
                    CALL WMessageEnable(FieldChanged,1)
                  CASE DEFAULT
                    CALL WMessageEnable(MouseMove,0)
                    CALL WMessageEnable(FieldChanged,0)
                    CALL HandleDialogError('Error')
                    CALL WDialogSelect(IDD_DIALOG10)
                    CALL WMessageEnable(MouseMove,1)
                    CALL WMessageEnable(FieldChanged,1)
                    icode=NoMessage
                END SELECT
              END IF
            END IF
        END SELECT
      END DO
      CALL WMessageEnable(MouseMove,0)
      CALL WMessageEnable(FieldChanged,0)
      CALL WDialogSelect(IDD_DIALOG10)
      CALL WDialogHide()
      RETURN
      END SUBROUTINE

      SUBROUTINE SelectScreenXY(earth,X,Y,PlaceX,PlaceY)
      USE WINTERACTER
      IMPLICIT NONE
      INTEGER, DIMENSION(0:81,0:61) :: earth
      INTEGER X,Y,PlaceX,PlaceY
      INTEGER DONE
      TYPE(WIN_STYLE) :: WStyle
      INTEGER WinHandle
      TYPE(WIN_MESSAGE) :: MESSAGE
      INTEGER icode
      WStyle%Flags=SysMenuOn+MaxButton+FixedSizeWin
      WStyle%X=PlaceX
      WStyle%Width=WInfoScreen(ScreenWidth)/10
      WStyle%Y=PlaceY
      WStyle%Height=WInfoScreen(ScreenHeight)/10
      WStyle%MenuID=0
      WStyle%title='X,Y'
      CALL WindowOpenChild(WStyle,WinHandle)
      IF (WinHandle.EQ.-1) THEN
        RETURN
      END IF
      DONE=0
      DO WHILE (DONE.EQ.0)
        CALL WMessage(icode,MESSAGE)
        SELECT CASE (icode)
          CASE (Expose)
            IF (MESSAGE%Win.EQ.0) THEN
              CALL ReDrawUnderExpose(earth,MESSAGE)
            END IF
          CASE (MouseButDown)
            IF (MESSAGE%Win.EQ.WinHandle) THEN
              IF (MESSAGE%Value1.EQ.3) THEN
                X=-1
                Y=-1
              ELSE
                X=MESSAGE%X/10000*WInfoScreen(ScreenWidth)
                Y=(10000-MESSAGE%Y)/10000*WInfoScreen(ScreenHeight)
              END IF
              DONE=1
            END IF
          CASE (CloseRequest)
            IF (MESSAGE%Win.EQ.WinHandle) THEN
              IF (MESSAGE%Value1.EQ.3) THEN
                X=-1
                Y=-1
              ELSE
                X=MESSAGE%X/10000*WInfoScreen(ScreenWidth)
                Y=(10000-MESSAGE%Y)/10000*WInfoScreen(ScreenHeight)
              END IF
              DONE=1
            ELSE
              SELECT CASE (MESSAGE%Win)
                CASE (0)
                  CALL TerminateLifeNicely
                  STOP
              END SELECT
            END IF
        END SELECT
      END DO
      CALL WindowCloseChild(WinHandle)
      RETURN
      END SUBROUTINE

      INTEGER FUNCTION BackScan(String,CharSet)
      IMPLICIT NONE
      CHARACTER*(*) String
      CHARACTER*(*) CharSet
      INTEGER i,j,StrLen,SetLen
      INTEGER InSet
      StrLen=LEN(String)
      SetLen=LEN(CharSet)
      DO i=StrLen,1,-1
        InSet=0
        DO j=1,SetLen,1
          IF (String(i:i).EQ.CharSet(j:j)) THEN
            InSet=1
          END IF
        END DO
        IF (InSet.EQ.0) THEN
          BackScan=i
          RETURN
        END IF
      END DO
      BackScan=1
      RETURN
      END FUNCTION BackScan

      SUBROUTINE HandleShowCellAge(PlaceX,PlaceY,earth,Xinit,Yinit)
      USE WINTERACTER
      IMPLICIT NONE
      INCLUDE 'resource.INC'
      INTEGER, DIMENSION(0:81,0:61) :: earth
      INTEGER PlaceX,PlaceY,Xinit,Yinit,Age
      INTEGER PropX,PropY,width,height,mainX,mainY,mainWidth,mainHeight
      INTEGER ScrMaxX,ScrMaxY,X,Y
      INTEGER icode
      INTEGER DONE
      TYPE(WIN_Message) :: MESSAGE
      CHARACTER (LEN=10) :: AgeString
      INTEGER AgeStrStart,AgeStrEnd
      CHARACTER (LEN=5) :: XString
      INTEGER XStrStart,XStrEnd
      CHARACTER (LEN=5) :: YString
      INTEGER YStrStart,YStrEnd
      CALL GetPixelPos(PlaceX,PlaceY,PlaceX,PlaceY)
      CALL WDialogSelect(IDD_DIALOG06)
      CALL DrawFinderAroundCells(Xinit,Yinit,Xinit,Yinit,0,176)
!      CALL IGrLineTo(REAL(mainX)*8.0/100.0,REAL(mainY)*6.0/100.0)
      Age=earth(Xinit,Yinit)
      CALL IntegerToString(Xinit,Xstring,'(I5)')
      CALL ILocateString(XString,XStrStart,XStrEnd)
      CALL IntegerToString(Yinit,Ystring,'(I5)')
      CALL ILocateString(YString,YStrStart,YStrEnd)
      CALL IntegerToString(Age,Agestring,'(I10)')
      CALL ILocateString(AgeString,AgeStrStart,AgeStrEnd)
      CALL WDialogPutString(IDDisplayCellAge,&
                               '   ' // XString(XStrStart:XStrEnd)&
                            // ',' // YString(YStrStart:YStrEnd)&
                            // ' is ' // AgeString(AgeStrStart:AgeStrEnd)&
                            // ' old')
      CALL WDialogShow(PlaceX,PlaceY,0,2)
      CALL WMessageEnable(MouseMove,1)
      DONE=0
      DO WHILE (DONE.EQ.0)
        CALL WMessage(icode,MESSAGE)
        SELECT CASE (icode)
          CASE (Expose)
            CALL ReDrawUnderExpose(earth,MESSAGE)
          CASE (MouseMove)
            X=INT(MESSAGE%X*8/1000)+1
            Y=INT((10000-MESSAGE%Y)*6/1000)+1
            IF (X.NE.Xinit.OR.Y.NE.Yinit) THEN
              CALL WMessageEnable(MouseMove,0)
              CALL WDialogSelect(IDD_DIALOG06)
              CALL WDialogHide()
              DONE=1
            END IF
          CASE (KeyDown)
            CALL WMessageEnable(MouseMove,0)
            CALL WDialogSelect(IDD_DIALOG06)
            CALL WDialogHide()
            DONE=1
          CASE (PushButton)
            CALL WMessageEnable(MouseMove,0)
            CALL WDialogSelect(IDD_DIALOG06)
            mainX=WInfoDialog(DialogXPos)
            mainY=WInfoDialog(DialogYPos)
            mainWidth=WInfoDialog(DialogWidth)
            mainHeight=WInfoDialog(DialogHeight)
            CALL WDialogHide()
            CALL WDialogSelect(IDD_DIALOG14)
            CALL WDialogPutString(IDSetCellAgeProp,AgeString(AgeStrStart:AgeStrEnd))
            width=WInfoDialog(DialogWidth)
            height=WinfoDialog(DialogHeight)
            scrMaxX=WInfoScreen(ScreenWidth)
            scrMaxY=WInfoScreen(ScreenHeight)
            IF (mainX+width.GT.scrMaxX) THEN
              propX=mainX-width+mainwidth
            ELSE
              propX=mainX
            END IF
            IF (mainY+height.GT.scrMaxY) THEN
              propY=mainY-height+mainHeight
              IF (propY.LT.0) THEN
                propY=0
              END IF
            ELSE
              propY=mainY
            END IF
            CALL WDialogShow(propX,propY,0,2)
            DONE=0
            DO WHILE (DONE.EQ.0)
              CALL WMessage(icode,MESSAGE)
              SELECT CASE (icode)
                CASE (Expose)
                  IF (MESSAGE%Win.EQ.0) THEN
                    CALL ReDrawUnderExpose(earth,MESSAGE)
                  END IF
                CASE (PushButton)
                  IF (MESSAGE%Win.EQ.IDD_DIALOG14) THEN
                    SELECT CASE (MESSAGE%Value1)
                      CASE (2)
                        DONE=1
                      CASE (IDCellPropCancel)
                        DONE=1
                      CASE (IDCellPropOK)
                        CALL ClearErrorList
                        CALL WDialogGetString(IDSetCellAgeProp,AgeString)
                        CALL IStringToInteger(AgeString,Age)
                        IF (InfoError(1).EQ.0) THEN
                          earth(Xinit,Yinit)=Age
                        END IF
                        DONE=1
                      CASE DEFAULT
                        CALL WMessageBox(OKOnly,InformationIcon,CommonOK,'Feature Not Yet Supported','Sorry')
                        CALL WDialogSelect(IDD_DIALOG14)
                    END SELECT
                  END IF
              END SELECT
            END DO
            CALL WDialogSelect(IDD_DIALOG14)
            CALL WDialogHide()
            DONE=1
        END SELECT
      END DO
      CALL DrawFinderAroundCells(Xinit,Yinit,Xinit,Yinit,0,208)
      CALL DrawCell(Xinit,Yinit,earth(Xinit,Yinit))
      RETURN
      END SUBROUTINE

      SUBROUTINE ShowMeXY(X,Y)
      USE WINTERACTER
      IMPLICIT NONE
      INCLUDE 'resource.INC'
      INTEGER X,Y
      CHARACTER (LEN=5) :: XString
      CHARACTER (LEN=5) :: YString
      CALL IntegerToString(X,Xstring,'(I5)')
      CALL IntegerToString(Y,Ystring,'(I5)')
      CALL WMessageBox(OKOnly,InformationIcon,CommonOK,'X='//XString//', Y='//YString,'Sorry')
      RETURN
      END SUBROUTINE

      SUBROUTINE GetCellPos(X,Y,Xpos,Ypos)
      IMPLICIT NONE
      INTEGER X,Y,Xpos,Ypos
      Xpos=INT(X*8/1000)+1
      Ypos=INT((10000-Y)*6/1000)+1
      RETURN
      END SUBROUTINE

      SUBROUTINE GetPixelPos(X,Y,Xpos,Ypos)
      USE WINTERACTER
      IMPLICIT NONE
      INTEGER X,Y,Xpos,Ypos
      Xpos=INT(REAL(X)/10000.0*REAL(WInfoWindow(WindowWidth)))+WInfoWindow(WindowXPos)+4
      Ypos=INT(REAL(Y)/10000.0*REAL(WInfoWindow(WindowHeight)))+WInfoWindow(WindowYPos)+22
      RETURN
      END SUBROUTINE
