
{**********************************************************************
 Package etpackage.pkg
 From PilotLogic Software House (http://www.pilotlogic.com/)
 This unit is part of CodeTyphon Studio
***********************************************************************}


{$IFDEF WINDOWS}
  {$DEFINE KS_WIN}
{$ELSE}
  {$DEFINE KS_LINUX}
{$ENDIF}

{$IFDEF MSWINDOWS}
  {$DEFINE KS_WIN}
{$ENDIF}

{$IFDEF LINUX}
  {$DEFINE KS_LINUX}
{$ENDIF}


{$ALIGN ON}
{$BOOLEVAL OFF}
{$LONGSTRINGS ON}
{$WRITEABLECONST ON}

{$IFNDEF KS_DEBUG}
  {$WARNINGS OFF}
  {$HINTS OFF}
{$ENDIF}

{$B-} {- Complete Boolean Evaluation }
{$R-} {- Range-Checking }
{$V-} {- Var-String Checking }
{$T-} {- Typed @ operator }
{$X+} {- Extended syntax }
{$P+} {- Open string params }
{$J+} {- Enable writeable const}


