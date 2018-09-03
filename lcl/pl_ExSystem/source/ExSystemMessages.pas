
{**********************************************************************
 Package etpackage.pkg
 From PilotLogic Software House (http://www.pilotlogic.com/)
 This unit is part of CodeTyphon Studio
***********************************************************************}

unit ExSystemMessages;

{$I ExSystem.inc}

interface

resourcestring
{$I ExSystemMessages.inc}

var
{$IFDEF UNIX}
  // Get Linux distro msgs
  SLSGetLinuxDistroError: string = SLSGetLinuxDistroError_rst;
  // Play WAV msgs
  SLSPlayerNotFound: string = SLSPlayerNotFound_rst;
{$ENDIF}
{$IFDEF MSWINDOWS}
  // List process msgs
  SLSListProcessError: string = SLSListProcessError_rst;
{$ENDIF}
  // SMTP msgs
  SLSSMTPError: string = SLSSMTPError_rst;
  SLSEmailSentSuccessfully: string = SLSEmailSentSuccessfully_rst;
  // Get IP msgs
  SLSGetIPError: string = SLSGetIPError_rst;
  // Process thread msgs
  SLSProcessThreadCanceled: string = SLSProcessThreadCanceled_rst;

implementation

end.

