
{**********************************************************************
 Package etpackage.pkg
 From PilotLogic Software House (http://www.pilotlogic.com/)
 This unit is part of CodeTyphon Studio
***********************************************************************}

unit ExSystemConsts;

{$I ExSystem.inc}

interface

const
{$IFDEF UNIX}
  CLSDistros =
    'Annvix: /etc/annvix-release' + sLineBreak +
    'Arch Linux: /etc/arch-release' + sLineBreak +
    'Arklinux: /etc/arklinux-release' + sLineBreak +
    'Aurox Linux: /etc/aurox-release' + sLineBreak +
    'BlackCat: /etc/blackcat-release' + sLineBreak +
    'Cobalt: /etc/cobalt-release' + sLineBreak +
    'Conectiva: /etc/conectiva-release' + sLineBreak +
    'Debian: /etc/debian_version, /etc/debian_release' + sLineBreak +
    'Fedora Core: /etc/fedora-release' + sLineBreak +
    'Gentoo Linux: /etc/gentoo-release' + sLineBreak +
    'Immunix: /etc/immunix-release' + sLineBreak +
    'Knoppix: knoppix_version' + sLineBreak +
    'Linux-From-Scratch: /etc/lfs-release' + sLineBreak +
    'Linux-PPC: /etc/linuxppc-release' + sLineBreak +
    'Mandrake: /etc/mandrake-release' + sLineBreak +
    'Mandriva/Mandrake Linux: /etc/mandriva-release, /etc/mandrake-release, /etc/mandakelinux-release' + sLineBreak +
    'MkLinux: /etc/mklinux-release' + sLineBreak +
    'Novell Linux Desktop: /etc/nld-release' + sLineBreak +
    'PLD Linux: /etc/pld-release' + sLineBreak +
    'Red Hat: /etc/redhat-release, /etc/redhat_version' + sLineBreak +
    'Slackware: /etc/slackware-version, /etc/slackware-release' + sLineBreak +
    'SME Server (Formerly E-Smith): /etc/e-smith-release' + sLineBreak +
    'Solaris SPARC: /etc/release' + sLineBreak +
    'Sun JDS: /etc/sun-release' + sLineBreak +
    'SUSE Linux: /etc/SuSE-release, /etc/novell-release' + sLineBreak +
    'SUSE Linux ES9: /etc/sles-release' + sLineBreak +
    'Tiny Sofa: /etc/tinysofa-release' + sLineBreak +
    'TurboLinux: /etc/turbolinux-release' + sLineBreak +
    'Ubuntu Linux: /etc/lsb-release' + sLineBreak +
    'UltraPenguin: /etc/ultrapenguin-release' + sLineBreak +
    'UnitedLinux: /etc/UnitedLinux-release' + sLineBreak +
    'VA-Linux/RH-VALE: /etc/va-release' + sLineBreak +
    'Yellow Dog: /etc/yellowdog-release';
  CLSDistrosPlayer =
    'Annvix: TODO' + sLineBreak +
    'Arch Linux: TODO' + sLineBreak +
    'Arklinux: TODO' + sLineBreak +
    'Aurox Linux: TODO' + sLineBreak +
    'BlackCat: TODO' + sLineBreak +
    'Cobalt: TODO' + sLineBreak +
    'Conectiva: TODO' + sLineBreak +
    'Debian: TODO' + sLineBreak +
    'Fedora Core: TODO' + sLineBreak +
    'Gentoo Linux: TODO' + sLineBreak +
    'Immunix: TODO' + sLineBreak +
    'Knoppix: TODO' + sLineBreak +
    'Linux-From-Scratch: TODO' + sLineBreak +
    'Linux-PPC: TODO' + sLineBreak +
    'Mandrake: TODO' + sLineBreak +
    'Mandriva/Mandrake Linux: aplay' + sLineBreak +
    'MkLinux: TODO' + sLineBreak +
    'Novell Linux Desktop: TODO' + sLineBreak +
    'PLD Linux: TODO' + sLineBreak +
    'Red Hat: aplay' + sLineBreak +
    'Slackware: TODO' + sLineBreak +
    'SME Server (Formerly E-Smith): TODO' + sLineBreak +
    'Solaris SPARC: TODO' + sLineBreak +
    'Sun JDS: TODO' + sLineBreak +
    'SUSE Linux: aplay' + sLineBreak +
    'SUSE Linux ES9: TODO' + sLineBreak +
    'Tiny Sofa: TODO' + sLineBreak +
    'TurboLinux: TODO' + sLineBreak +
    'Ubuntu Linux: aplay, paplay' + sLineBreak +
    'UltraPenguin: TODO' + sLineBreak +
    'UnitedLinux: TODO' + sLineBreak +
    'VA-Linux/RH-VALE: TODO' + sLineBreak +
    'Yellow Dog: TODO';
{$ENDIF}
  CLSGETMethod = 'GET';
  CLSPOSTMethod = 'POST';
  CLSFormatIPMask = '%d.%d.%d.%d';
  CLSFormatMACMask = '%2.2x-%2.2x-%2.2x-%2.2x-%2.2x-%2.2x';
  CLSHTTPResultCodeOK = 200;
  CLSHTTPResultCodeRedirect = 302;
  CLSBoundaryEndPart = '_LazSolutions_boundary';
  CLSDispNotificationTo = 'Disposition-Notification-To: ';
  CLSDefaultHintPause = 3000;

var
  // Network and web writeable consts
  CLSAttempt: Byte = 3;
  CLSSitesToTry: string =
    // Put yours favorites websites to get external IP.
    'http://www.whatismyip.com/automation/n09230945.asp' + sLineBreak +
    'http://www.ip-adress.com/' + sLineBreak +
    'http://get-myip.com/';
  CLSExtractIPRegEx: string =
    '\b(25[0-5]|2[0-4][0-9]|[01]?[0-9][0-9]?)\.(25[0-5]|2[0-4' +
    '][0-9]|[01]?[0-9][0-9]?)\.(25[0-5]|2[0-4][0-9]|[01]?[0-9' +
    '][0-9]?)\.(25[0-5]|2[0-4][0-9]|[01]?[0-9][0-9]?)\b';
  CLSExtractEmailRegEx: string =
    '[^\w\d\-\.]([\w\d\-\.]+@[\w\d\-]+(\.[\w\d\-]+)+)[^\w\d\-\.]';
  CLSExtractDateRegEx: string =
    '(0[1-9]|[12][0-9]|3[01])[- /.](0[1-9]|1[012])[- /.](19|20)\d\d';
  CLSExtractURLRegEx: string =
    '((http)|(https)|(ftp)):\/\/([\- \w]+\.)+\w{2,3}(\/ [%\-\w]+(\.\w{2,})?)*';
  CLSSendMailPath: string =
{$IFDEF UNIX}
    '/usr/bin/lssendmail'
{$ELSE}
    {$I %programfiles%} + '\LazSolutions\LSSendMail\lssendmail.exe'
{$ENDIF};
  CLSProcessNameValueSeparator: Char = '=';

implementation

end.

