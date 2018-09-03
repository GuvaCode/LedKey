{$DEFINE LSLANGUAGE_EN}
{.$DEFINE LSLANGUAGE_PTBR}
{$IFDEF LSLANGUAGE_EN}
  SLSGetLinuxDistroError_rst = 'ERROR: Could not determine the distro name.';
  SLSPlayerNotFound_rst = 'ERROR: Could not determine the player name.';
  SLSSMTPError_rst = 'SMTP error: %s.' + sLineBreak + '%s%s' + sLineBreak + sLineBreak +
                  'Try sending again.';
  SLSEmailSentSuccessfully_rst = '-E-mail sent successfully!-';
  SLSGetIPError_rst = 'Could not connect to the site to obtain the IP.';
  SLSProcessThreadCanceled_rst = 'Process canceled for user.';
  SLSListProcessError_rst = 'ERROR: Error showing process list.';
{$ENDIF}
{$IFDEF LSLANGUAGE_PTBR}
  SLSGetLinuxDistroError_rst = 'ERRO: Não foi possível determinar o nome da distro.';
  SLSPlayerNotFound_rst = 'ERRO: Não foi possível determinar o nome do player.';
  SLSSMTPError_rst = 'SMTP erro: %s.' + sLineBreak + '%s%s' + sLineBreak + sLineBreak +
                  'Tente enviar novamente.';
  SLSEmailSentSuccessfully_rst = '-E-mail enviado com sucesso!-';
  SLSGetIPError_rst = 'Não foi possível conectar ao site para obter o IP.';
  SLSProcessThreadCanceled_rst = 'Processo cancelado pelo usuário.';
  SLSListProcessError_rst = 'ERRO: Erro ao mostrar lista de processos.';
{$ENDIF}
