program StyledButtonsDemo;

uses
  Vcl.Forms,
  MainFormUnitOld in '..\MainFormUnitOld.pas' {MainForm},
  Vcl.Themes,
  Vcl.Styles,
  Vcl.StyledButton in '..\..\..\source\Vcl.StyledButton.pas',
  Vcl.BootstrapButtonStyles in '..\..\..\source\Vcl.BootstrapButtonStyles.pas',
  Vcl.StandardButtonStyles in '..\..\..\source\Vcl.StandardButtonStyles.pas';

{$R *.res}

begin
  Application.Initialize;
  Application.MainFormOnTaskbar := True;
  //TStyleManager.TrySetStyle('Windows10');
  Application.CreateForm(TMainForm, MainForm);
  Application.Run;
end.
