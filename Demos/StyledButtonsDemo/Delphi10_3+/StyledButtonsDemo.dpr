program StyledButtonsDemo;

uses
  Vcl.Forms,
  MainFormUnit in '..\MainFormUnit.pas' {MainForm},
  Vcl.Themes,
  Vcl.Styles,
  Vcl.StyledButton in '..\..\..\source\Vcl.StyledButton.pas',
  Vcl.BootstrapButtonStyles in '..\..\..\source\Vcl.BootstrapButtonStyles.pas',
  Vcl.StandardButtonStyles in '..\..\..\source\Vcl.StandardButtonStyles.pas';

{$R *.res}

begin
  Application.Initialize;
  Application.MainFormOnTaskbar := True;
  TStyleManager.TrySetStyle('Iceberg Classico');
  Application.CreateForm(TMainForm, MainForm);
  Application.Run;
end.
