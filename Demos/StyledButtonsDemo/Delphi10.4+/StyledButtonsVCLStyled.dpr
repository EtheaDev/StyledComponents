program StyledButtonsVCLStyled;

uses
  Vcl.Forms,
  MainFormTest in '..\MainFormTest.pas' {TestMainForm},
  Vcl.Themes,
  Vcl.Styles,
  Vcl.StyledButton in '..\..\..\source\Vcl.StyledButton.pas',
  Vcl.ButtonStylesAttributes in '..\..\..\source\Vcl.ButtonStylesAttributes.pas',
  Vcl.BootstrapButtonStyles in '..\..\..\source\Vcl.BootstrapButtonStyles.pas',
  Vcl.AngularButtonStyles in '..\..\..\source\Vcl.AngularButtonStyles.pas',
  Vcl.StandardButtonStyles in '..\..\..\source\Vcl.StandardButtonStyles.pas',
  Vcl.StyledButtonEditorUnit in '..\..\..\packages\Vcl.StyledButtonEditorUnit.pas' {StyledButtonEditor},
  DResources in '..\DResources.pas' {dmResources: TDataModule},
  FAboutForm in '..\..\FAboutForm.pas' {fmAbout};

{$R *.res}

begin
  Application.Initialize;
  Application.MainFormOnTaskbar := True;
  TStyleManager.TrySetStyle('Windows10');
  Application.Title := 'VCL Styled Buttons compared to StyleButtons';
  Application.CreateForm(TdmResources, dmResources);
  Application.CreateForm(TTestMainForm, TestMainForm);
  Application.Run;
end.
