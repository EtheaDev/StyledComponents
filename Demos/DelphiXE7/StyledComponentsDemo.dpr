program StyledComponentsDemo;

uses
  Vcl.Forms,
  Vcl.Themes,
  Vcl.Styles,
  uSettings in '..\source\uSettings.pas',
  MainDemoForm in '..\source\MainDemoForm.pas' {frmMain},
  DResourcesOld in '..\source\DResourcesOld.pas' {dmResources: TDataModule},
  DemoWelcomeForm in '..\source\DemoWelcomeForm.pas' {WelcomeForm},
  FAboutForm in '..\source\FAboutForm.pas' {fmAbout},
  AutoClickFormOld in '..\source\AutoClickFormOld.pas' {fmAutoClick},
  BitBtnForm in '..\source\BitBtnForm.pas' {fmBitBtn},
  RoundedCornersFormOld in '..\source\RoundedCornersFormOld.pas' {fmRoundedCorners},
  StyledButtonGroupFormOld in '..\source\StyledButtonGroupFormOld.pas' {fmStyledButtonGroup},
  StyledButtonsFormOld in '..\source\StyledButtonsFormOld.pas' {fmStyledButtons},
  StyledCategoryButtonsFormOld in '..\source\StyledCategoryButtonsFormOld.pas' {fmStyledCategoryButtons},
  StyledDbNavigatorFormOld in '..\source\StyledDbNavigatorFormOld.pas' {fmStyledDbNavigator},
  StyledDialogDemoForm in '..\source\StyledDialogDemoForm.pas' {fmStyledTaskDialog},
  StyledToolbarFormOld in '..\source\StyledToolbarFormOld.pas' {fmStyledToolbar};

{$R *.res}

begin
  Application.Initialize;
  Application.MainFormOnTaskbar := True;
  Application.ActionUpdateDelay := 50;
  Application.Title := 'Styled Components Demos with Delphi XE7';
  Application.CreateForm(TdmResources, dmResources);
  Application.CreateForm(TfrmMain, frmMain);
  Application.Run;
end.
