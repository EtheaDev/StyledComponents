program BitBtnDemo;

uses
  Vcl.Forms,
  Vcl.Themes,
  Vcl.Styles,
  Vcl.StyledButton,
  Vcl.ButtonStylesAttributes,
  MainFormBitBtn in '..\MainFormBitBtn.pas' {fmBitBtn};

{$R *.res}

begin
  Application.Initialize;
  Application.MainFormOnTaskbar := True;
  Application.Title := 'BitButtons - Kind and ModalResult Demo - (c) Copyright Ethea S.r.l.';
  //Examples to define a default Rendering Style for DbNavigator Styled Buttons
  //TStyledButton.RegisterDefaultRenderingStyle(btRounded);
  Application.CreateForm(TfmBitBtn, fmBitBtn);
  Application.Run;
end.
