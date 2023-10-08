unit MainFormBitBtn;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.StdCtrls, Vcl.Buttons,
  Vcl.ButtonStylesAttributes, Vcl.StyledButton;

const
  BTN_ENABLED = True;

type
  TfmBitBtn = class(TForm)
    btOK: TBitBtn;
    btCancel: TBitBtn;
    btClose: TBitBtn;
    btYes: TBitBtn;
    btNo: TBitBtn;
    btAbort: TBitBtn;
    btRetry: TBitBtn;
    btIgnore: TBitBtn;
    btCustom: TBitBtn;
    btHelp: TBitBtn;
    btAll: TBitBtn;
    procedure FormCreate(Sender: TObject);
    procedure ButtonClick(Sender: TObject);
    procedure ButtonMouseLeave(Sender: TObject);
    procedure ButtonMouseEnter(Sender: TObject);
  private
    procedure CreateButtons(const AStyleFamily: TStyledButtonFamily;
      const ALeftOffSet: Integer);
  public
    { Public declarations }
  end;

var
  fmBitBtn: TfmBitBtn;

implementation

{$R *.dfm}

uses
  Vcl.StandardButtonStyles
  , Vcl.AngularButtonStyles
  , Vcl.BootstrapButtonStyles
  , Vcl.ColorButtonStyles
  ;

procedure TfmBitBtn.ButtonClick(Sender: TObject);
var
  LModalResult: Integer;
begin
  if Sender is TBitBtn then
    LModalResult := TBitBtn(Sender).ModalResult
  else if Sender is TStyledButton then
    LModalResult := TStyledButton(Sender).ModalResult
  else
    Exit;
  ShowMessage(Format('ModalResult = %d', [LModalResult]));
end;

procedure TfmBitBtn.ButtonMouseEnter(Sender: TObject);
var
  LBtn: TStyledButton;
begin
  if Sender is TStyledButton then
  begin
    LBtn := TStyledButton(Sender);
    Hint := Format('StyleFamily: %s - StyleClass: %s - StyleAppearance: %s',
      [LBtn.StyleFamily, LBtn.StyleClass, LBtn.StyleAppearance]);
  end;
end;

procedure TfmBitBtn.ButtonMouseLeave(Sender: TObject);
begin
  Hint := '';
end;

procedure TfmBitBtn.CreateButtons(const AStyleFamily: TStyledButtonFamily;
  const ALeftOffSet: Integer);

  function CreateButtonAs(const ABitBtn: TBitBtn): TStyledButton;
  begin
    ABitBtn.Enabled := BTN_ENABLED;
    Result := TStyledButton.Create(Self);
    Result.SetBounds(ABitBtn.Left+ALeftOffSet, ABitBtn.Top, ABitBtn.Width, ABitBtn.Height);
    Result.StyleFamily := AStyleFamily;
    Result.Caption := ABitBtn.Caption;
    Result.Enabled := BTN_ENABLED;
    Result.Kind := ABitBtn.Kind;
    if Result.Kind = bkCustom then
    begin
      Result.NumGlyphs := ABitBtn.NumGlyphs;
      Result.Glyph := ABitBtn.Glyph;
    end;
    Result.Parent := Self;
    Result.OnClick := ButtonClick;
    Result.OnMouseEnter := ButtonMouseEnter;
    Result.OnMouseLeave := ButtonMouseLeave;
  end;
begin
  CreateButtonAs(btOK);
  CreateButtonAs(btCancel);
  CreateButtonAs(btClose);
  CreateButtonAs(btYes);
  CreateButtonAs(btNo);
  CreateButtonAs(btAbort);
  CreateButtonAs(btRetry);
  CreateButtonAs(btIgnore);
  CreateButtonAs(btHelp);
  CreateButtonAs(btAll);
  CreateButtonAs(btCustom);
end;

procedure TfmBitBtn.FormCreate(Sender: TObject);
begin
  Caption := Application.Title;
  CreateButtons(DEFAULT_CLASSIC_FAMILY, 100);
  CreateButtons(BOOTSTRAP_FAMILY, 200);
  CreateButtons(ANGULAR_LIGHT_FAMILY, 300);
  CreateButtons(ANGULAR_DARK_FAMILY, 400);
  CreateButtons(BASIC_COLOR_FAMILY, 500);
  CreateButtons(SVG_COLOR_FAMILY, 600);
end;

initialization
  {$IFDEF DEBUG}
  ReportMemoryLeaksOnShutdown := True;
  {$ENDIF}


end.
