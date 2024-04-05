{******************************************************************************}
{                                                                              }
{  StyledTaskDialogForm: a Task Dialog Form with StyleButtons                  }
{                                                                              }
{  Copyright (c) 2022-2024 (Ethea S.r.l.)                                      }
{  Author: Carlo Barazzetta                                                    }
{  Contributors:                                                               }
{                                                                              }
{  https://github.com/EtheaDev/StyledComponents                                }
{                                                                              }
{******************************************************************************}
{                                                                              }
{  Licensed under the Apache License, Version 2.0 (the "License");             }
{  you may not use this file except in compliance with the License.            }
{  You may obtain a copy of the License at                                     }
{                                                                              }
{      http://www.apache.org/licenses/LICENSE-2.0                              }
{                                                                              }
{  Unless required by applicable law or agreed to in writing, software         }
{  distributed under the License is distributed on an "AS IS" BASIS,           }
{  WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.    }
{  See the License for the specific language governing permissions and         }
{  limitations under the License.                                              }
{                                                                              }
{******************************************************************************}
unit Vcl.StyledTaskDialogFormUnit;

interface

{$INCLUDE StyledComponents.inc}

uses
  Winapi.Windows,
  Winapi.Messages,
  System.SysUtils,
  System.Variants,
  System.Classes,
  Vcl.Graphics,
  Vcl.Controls,
  Vcl.Forms,
  Vcl.Dialogs,
  Vcl.StdCtrls,
  Vcl.ExtCtrls,
  Vcl.ImgList,
  System.UITypes,
  Vcl.StyledButton,
  Vcl.StyledTaskDialog,
  Vcl.StandardButtonStyles,
  Vcl.BootstrapButtonStyles,
  Vcl.AngularButtonStyles,
  Vcl.ButtonStylesAttributes,
  Vcl.ColorButtonStyles;

type
  TTaskDialogLauncherHandler = class(TInterfacedObject, ITaskDialogLauncher)
    function DoExecute(ParentWnd: HWND;
      const ADialogType: TMsgDlgType;
      const ATaskDialog: TCustomTaskDialog;
      const ADialogBtnFamily: TStyledButtonFamily): Boolean;
  end;

  TStyledTaskDialogForm = class(TForm)
    FooterPanel: TPanel;
    CenterPanel: TPanel;
    ImagePanel: TPanel;
    ButtonsPanel: TPanel;
    BottomBevel: TBevel;
    MessageScrollBox: TScrollBox;
    TitleLabel: TLabel;
    TextLabel: TLinkLabel;
    AutoSizeLabel: TLabel;
    YesButton: TStyledButton;
    NoButton: TStyledButton;
    OKButton: TStyledButton;
    CancelButton: TStyledButton;
    AbortButton: TStyledButton;
    RetryButton: TStyledButton;
    IgnoreButton: TStyledButton;
    AllButton: TStyledButton;
    NoToAllButton: TStyledButton;
    YesToAllButton: TStyledButton;
    HelpButton: TStyledButton;
    CloseButton: TStyledButton;
    FooterTextLabel: TLabel;
    procedure FormCreate(Sender: TObject);
    procedure ButtonClick(Sender: TObject);
    procedure FormKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
    procedure FormShow(Sender: TObject);
    procedure TextLabelLinkClick(Sender: TObject; const Link: string;
      LinkType: TSysLinkType);
    procedure FormDestroy(Sender: TObject);
    procedure HelpButtonClick(Sender: TObject);
  private
    FFocusedButton: TStyledButton;
    FCustomIcons: TStyledDialogIcons;
    FTaskDialog: TCustomTaskDialog;
    FDialogType: TMsgDlgType;
    FDialogBtnFamily: TStyledButtonFamily;
    FCommonButtons: TTaskDialogCommonButtons;
    FDefaultButton: TTaskDialogCommonButton;
    FButtons: TTaskDialogButtons;
    FMainIcon: TTaskDialogIcon;
    procedure GetIconNameAndIndex(ATaskDialog: TMsgDlgType;
      out AImageName: string; out AImageIndex: Integer); overload;
    procedure GetIconNameAndIndex(ATaskDialogIcon: TTaskDialogIcon;
      out AImageName: string; out AImageIndex: Integer); overload;
    procedure ShowDialogForm;
    procedure SetHelpContext(const AValue: Integer);
    function GetHelpContext: Integer;
    function GetText: string;
    function GetTitle: string;
    procedure SetText(const AValue: string);
    procedure SetTitle(const AValue: string);
    procedure AdjustHeight;
    procedure AdjustWidth;
    procedure AdjustButtonsCaption;
    procedure SetButtons(const AValue: TTaskDialogButtons);
    procedure PlayMessageDlgSound;
    procedure FocusDefaultButton;
    procedure LoadDialogImage;
    procedure SetFocusToButton(AStyledButton: TStyledButton);
    procedure SetFooterText(const AValue: string);
    function GetFooterText: string;
    function GetFocusedButton: TStyledButton;
    procedure InitDlgButtonsWithFamily(const AFamily: TStyledButtonFamily);
    procedure UpdateButtonVisibility;
(*
    property Button: TTaskDialogButtonItem read FButton write FButton;
*)
    property Buttons: TTaskDialogButtons read FButtons write SetButtons;
    property CommonButtons: TTaskDialogCommonButtons read FCommonButtons write FCommonButtons default [tcbOk, tcbCancel];
    property DefaultButton: TTaskDialogCommonButton read FDefaultButton write FDefaultButton default tcbOk;
(*
    property CustomFooterIcon: TIcon read FCustomFooterIcon write SetCustomFooterIcon;
    property CustomMainIcon: TIcon read FCustomMainIcon write SetCustomMainIcon;
    property ExpandButtonCaption: string read FExpandButtonCaption write FExpandButtonCaption;
    property Expanded: Boolean read FExpanded;
    property ExpandedText: string read FExpandedText write SetExpandedText;
    property Flags: TTaskDialogFlags read FFlags write SetFlags default [tfAllowDialogCancellation];
    property FooterIcon: TTaskDialogIcon read FFooterIcon write SetFooterIcon default tdiNone;
    property Handle: HWND read FHandle;
*)
    property FooterText: string read GetFooterText write SetFooterText;
    property HelpContext: Integer read GetHelpContext write SetHelpContext default 0;
    property MainIcon: TTaskDialogIcon read FMainIcon write FMainIcon default tdiInformation;
(*
    property ProgressBar: TTaskDialogProgressBar read FProgressBar write FProgressBar;
    property RadioButton: TTaskDialogRadioButtonItem read FRadioButton;
    property RadioButtons: TTaskDialogButtons read FRadioButtons write SetRadioButtons;
*)
    property TextMessage: string read GetText write SetText;
    property TitleMessage: string read GetTitle write SetTitle;
(*
    property URL: string read FURL;
    property VerificationText: string read FVerificationText write FVerificationText;
    property OnButtonClicked: TTaskDlgClickEvent read FOnButtonClicked write FOnButtonClicked;
    property OnDialogConstructed: TNotifyEvent read FOnDialogConstructed write FOnDialogConstructed;
    property OnDialogCreated: TNotifyEvent read FOnDialogCreated write FOnDialogCreated;
    property OnDialogDestroyed: TNotifyEvent read FOnDialogDestroyed write FOnDialogDestroyed;
    property OnExpanded: TNotifyEvent read FOnExpanded write FOnExpanded;
    property OnHyperlinkClicked: TNotifyEvent read FOnHyperlinkClicked write FOnHyperlinkClicked;
    property OnNavigated: TNotifyEvent read FOnNavigated write FOnNavigated;
    property OnRadioButtonClicked: TNotifyEvent read FOnRadioButtonClicked write FOnRadioButtonClicked;
    property OnTimer: TTaskDlgTimerEvent read FOnTimer write FOnTimer;
    property OnVerificationClicked: TNotifyEvent read FOnVerificationClicked write FOnVerificationClicked;
*)
  protected
    procedure UpdateCustomIcons;
    procedure Loaded; override;
    procedure LoadImage(const AImageIndex: TImageIndex;
      AImageName: string); virtual; abstract;
    procedure DefaultDialogSize(out AClientWidth, AClientHeight, AImageSize: Integer); virtual;
  public
    procedure SetDialogFont(const AFont: TFont); virtual;
    constructor Create(AOwner: TComponent); override;
  end;

  TStyledTaskDialogFormClass = class of TStyledTaskDialogForm;

/// <summary>
///  To activate or deactivate use of Custom Task Dialog
/// </summary>
procedure UseStyledDialogForm(const AActivate: Boolean);

/// <summary>
///  Register the custom StyledTaskDialog passing the Form Class
///  The Form must inherits from TStyledTaskDialogForm
/// </summary>
procedure RegisterTaskDialogFormClass(AFormClass: TStyledTaskDialogFormClass);

/// <summary>
///  Unregister the custom StyledTaskDialog to use Standard Task Dialog
/// </summary>
procedure UnregisterTaskDialogFormClass;

implementation

{$R *.dfm}

uses
  System.Math
  , Vcl.Themes
  , System.HelpIntfs
  , Winapi.ShellAPI
  , Vcl.StyledCmpMessages
  , System.Typinfo
  ;

var
  DialogLauncher: ITaskDialogLauncher;
  TaskDialogFormClass: TStyledTaskDialogFormClass;
  DlgButtonClasses: TButtonClasses;

procedure RegisterTaskDialogFormClass(AFormClass: TStyledTaskDialogFormClass);
begin
  TaskDialogFormClass := AFormClass;
  UseStyledDialogForm(True);
end;

procedure UnregisterTaskDialogFormClass;
begin
  TaskDialogFormClass := nil;
end;

procedure UseStyledDialogForm(const AActivate: Boolean);
begin
  if AActivate then
    RegisterCustomExecute(DialogLauncher)
  else
    UnregisterCustomExecute;
end;

{ TStyledTaskDialogForm }

procedure TStyledTaskDialogForm.SetButtons(const AValue: TTaskDialogButtons);
var
  I: Integer;
  LTaskDialogButtonItem: TTaskDialogBaseButtonItem;
  LStyledButton: TStyledButton;
begin
  FButtons := AValue;
  for I := FButtons.Count -1 downto 0 do
  begin
    LTaskDialogButtonItem := FButtons[I];
    case LTaskDialogButtonItem.ModalResult of
      mrYes: LStyledButton := YesButton;
      mrNo: LStyledButton := NoButton;
      mrOk: LStyledButton := OKButton;
      mrCancel: LStyledButton := CancelButton;
      mrAbort: LStyledButton := AbortButton;
      mrRetry: LStyledButton := RetryButton;
      mrIgnore: LStyledButton := IgnoreButton;
      mrAll: LStyledButton := AllButton;
      mrNoToAll: LStyledButton := NoToAllButton;
      mrYesToAll: LStyledButton := YesToAllButton;
      mrClose: LStyledButton := CloseButton;
      mrHelp: LStyledButton := HelpButton;
    else
      LStyledButton := HelpButton;
    end;
    if Assigned(LStyledButton) then
    begin
      LStyledButton.Caption := LTaskDialogButtonItem.Caption;
      if LTaskDialogButtonItem.Default then
        SetFocusToButton(LStyledButton);
    end;
  end;
end;

procedure TStyledTaskDialogForm.SetFocusToButton(AStyledButton: TStyledButton);
begin
  if AStyledButton.CanFocus then
    AStyledButton.SetFocus;
  FFocusedButton := AStyledButton;
end;

procedure TStyledTaskDialogForm.SetFooterText(const AValue: string);
begin
  FooterTextLabel.Caption := AValue;
  FooterPanel.Visible := AValue <> '';
end;

procedure TStyledTaskDialogForm.FocusDefaultButton;
begin
  if not Assigned(FFocusedButton) then
  begin
    case DefaultButton of
      tcbOk: SetFocusToButton(OKbutton);
      tcbYes: SetFocusToButton(YesButton);
      tcbNo: SetFocusToButton(NoButton);
      tcbCancel: SetFocusToButton(CancelButton);
      tcbRetry: SetFocusToButton(RetryButton);
      tcbClose: SetFocusToButton(CloseButton);
    end;
  end;
end;

procedure TStyledTaskDialogForm.SetDialogFont(const AFont: TFont);
begin
  Self.Font.Assign(AFont);
  TextLabel.Font.Name := Font.Name;
  TextLabel.Font.Size := AFont.Size;
  //TitleLabel font attributes
  if not StyleServices.Enabled or StyleServices.IsSystemStyle then
    TitleLabel.Font.Color := clHighlight
  else
    TextLabel.Font.Color := StyleServices.GetSystemColor(clHighlight);
  TitleLabel.Font.Style := [TFontStyle.fsBold];
  TitleLabel.Font.Name := Font.Name;
  TitleLabel.Font.Height := Round(AFont.Height * 1.4);
end;

procedure TStyledTaskDialogForm.SetHelpContext(const AValue: Integer);
begin
  inherited HelpContext := AValue;
  HelpButton.HelpContext := AValue;
end;

procedure TStyledTaskDialogForm.AdjustHeight;
var
  LFooterPanelHeight: Integer;
  LMinHeight, LCalcHeight: Integer;
  LImageSize, LWidth, LHeight, LMargins: Integer;
begin
  LMargins := ImagePanel.Left * 2;
  DefaultDialogSize(LWidth, LHeight, LImageSize);
  if FooterPanel.Visible then
    LFooterPanelHeight := FooterPanel.Height + LMargins
  else
    LFooterPanelHeight := 0;
  LCalcHeight :=
    AutoSizeLabel.Height + LMargins +
    TitleLabel.Height + LMargins +
    LFooterPanelHeight +
    ButtonsPanel.Height + LMargins;
  LMinHeight := LImageSize + LMargins +
    LFooterPanelHeight + LMargins +
    ButtonsPanel.Height + LMargins;

  Constraints.MinHeight := LMinHeight +
    Height - ClientHeight;

  LHeight := Min(Self.Monitor.Height - 100,
    Max(LCalcHeight, LMinHeight));

  ClientHeight := LHeight;

  TextLabel.Font.Assign(AutoSizeLabel.Font);
  TextLabel.Height := AutoSizeLabel.Height;
  AutoSizeLabel.Visible := False;

  if LCalcHeight > LHeight then
  begin
    MessageScrollBox.VertScrollBar.Visible := True;
    MessageScrollBox.VertScrollBar.Range := LCalcHeight;
  end
  else
    MessageScrollBox.VertScrollBar.Visible := False;
end;

procedure TStyledTaskDialogForm.AdjustWidth;
var
  LFormWidth, I: Integer;
  LStyledButton: TStyledButton;
  LMargins: Integer;
  LImageSize, LWidth, LHeight: Integer;
begin
  DefaultDialogSize(LWidth, LHeight, LImageSize);
  LMargins := ButtonsPanel.Margins.Left;
  LFormWidth := LMargins;
  for I := 0 to ComponentCount -1 do
  begin
    if Components[I] is TStyledButton then
    begin
      LStyledButton := TStyledButton(Components[I]);
      if LStyledButton.Visible then
        LFormWidth := LFormWidth + LStyledButton.Width + LMargins + LMargins;
    end;
  end;
  LFormWidth := LFormWidth + LMargins;
  ClientWidth := Max(LWidth, LFormWidth);
end;

procedure TStyledTaskDialogForm.SetText(const AValue: string);
begin
  AutoSizeLabel.Caption := AValue;
  TextLabel.Caption := AValue;
end;

procedure TStyledTaskDialogForm.SetTitle(const AValue: string);
begin
  TitleLabel.Caption := AValue;
end;

procedure TStyledTaskDialogForm.LoadDialogImage;
var
  LIconName: string;
  LIconIndex: Integer;
begin
  if FMainIcon <> tdiNone then
    GetIconNameAndIndex(FMainIcon, LIconName, LIconIndex)
  else
    GetIconNameAndIndex(FDialogType, LIconName, LIconIndex);
  LoadImage(LIconIndex, LIconName);
end;

procedure TStyledTaskDialogForm.UpdateButtonVisibility;

  function IsButtonVisible(const AButton: TStyledButton): Boolean;
  var
    I: Integer;
    LTaskDialogButtonItem: TTaskDialogBaseButtonItem;
  begin
    Result := False;
    for I := FButtons.Count -1 downto 0 do
    begin
      LTaskDialogButtonItem := FButtons[I];
      if AButton.ModalResult = LTaskDialogButtonItem.ModalResult then
      begin
        Result := True;
        break;
      end;
    end;
  end;

begin
  YesButton.Visible := (tcbYes in FCommonButtons) or IsButtonVisible(YesButton);
  NoButton.Visible := (tcbNo in FCommonButtons) or IsButtonVisible(NoButton);
  OKButton.Visible := (tcbOk in FCommonButtons) or IsButtonVisible(OKButton);
  CancelButton.Visible := (tcbCancel in FCommonButtons) or IsButtonVisible(CancelButton);
  AbortButton.Visible := IsButtonVisible(AbortButton);
  RetryButton.Visible := (tcbRetry in FCommonButtons) or IsButtonVisible(RetryButton);
  IgnoreButton.Visible := IsButtonVisible(IgnoreButton);
  AllButton.Visible := IsButtonVisible(AllButton);
  NoToAllButton.Visible := IsButtonVisible(NoToAllButton);
  YesToAllButton.Visible := IsButtonVisible(YesToAllButton);
  HelpButton.Visible := HelpButton.HelpContext <> 0;
  CloseButton.Visible := (tcbClose in FCommonButtons) or IsButtonVisible(CloseButton);
end;

procedure TStyledTaskDialogForm.ShowDialogForm;
begin
  //Initialize components based on ATaskDialog attributes
  Caption := FTaskDialog.Caption;
  HelpContext := FTaskDialog.HelpContext;
  Buttons := FTaskDialog.Buttons;
  CommonButtons := FTaskDialog.CommonButtons;
  DefaultButton := FTaskDialog.DefaultButton;
(*
    property Button: TTaskDialogButtonItem read FButton write FButton;
*)
  UpdateButtonVisibility;

  if FDialogBtnFamily <> '' then
    InitDlgButtonsWithFamily(FDialogBtnFamily);
(*
    property CustomFooterIcon: TIcon read FCustomFooterIcon write SetCustomFooterIcon;
    property CustomMainIcon: TIcon read FCustomMainIcon write SetCustomMainIcon;
    property ExpandButtonCaption: string read FExpandButtonCaption write FExpandButtonCaption;
    property Expanded: Boolean read FExpanded;
    property ExpandedText: string read FExpandedText write SetExpandedText;
    property Flags: TTaskDialogFlags read FFlags write SetFlags default [tfAllowDialogCancellation];
    property FooterIcon: TTaskDialogIcon read FFooterIcon write SetFooterIcon default tdiNone;
    property Handle: HWND read FHandle;
*)
    FooterText := FTaskDialog.FooterText;
    MainIcon := FTaskDialog.MainIcon;
(*
    property ProgressBar: TTaskDialogProgressBar read FProgressBar write FProgressBar;
    property RadioButton: TTaskDialogRadioButtonItem read FRadioButton;
    property RadioButtons: TTaskDialogButtons read FRadioButtons write SetRadioButtons;
*)
    TextMessage := FTaskDialog.Text;
    TitleMessage :=  FTaskDialog.Title;
(*
    property URL: string read FURL;
    property VerificationText: string read FVerificationText write FVerificationText;
    property OnButtonClicked: TTaskDlgClickEvent read FOnButtonClicked write FOnButtonClicked;
    property OnDialogConstructed: TNotifyEvent read FOnDialogConstructed write FOnDialogConstructed;
    property OnDialogCreated: TNotifyEvent read FOnDialogCreated write FOnDialogCreated;
    property OnDialogDestroyed: TNotifyEvent read FOnDialogDestroyed write FOnDialogDestroyed;
    property OnExpanded: TNotifyEvent read FOnExpanded write FOnExpanded;
    property OnHyperlinkClicked: TNotifyEvent read FOnHyperlinkClicked write FOnHyperlinkClicked;
    property OnNavigated: TNotifyEvent read FOnNavigated write FOnNavigated;
    property OnRadioButtonClicked: TNotifyEvent read FOnRadioButtonClicked write FOnRadioButtonClicked;
    property OnTimer: TTaskDlgTimerEvent read FOnTimer write FOnTimer;
    property OnVerificationClicked: TNotifyEvent read FOnVerificationClicked write FOnVerificationClicked;
*)
  //Load and show Image
  LoadDialogImage;
end;

procedure TStyledTaskDialogForm.TextLabelLinkClick(Sender: TObject;
  const Link: string; LinkType: TSysLinkType);
begin
  ShellExecute(Self.Handle, 'open' , PChar(Link),
    nil, nil, SW_SHOW );
end;

procedure TStyledTaskDialogForm.UpdateCustomIcons;

  procedure GetAndSetIcon(const AType: TMsgDlgType;
    AColor: TColor);
  var
    LIcon: TIcon;
    LTypeName: string;
  begin
    LIcon := TIcon.Create;
    LTypeName := GetEnumName(TypeInfo(TMsgDlgType), Ord(AType));
    FreeAndNil(FCustomIcons[AType]);
    FCustomIcons[AType] := LIcon;
  end;

begin
  GetAndSetIcon(mtWarning, clYellow);
  GetAndSetIcon(mtError, clRed);
  GetAndSetIcon(mtInformation, clSkyBlue);
  GetAndSetIcon(mtConfirmation, clLime);
  GetAndSetIcon(mtCustom, clWindow);
(*
  if FRegistered then
    RegisterCustomIcons(FCustomIcons);
*)
end;

procedure TStyledTaskDialogForm.ButtonClick(Sender: TObject);
begin
  Close;
  self.ModalResult := (Sender as TStyledButton).ModalResult;
end;

constructor TStyledTaskDialogForm.Create(AOwner: TComponent);
begin
  inherited;
  FCommonButtons := [tcbOk, tcbCancel];
  FDefaultButton := tcbOk;
  FDialogBtnFamily := DEFAULT_CLASSIC_FAMILY;
end;

procedure TStyledTaskDialogForm.DefaultDialogSize(out AClientWidth, AClientHeight, AImageSize: Integer);
var
  LScaleFactor: Single;
begin
  LScaleFactor := Self.PixelsPerInch / 96;
  //Values for 96 DPI
  AClientWidth := Round(600 * LScaleFactor);
  AClientHeight := Round(280 * LScaleFactor);
  AImageSize := Round(128 * LScaleFactor);
end;

procedure TStyledTaskDialogForm.FormCreate(Sender: TObject);
begin
  FooterPanel.Visible := False;
end;

procedure TStyledTaskDialogForm.FormDestroy(Sender: TObject);
begin
  FCustomIcons[mtWarning].Free;
  FCustomIcons[mtError].Free;
  FCustomIcons[mtInformation].Free;
  FCustomIcons[mtConfirmation].Free;
  FCustomIcons[mtCustom].Free;
end;

function TStyledTaskDialogForm.GetFocusedButton: TStyledButton;
var
  I: Integer;
begin
  for I := 0 to ComponentCount -1 do
  begin
    if (Components[I] is TStyledButton) then
    begin
      Result := TStyledButton(Components[I]);
      if Result.Focused then
        Exit;
    end;
  end;
  Result := FFocusedButton;
end;

procedure TStyledTaskDialogForm.FormKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
var
  LButton: TStyledButton;
begin
  if Key = VK_ESCAPE then
    CancelButton.Click
  else if key = VK_RETURN then
  begin
    LButton := GetFocusedButton;
    if LButton <> nil then
      LButton.Click
    else
      OKButton.Click;
  end;
end;

procedure TStyledTaskDialogForm.PlayMessageDlgSound;
const
  Sounds: array [TMsgDlgType] of integer = (
    MB_ICONEXCLAMATION, MB_ICONHAND, MB_OK,
    MB_ICONQUESTION, MB_ICONASTERISK);
begin
  MessageBeep(Sounds[FDialogType]);
end;

procedure TStyledTaskDialogForm.FormShow(Sender: TObject);
begin
  Screen.Cursor := crHourGlass;
  try
    ShowDialogForm;
    AdjustButtonsCaption;
    AdjustWidth;
    AutoSizeLabel.AutoSize := True;
    AdjustHeight;
    PlayMessageDlgSound;
    FocusDefaultButton;
  finally
    Screen.Cursor := crDefault;
  end;
end;

function TStyledTaskDialogForm.GetFooterText: string;
begin
  Result := FooterTextLabel.Caption;
end;

function TStyledTaskDialogForm.GetHelpContext: Integer;
begin
  Result := inherited HelpContext;
end;

procedure TStyledTaskDialogForm.GetIconNameAndIndex(
  ATaskDialog: TMsgDlgType; out AImageName: string; out AImageIndex: Integer);
const
  ImageNames: array[TMsgDlgType] of string =
    ('Warning', 'Error', 'Information', 'Confirmation', 'Custom');
begin
  AImageName := ImageNames[ATaskDialog];
  AImageIndex := Ord(ATaskDialog);
end;

procedure TStyledTaskDialogForm.GetIconNameAndIndex(
  ATaskDialogIcon: TTaskDialogIcon; out AImageName: string; out AImageIndex: Integer);
const
  ImageNames: array[tdiNone..tdiShield] of string =
    ('Custom', 'Warning', 'Error', 'Information', 'Shield');
  ImageIndexes: array[tdiNone..tdiShield] of integer = (4, 0, 1, 2, 5);
begin
  AImageName := ImageNames[ATaskDialogIcon];
  AImageIndex := ImageIndexes[ATaskDialogIcon];
end;

function TStyledTaskDialogForm.GetText: string;
begin
  Result := TextLabel.Caption;
end;

function TStyledTaskDialogForm.GetTitle: string;
begin
  Result :=  TitleLabel.Caption;
end;

procedure TStyledTaskDialogForm.HelpButtonClick(Sender: TObject);
begin
  Application.HelpContext(HelpContext);
end;

procedure TStyledTaskDialogForm.AdjustButtonsCaption;
begin
  YesButton.Caption := STR_YES;
  NoButton.Caption := STR_NO;
  OKButton.Caption := STR_OK;
  CancelButton.Caption := STR_CANCEL;
  HelpButton.Caption := STR_HELP;
  RetryButton.Caption := STR_RETRY;
  AbortButton.Caption := STR_ABORT;
  IgnoreButton.Caption := STR_IGNORE;
  AllButton.Caption := STR_ALL;
  NoToAllButton.Caption := STR_NOTOALL;
  YesToAllButton.Caption := STR_YESTOALL;
  CloseButton.Caption := STR_CLOSE;
end;

procedure TStyledTaskDialogForm.Loaded;
begin
  TextLabel.Align := alTop;
  inherited;
end;

procedure TStyledTaskDialogForm.InitDlgButtonsWithFamily(const AFamily: TStyledButtonFamily);

  procedure UpdateButtonStyle(const AButton: TStyledButton);
  begin
    if AButton.Visible then
      AButton.SetButtonStyle(AFamily, AButton.ModalResult);
  end;

begin
  UpdateButtonStyle(YesButton);
  UpdateButtonStyle(NoButton);
  UpdateButtonStyle(OKButton);
  UpdateButtonStyle(CancelButton);
  UpdateButtonStyle(AbortButton);
  UpdateButtonStyle(RetryButton);
  UpdateButtonStyle(IgnoreButton);
  UpdateButtonStyle(AllButton);
  UpdateButtonStyle(NoToAllButton);
  UpdateButtonStyle(YesToAllButton);
  UpdateButtonStyle(HelpButton);
  UpdateButtonStyle(CloseButton);
end;

{ TTaskDialogLauncherHandler }

function TTaskDialogLauncherHandler.DoExecute(ParentWnd: HWND;
  const ADialogType: TMsgDlgType;
  const ATaskDialog: TCustomTaskDialog;
  const ADialogBtnFamily: TStyledButtonFamily): Boolean;
var
  LForm: TStyledTaskDialogForm;
  LFont: TFont;
  LDlgBtnFamily: TStyledButtonFamily;
begin
  LForm := TaskDialogFormClass.Create(nil);
  try
    LForm.FTaskDialog := ATaskDialog;
    LForm.FDialogType := ADialogType;
    LForm.FDialogBtnFamily := ADialogBtnFamily;
    LFont := GetDialogFont;
    LDlgBtnFamily := GetDialogBtnFamily;
    if Assigned(LFont) then
      LForm.SetDialogFont(LFont)
    else
      LForm.SetDialogFont(Screen.MessageFont);
    LForm.ShowModal;
    ATaskDialog.ModalResult := LForm.ModalResult;
    Result := True;
  finally
    LForm.Free;
  end;
end;

initialization
  TaskDialogFormClass := TStyledTaskDialogForm;
  //Create handler for execute custom TaskDialog Form
  DialogLauncher := TTaskDialogLauncherHandler.Create;
  //Register the handler
  RegisterCustomExecute(DialogLauncher);
  //Init default Dialog buttons Styles
  SetLength(DlgButtonClasses, Ord(TMsgDlgBtn.mbClose)+1);

end.
