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
  Vcl.ColorButtonStyles,
  Vcl.ButtonGroup;

type
  TTaskDialogLauncherHandler = class(TInterfacedObject, ITaskDialogLauncher)
    function DoExecute(ParentWnd: HWND;
      const ADialogType: TMsgDlgType;
      const ATaskDialog: TStyledTaskDialog;
      const ADialogBtnFamily: TStyledButtonFamily): Boolean;
  end;

  TStyledTaskDialogForm = class(TForm)
    FooterPanel: TPanel;
    CenterPanel: TPanel;
    ImagePanel: TPanel;
    ButtonsPanel: TPanel;
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
    FooterIconPanel: TPanel;
    FooterTextLabel: TLinkLabel;
    IconContainer: TPanel;
    CommandLinksPanel: TPanel;
    VerificationPanel: TPanel;
    RadioGroupPanel: TPanel;
    VerificationCheckBox: TCheckBox;
    procedure FormCreate(Sender: TObject);
    procedure ButtonClick(Sender: TObject);
    procedure FormKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
    procedure FormShow(Sender: TObject);
    procedure TextLabelLinkClick(Sender: TObject; const Link: string;
      LinkType: TSysLinkType);
    procedure FormDestroy(Sender: TObject);
    procedure HelpButtonClick(Sender: TObject);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
    procedure VerificationCheckBoxClick(Sender: TObject);
    procedure RadioButtonClick(Sender: TObject);
  private
    FFocusedButton: TStyledButton;
    FCustomIcons: TStyledDialogIcons;
    FTaskDialog: TCustomTaskDialog;
    FDialogType: TMsgDlgType;
    FDialogBtnFamily: TStyledButtonFamily;
    FCommonButtons: TTaskDialogCommonButtons;
    FDefaultButton: TTaskDialogCommonButton;
    FButtons: TTaskDialogButtons;
    FFooterIcon: TTaskDialogIcon;
    FCustomMainIcon: TIcon;
    FCustomFooterIcon: TIcon;
    FMainIcon: TTaskDialogIcon;
    FOnTimer: TTaskDlgTimerEvent;
    FRadioButtons: TTaskDialogButtons;
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
    procedure AdjustControlsTopPos;
    procedure AdjustButtonsCaption;
    procedure SetButtons(const AValue: TTaskDialogButtons);
    procedure PlayMessageDlgSound;
    procedure FocusDefaultButton;
    procedure LoadDialogImage;
    procedure SetFocusToButton(AStyledButton: TStyledButton);
    procedure SetFooterText(const AValue: string);
    function GetFooterText: string;
    procedure SetVerificationText(const AValue: string);
    function GetVerificationText: string;
    function GetFocusedButton: TStyledButton;
    procedure InitDlgButtonsWithFamily(const AFamily: TStyledButtonFamily);
    procedure UpdateButtonVisibility;
    procedure SetFooterIcon(const AValue: TTaskDialogIcon);
    procedure SetCustomMainIcon(const AValue: TIcon);
    procedure SetCustomFooterIcon(const AValue: TIcon);
    procedure SetMainIcon(const AValue: TTaskDialogIcon);
    procedure AddCustomButtons(const AButtons: TTaskDialogButtons);
    function FindButton(const AModalResult: TModalResult): TStyledButton;
    procedure SetMainIconSize(const AValue: Integer);
    function GetMainIconSize: Integer;
    function ModalResultToCommonButton(const AModalResult: TModalResult;
        out ACommonButton: TTaskDialogCommonButton): Boolean;
    procedure SetRadioButtons(const AValue: TTaskDialogButtons);
    function UsingCommandLinks: Boolean;
    property Buttons: TTaskDialogButtons read FButtons write SetButtons;
    property RadioButtons: TTaskDialogButtons read FRadioButtons write SetRadioButtons;
    property CommonButtons: TTaskDialogCommonButtons read FCommonButtons write FCommonButtons default [tcbOk, tcbCancel];
    property DefaultButton: TTaskDialogCommonButton read FDefaultButton write FDefaultButton default tcbOk;
(*
    property ExpandButtonCaption: string read FExpandButtonCaption write FExpandButtonCaption;
    property Expanded: Boolean read FExpanded;
    property ExpandedText: string read FExpandedText write SetExpandedText;
    property Flags: TTaskDialogFlags read FFlags write SetFlags default [tfAllowDialogCancellation];
    property Handle: HWND read FHandle;
*)
    property MainIcon: TTaskDialogIcon read FMainIcon write SetMainIcon default tdiInformation;
    property CustomMainIcon: TIcon read FCustomMainIcon write SetCustomMainIcon;
    property MainIconSize: Integer read GetMainIconSize write SetMainIconSize default DEFAULT_MAIN_ICON_SIZE;

    property CustomFooterIcon: TIcon read FCustomFooterIcon write SetCustomFooterIcon;
    property FooterIcon: TTaskDialogIcon read FFooterIcon write SetFooterIcon default tdiNone;
    property FooterText: string read GetFooterText write SetFooterText;
    property VerificationText: string read GetVerificationText write SetVerificationText;
    property HelpContext: Integer read GetHelpContext write SetHelpContext default 0;
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
*)
    property OnTimer: TTaskDlgTimerEvent read FOnTimer write FOnTimer;
  protected
    class function CanUseAnimations: Boolean; virtual; abstract;
    function TaskDialogIconToImageIndex(
      const ATaskDialogIcon: TTaskDialogIcon): Integer;
    procedure UpdateCustomIcons;
    procedure Loaded; override;
    procedure LoadImage(const AImageIndex: TImageIndex;
      AImageName: string); virtual; abstract;
    procedure LoadCustomMainIcon(const AIcon: TIcon;
      const ATaskDialogIcon: TTaskDialogIcon); virtual;
    procedure LoadCustomFooterIcon(const AIcon: TIcon;
      const ATaskDialogIcon: TTaskDialogIcon); virtual;
    procedure DefaultDialogSize(out AClientWidth, AClientHeight, AImageSize: Integer); virtual;
    function GetDefaultButton(const ADefaultButton: TTaskDialogCommonButton): TStyledButton; virtual;
    property TaskDialog: TCustomTaskDialog read FTaskDialog;
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
  _DialogLauncher: ITaskDialogLauncher;
  _AnimatedTaskDialogFormClass, _TaskDialogFormClass: TStyledTaskDialogFormClass;
  _DlgButtonClasses: TButtonClasses;
  _DialogPosition: Vcl.Forms.TPosition;

procedure RegisterTaskDialogFormClass(AFormClass: TStyledTaskDialogFormClass);
begin
  if AFormClass.CanUseAnimations then
    _AnimatedTaskDialogFormClass := AFormClass
  else
    _TaskDialogFormClass := AFormClass;
  UseStyledDialogForm(True);
end;

procedure UnregisterTaskDialogFormClass;
begin
  _AnimatedTaskDialogFormClass := nil;
  _TaskDialogFormClass := nil;
end;

procedure UseStyledDialogForm(const AActivate: Boolean);
begin
  if AActivate then
    RegisterCustomExecute(_DialogLauncher)
  else
    UnregisterCustomExecute;
end;

{ TStyledTaskDialogForm }

procedure TStyledTaskDialogForm.SetRadioButtons(const AValue: TTaskDialogButtons);
var
  I: Integer;
  LTaskDialogRadioButtonItem: TTaskDialogRadioButtonItem;
  LRadioButton, LLastButton: TRadioButton;
  LHeight: Integer;
begin
  LHeight := RadioGroupPanel.Height;
  FRadioButtons := AValue;
  LLastButton := nil;
  for I := 0 to FRadioButtons.Count -1 do
  begin
    LTaskDialogRadioButtonItem := FRadioButtons[I] as TTaskDialogRadioButtonItem;
    LRadioButton := TRadioButton.Create(Self);
    LRadioButton.Tag := 100+I;
    LRadioButton.Caption := LTaskDialogRadioButtonItem.Caption;
    LRadioButton.Parent := RadioGroupPanel;
    LRadioButton.Align := alTop;
    LRadioButton.Height := LHeight;
    if Assigned(LLastButton) then
      LRadioButton.Top := LLastButton.Top + LLastButton.Height;
    LRadioButton.OnClick := RadioButtonClick;
    if LTaskDialogRadioButtonItem.Default then
      LRadioButton.Checked := True;
    RadioGroupPanel.Visible := True;
    RadioGroupPanel.Height := LHeight * (I+1);
    LLastButton := LRadioButton;
  end;
end;

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
      TabOrder := LStyledButton.TabOrder -1;
      //LStyledButton.Caption := LTaskDialogButtonItem.Caption;
      if LTaskDialogButtonItem.Default then
        SetFocusToButton(LStyledButton);
    end;
  end;
end;

procedure TStyledTaskDialogForm.SetCustomMainIcon(const AValue: TIcon);
begin
  if FCustomMainIcon <> AValue then
  begin
    FCustomMainIcon := AValue;
    LoadCustomMainIcon(FCustomMainIcon, FMainIcon);
  end;
end;

procedure TStyledTaskDialogForm.SetCustomFooterIcon(const AValue: TIcon);
begin
  if FCustomFooterIcon <> AValue then
  begin
    FCustomMainIcon := AValue;
    LoadCustomFooterIcon(FCustomFooterIcon, FfooterIcon);
  end;
end;

procedure TStyledTaskDialogForm.SetFocusToButton(AStyledButton: TStyledButton);
begin
  if AStyledButton.CanFocus then
    AStyledButton.SetFocus;
  FFocusedButton := AStyledButton;
end;

procedure TStyledTaskDialogForm.SetMainIcon(const AValue: TTaskDialogIcon);
begin
  if FMainIcon <> AValue then
  begin
    FMainIcon := AValue;
    LoadCustomMainIcon(FCustomMainIcon, FMainIcon);
  end;
end;

procedure TStyledTaskDialogForm.SetMainIconSize(const AValue: Integer);
begin
  if AValue <> ImagePanel.Width then
  begin
    ImagePanel.Width := AValue;
    IconContainer.Height := AValue;
  end;
end;

procedure TStyledTaskDialogForm.SetFooterIcon(const AValue: TTaskDialogIcon);
begin
  if FFooterIcon <> AValue then
  begin
    FFooterIcon := AValue;
    LoadCustomFooterIcon(FCustomFooterIcon, FFooterIcon);
  end;
end;

procedure TStyledTaskDialogForm.SetFooterText(const AValue: string);
var
  LRows: TStringList;
begin
  if AValue <> '' then
  begin
    LRows := TStringList.Create;
    try
      LRows.Text := AValue;
      FooterTextLabel.Caption := AValue;
      if LRows.Count > 1 then
        FooterPanel.Height := FooterPanel.Height * LRows.Count
      else
        FooterPanel.Height := 32;
      FooterPanel.Visible := True;
    finally
      LRows.Free;
    end;
  end
  else
    FooterPanel.Visible := False;
end;

function TStyledTaskDialogForm.GetDefaultButton(
  const ADefaultButton :TTaskDialogCommonButton): TStyledButton;
begin
  case ADefaultButton of
    tcbOk: Result := OKButton;
    tcbYes: Result := YesButton;
    tcbNo: Result := NoButton;
    tcbCancel: Result := CancelButton;
    tcbRetry: Result := RetryButton;
    tcbClose: Result := CloseButton;
  else
    Result := nil;
  end;
end;

procedure TStyledTaskDialogForm.FocusDefaultButton;
var
  LButton: TStyledButton;
begin
  if not Assigned(FFocusedButton) then
  begin
    LButton := GetDefaultButton(DefaultButton);
    if Assigned(LButton) then
      SetFocusToButton(LButton);
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
  LRadioGroupPanelHeight: Integer;
  LCommandLinksPanelHeight: Integer;
  LVerificationPanelHeight: Integer;
  LButtonsPanelHeight: Integer;
  LFooterPanelHeight: Integer;
  LMinHeight, LCalcHeight: Integer;
  LImageSize, LWidth, LHeight, LMargins: Integer;
begin
  LMargins := Round(ImagePanel.Left * 2);
  DefaultDialogSize(LWidth, LHeight, LImageSize);

  //Calculate Height of Form, based on Visible Components
  if RadioGroupPanel.Visible then
    LRadioGroupPanelHeight := RadioGroupPanel.Height + LMargins
  else
    LRadioGroupPanelHeight := 0;

  if FooterPanel.Visible then
    LFooterPanelHeight := FooterPanel.Height + LMargins
  else
    LFooterPanelHeight := 0;

  if VerificationPanel.Visible then
    LVerificationPanelHeight := VerificationPanel.Height + LMargins
  else
    LVerificationPanelHeight := 0;

  if CommandLinksPanel.Visible then
    LCommandLinksPanelHeight := CommandLinksPanel.Height + LMargins
  else
    LCommandLinksPanelHeight := 0;

  if ButtonsPanel.Visible then
    LButtonsPanelHeight := ButtonsPanel.Height + LMargins
  else
    LButtonsPanelHeight := 0;

  LCalcHeight :=
    AutoSizeLabel.Height + LMargins +
    TitleLabel.Height + LMargins +
    LRadioGroupPanelHeight +
    LFooterPanelHeight +
    LVerificationPanelHeight +
    LButtonsPanelHeight +
    LCommandLinksPanelHeight +
    LMargins;
  LMinHeight := LImageSize + LMargins +
    LRadioGroupPanelHeight +
    LFooterPanelHeight +
    LVerificationPanelHeight +
    LButtonsPanelHeight +
    LCommandLinksPanelHeight +
    LMargins;

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
  if ButtonsPanel.Visible then
  begin
    for I := 0 to ComponentCount - 1 do
    begin
      if Components[I] is TStyledButton then
      begin
        LStyledButton := TStyledButton(Components[I]);
        if LStyledButton.Visible and (LStyledButton.Align = alRight) then
          LFormWidth := LFormWidth + LStyledButton.Width + LMargins + LMargins;
      end;
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

procedure TStyledTaskDialogForm.SetVerificationText(const AValue: string);
var
  LRows: TStringList;
begin
  if AValue <> '' then
  begin
    LRows := TStringList.Create;
    try
      LRows.Text := AValue;
      VerificationCheckBox.Caption := AValue;
      if LRows.Count > 1 then
        VerificationPanel.Height := VerificationPanel.Height * LRows.Count;
      VerificationPanel.Visible := True;
    finally
      LRows.Free;
    end;
  end
  else
    FooterPanel.Visible := False;
end;

procedure TStyledTaskDialogForm.LoadCustomFooterIcon(const AIcon: TIcon;
  const ATaskDialogIcon: TTaskDialogIcon);
begin
  //In descendand form implements this method
  ;
end;

procedure TStyledTaskDialogForm.LoadCustomMainIcon(const AIcon: TIcon;
  const ATaskDialogIcon: TTaskDialogIcon);
begin
  //In descendand form implements this method
  ;
end;

procedure TStyledTaskDialogForm.LoadDialogImage;
var
  LIconName: string;
  LIconIndex: Integer;
  LMargins: Integer;
begin
  if (tfUseHiconMain in FTaskDialog.Flags) and Assigned(FTaskDialog.CustomMainIcon) then
  begin
    LoadCustomMainIcon(FTaskDialog.CustomMainIcon, FTaskDialog.MainIcon);
  end
  else
  begin
    GetIconNameAndIndex(FMainIcon, LIconName, LIconIndex);
    LoadImage(LIconIndex, LIconName);
  end;
  LMargins := CenterPanel.Margins.Left + CenterPanel.Margins.Right;
  if ImagePanel.Visible then
    Inc(LMargins, ImagePanel.Width);
  RadioGroupPanel.Margins.Left := LMargins;
  CommandLinksPanel.Margins.Left := LMargins;
end;

function TStyledTaskDialogForm.FindButton(const AModalResult: TModalResult): TStyledButton;
var
  I: Integer;
begin
  for I := 0 to Self.ComponentCount - 1 do
  begin
    if Components[I] is TStyledButton then
    begin
      Result := TStyledButton(Components[I]);
      if Result.ModalResult = AModalResult then
      begin
        Result.Visible := True;
        Exit;
      end;
    end;
  end;
  Result := nil;
end;

procedure TStyledTaskDialogForm.UpdateButtonVisibility;

  function IsButtonVisible(const AButton: TStyledButton): Boolean;
  var
    I: Integer;
    LTaskDialogButtonItem: TTaskDialogBaseButtonItem;
  begin
    Result := False;
    for I := FButtons.Count - 1 downto 0 do
    begin
      LTaskDialogButtonItem := FButtons[I];
      if AButton.ModalResult = LTaskDialogButtonItem.ModalResult then
      begin
        Result := True;
        break;
      end;
    end;
    if Result and CommandLinksPanel.Visible then
    begin
      for I := 0 to CommandLinksPanel.ComponentCount -1 do
      if CommandLinksPanel.Components[I] is TStyledButton and
        (AButton.ModalResult = TStyledButton(CommandLinksPanel.Components[I]).ModalResult) then
      begin
        //Hide Common Button if used as CommandLink
        Result := False;
        break;
      end;
    end;
  end;

begin
  YesButton.Visible := (tcbYes in FCommonButtons) or IsButtonVisible(YesButton);
  NoButton.Visible := (tcbNo in FCommonButtons) or IsButtonVisible(NoButton);
  OKButton.Visible := (tcbOk in FCommonButtons) or IsButtonVisible(OKButton);
  CancelButton.Visible := (tcbCancel in FCommonButtons) or IsButtonVisible(CancelButton);
  RetryButton.Visible := (tcbRetry in FCommonButtons) or IsButtonVisible(RetryButton);
  CloseButton.Visible := (tcbClose in FCommonButtons) or IsButtonVisible(CloseButton);
  if not UsingCommandLinks then
  begin
    AbortButton.Visible := IsButtonVisible(AbortButton);
    IgnoreButton.Visible := IsButtonVisible(IgnoreButton);
    AllButton.Visible := IsButtonVisible(AllButton);
    NoToAllButton.Visible := IsButtonVisible(NoToAllButton);
    YesToAllButton.Visible := IsButtonVisible(YesToAllButton);
    HelpButton.Visible := IsButtonVisible(HelpButton);
  end
  else
  begin
    AbortButton.Visible := False;
    IgnoreButton.Visible := False;
    AllButton.Visible := False;
    NoToAllButton.Visible := False;
    YesToAllButton.Visible := False;
    HelpButton.Visible := False;
  end;
end;

function TStyledTaskDialogForm.UsingCommandLinks: Boolean;
begin
  Result := tfUseCommandLinks in FTaskDialog.Flags;
end;

procedure TStyledTaskDialogForm.AddCustomButtons(const AButtons: TTaskDialogButtons);
var
  LButtonItem: TTaskDialogButtonItem;
  I: Integer;
  LLastButton, LStyledButton: TStyledButton;
  LUsingCommandLinks: Boolean;
  LCommandLinkHeight: Integer;
  LCommonButton: TTaskDialogCommonButton;
begin
  LLastButton := nil;
  LCommandLinkHeight := CommandLinksPanel.Height;
  LUsingCommandLinks := UsingCommandLinks;
  if LUsingCommandLinks then
  begin
    CommandLinksPanel.Visible := True;
    if CommonButtons = [] then
    begin
      ButtonsPanel.Visible := False;
      ButtonsPanel.Height := 0;
    end
    else
      ButtonsPanel.Visible := True;
  end;

  CommandLinksPanel.Height := 0;
  for I := AButtons.Count - 1 downto 0 do
  begin
    LButtonItem := AButtons.Items[I] as TTaskDialogButtonItem;
    // Find if the Button is already present in the Form
    // for CommandLinks the buttons must be always created
    LStyledButton := FindButton(LButtonItem.ModalResult);
    if Assigned(LStyledButton) and LUsingCommandLinks then
      LStyledButton.Visible := False;
    if not Assigned(LStyledButton) or LUsingCommandLinks then
    begin
      LStyledButton := TStyledButton.Create(Self);
      LStyledButton.SetButtonStyle(FDialogBtnFamily, LButtonItem.ModalResult);
    end;
    LStyledButton.Caption := LButtonItem.Caption;
    LStyledButton.Default := LButtonItem.Default;
    LStyledButton.ElevationRequired := LButtonItem.ElevationRequired;
    LStyledButton.Enabled := LButtonItem.Enabled;
    if LUsingCommandLinks and ModalResultToCommonButton(LStyledButton.ModalResult, LCommonButton) then
    begin
      LStyledButton.Parent := CommandLinksPanel;
      LStyledButton.AlignWithMargins := True;
      LStyledButton.Margins.Top := 1;
      LStyledButton.Margins.Bottom := 1;
      LStyledButton.Align := alBottom;
      LStyledButton.Style := bsCommandLink;
      LStyledButton.CommandLinkHint := LButtonItem.CommandLinkHint;
      LStyledButton.Height := LCommandLinkHeight - 2;
      CommandLinksPanel.Height := CommandLinksPanel.Height + LCommandLinkHeight;
      if LButtonItem.Default then
      begin
        LStyledButton.Default := True;
        Self.ActiveControl := LStyledButton;
      end;
      if Assigned(LLastButton) then
      begin
        LStyledButton.TabOrder := LStyledButton.TabOrder -1;
        LStyledButton.Top := LLastButton.Top - LLastButton.Height;
      end;
      LLastButton := LStyledButton;
    end
    else
    begin
      LStyledButton.Parent := ButtonsPanel;
      LStyledButton.Align := alRight;
      LStyledButton.AlignWithMargins := True;
      LStyledButton.Margins.Assign(YesButton.Margins);
      if Assigned(LLastButton) then
        LStyledButton.Left := LLastButton.Left - LLastButton.Width;
      LLastButton := LStyledButton;
    end;
  end;
  if Self.ActiveControl = nil then
    Self.ActiveControl := LLastButton;
end;

procedure TStyledTaskDialogForm.AdjustControlsTopPos;
var
  LTop: Integer;

  procedure AddHeight(const AControl: TControl);
  begin
    if AControl.Visible then
      Inc(LTop, AControl.Height);
  end;

  procedure SetTop(const AControl: TControl);
  begin
    if AControl.Visible then
    begin
      AControl.Top := LTop;
      Inc(LTop, AControl.Height);
    end;
  end;

begin
  LTop := 0;
  //Calculating Top
  AddHeight(RadioGroupPanel);
  AddHeight(CommandLinksPanel);
  AddHeight(VerificationPanel);
  AddHeight(ButtonsPanel);
  AddHeight(FooterPanel);
  //Calculating Top as difference
  LTop := ClientHeight - LTop;

  //Setting Top for components starting from Top Visible Component
  SetTop(RadioGroupPanel);
  SetTop(CommandLinksPanel);
  SetTop(VerificationPanel);
  SetTop(ButtonsPanel);
  SetTop(FooterPanel);
end;

procedure TStyledTaskDialogForm.ShowDialogForm;
begin
  //Initialize components based on ATaskDialog attributes
  Caption := FTaskDialog.Caption;
  HelpContext := FTaskDialog.HelpContext;
  CommonButtons := FTaskDialog.CommonButtons;
  DefaultButton := FTaskDialog.DefaultButton;
  Buttons := FTaskDialog.Buttons;
  RadioButtons := FTaskDialog.RadioButtons;
  AddCustomButtons(FTaskDialog.Buttons);
  UpdateButtonVisibility;

  if FDialogBtnFamily <> '' then
    InitDlgButtonsWithFamily(FDialogBtnFamily);

  MainIcon := FTaskDialog.MainIcon;
  CustomMainIcon := FTaskDialog.CustomMainIcon;

(*
    property ExpandButtonCaption: string read FExpandButtonCaption write FExpandButtonCaption;
    property Expanded: Boolean read FExpanded;
    property ExpandedText: string read FExpandedText write SetExpandedText;
    property Flags: TTaskDialogFlags read FFlags write SetFlags default [tfAllowDialogCancellation];
    property Handle: HWND read FHandle;
*)
    CustomFooterIcon := FTaskDialog.CustomFooterIcon;
    FooterIcon := FTaskDialog.FooterIcon;
    FooterText := FTaskDialog.FooterText;
(*
    property ProgressBar: TTaskDialogProgressBar read FProgressBar write FProgressBar;
    property RadioButton: TTaskDialogRadioButtonItem read FRadioButton;
    property RadioButtons: TTaskDialogButtons read FRadioButtons write SetRadioButtons;
*)
    TextMessage := FTaskDialog.Text;
    TitleMessage :=  FTaskDialog.Title;
(*
    property URL: string read FURL;
*)
    VerificationText := FTaskDialog.VerificationText;

  //Load and show Image
  LoadDialogImage;
end;

function TStyledTaskDialogForm.TaskDialogIconToImageIndex(
  const ATaskDialogIcon: TTaskDialogIcon): Integer;
begin
  case ATaskDialogIcon of
    tdiWarning: Result := 0;
    tdiError: Result := 1;
    tdiInformation: Result := 2;
    tdiShield: Result := 5;
  else
    Result := 4;
  end;
end;

procedure TStyledTaskDialogForm.TextLabelLinkClick(Sender: TObject;
  const Link: string; LinkType: TSysLinkType);
begin
  if (LinkType = sltURL) and Assigned(FTaskDialog.OnHyperlinkClicked) then
  begin
    if (FTaskDialog is TStyledTaskDialog) then
      TStyledTaskDialog(FTaskDialog).DoOnHyperlinkClicked(Link)
  end
  else
    ShellExecute(Self.Handle, 'open' , PChar(Link), nil, nil, SW_SHOW );
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

procedure TStyledTaskDialogForm.VerificationCheckBoxClick(Sender: TObject);
begin
  if VerificationCheckBox.Checked then
    FTaskDialog.Flags := FTaskDialog.Flags + [tfVerificationFlagChecked]
  else
    FTaskDialog.Flags := FTaskDialog.Flags - [tfVerificationFlagChecked];
  if Assigned(FTaskDialog.OnVerificationClicked) then
    FTaskDialog.OnVerificationClicked(FTaskDialog);
end;

procedure TStyledTaskDialogForm.ButtonClick(Sender: TObject);
var
  LModalResult: TModalResult;
begin
  LModalResult := (Sender as TStyledButton).ModalResult;
  Close;
  Self.ModalResult := LModalResult;
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
  AImageSize := Round(DEFAULT_MAIN_ICON_SIZE * LScaleFactor);
end;

procedure TStyledTaskDialogForm.FormClose(Sender: TObject;
  var Action: TCloseAction);
var
  LCanClose: Boolean;
begin
  LCanClose := True;
  if Assigned(FTaskDialog.OnButtonClicked) then
    FTaskDialog.OnButtonClicked(Sender, ModalResult, LCanClose);
  if not LCanClose then
    Action := TCloseAction.caNone;
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
  else if Key = VK_RETURN then
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

procedure TStyledTaskDialogForm.RadioButtonClick(Sender: TObject);
var
  LRadioButton: TRadioButton;
begin
  if FTaskDialog is TStyledTaskDialog then
  begin
    LRadioButton := Sender as TRadioButton;
    TStyledTaskDialog(FTaskDialog).DoOnRadioButtonClicked(LRadioButton.Tag);
  end;
end;

procedure TStyledTaskDialogForm.FormShow(Sender: TObject);
begin
  Screen.Cursor := crHourGlass;
  try
    AdjustButtonsCaption;
    ShowDialogForm;
    AdjustWidth;
    AutoSizeLabel.AutoSize := True;
    AdjustHeight;
    AdjustControlsTopPos;
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

function TStyledTaskDialogForm.GetMainIconSize: Integer;
begin
  Result := ImagePanel.Width;
end;

function TStyledTaskDialogForm.GetText: string;
begin
  Result := TextLabel.Caption;
end;

function TStyledTaskDialogForm.GetTitle: string;
begin
  Result :=  TitleLabel.Caption;
end;

function TStyledTaskDialogForm.GetVerificationText: string;
begin
  Result := VerificationCheckBox.Caption;
end;

procedure TStyledTaskDialogForm.HelpButtonClick(Sender: TObject);
begin
  ModalResult := mrNone;
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
  Position := _DialogPosition;
  TextLabel.Align := alTop;
  inherited;
end;

function TStyledTaskDialogForm.ModalResultToCommonButton(
  const AModalResult: TModalResult;
  out ACommonButton: TTaskDialogCommonButton): Boolean;
begin
  case AModalResult of
    mrOK: ACommonButton := tcbOk;
    mrYes: ACommonButton := tcbYes;
    mrNo: ACommonButton := tcbNo;
    mrCancel: ACommonButton := tcbCancel;
    mrRetry: ACommonButton := tcbRetry;
    mrClose: ACommonButton := tcbClose;
  else
    ACommonButton := tcbOk;
  end;
  Result := ACommonButton in CommonButtons;
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
  const ATaskDialog: TStyledTaskDialog;
  const ADialogBtnFamily: TStyledButtonFamily): Boolean;
var
  LForm: TStyledTaskDialogForm;
  LFont: TFont;
  LDlgBtnFamily: TStyledButtonFamily;
begin
  if tfPositionRelativeToWindow in ATaskDialog.Flags then
    _DialogPosition := poOwnerFormCenter
  else
    _DialogPosition := poScreenCenter;
  if Assigned(_AnimatedTaskDialogFormClass) then
    LForm := _AnimatedTaskDialogFormClass.Create(nil)
  else
    LForm := _TaskDialogFormClass.Create(nil);
  try
    if Assigned(ATaskDialog.OnDialogConstructed) then
      ATaskDialog.OnDialogConstructed(ATaskDialog);

    //Assign all events of TaskDialog
    LForm.OnTimer := ATaskDialog.OnTimer;

    //Assign called TaskDialog component
    LForm.FTaskDialog := ATaskDialog;

    //Assign Size of Main Icon
    LForm.MainIconSize := ATaskDialog.MainIconSize;

    LForm.FDialogType := ADialogType;
    LForm.FDialogBtnFamily := ADialogBtnFamily;
    LFont := GetDialogFont;
    LForm.AlphaBlendValue := GetDialogAlphaBlendValue;
    LForm.AlphaBlend := LForm.AlphaBlendValue <> DEFAULT_ALPHABLEND;
    LDlgBtnFamily := GetDialogBtnFamily;
    if Assigned(LFont) then
      LForm.SetDialogFont(LFont)
    else
      LForm.SetDialogFont(Screen.MessageFont);

    if Assigned(ATaskDialog.OnDialogCreated) then
      ATaskDialog.OnDialogCreated(ATaskDialog);

    //Show the Dialog in Modal mode
    LForm.ShowModal;

    ATaskDialog.ModalResult := LForm.ModalResult;
    ATaskDialog.Button := TTaskDialogButtonItem(ATaskDialog.Buttons.FindButton(ATaskDialog.ModalResult));
    //ATaskDialog.RadioButton := TTaskDialogRadioButtonItem(ATaskDialog.RadioButtons.FindButton(LRadioButton));
(*
    if LVerificationChecked then
      Include(FFlags, tfVerificationFlagChecked)
    else
      Exclude(FFlags, tfVerificationFlagChecked);
*)
    Result := True;
  finally
    LForm.Free;
    if Assigned(ATaskDialog.OnDialogDestroyed) then
      ATaskDialog.OnDialogDestroyed(ATaskDialog);
  end;
end;

initialization
  _TaskDialogFormClass := TStyledTaskDialogForm;
  //Create handler for execute custom TaskDialog Form
  _DialogLauncher := TTaskDialogLauncherHandler.Create;

  _DialogPosition := poScreenCenter;
  //Register the handler
  RegisterCustomExecute(_DialogLauncher);
  //Init default Dialog buttons Styles
  SetLength(_DlgButtonClasses, Ord(TMsgDlgBtn.mbClose)+1);

end.
