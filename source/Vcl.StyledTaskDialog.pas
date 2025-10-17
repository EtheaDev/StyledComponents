{******************************************************************************}
{                                                                              }
{  StyledTaskDialog: a Task Dialog Component with StyleButtons                 }
{                                                                              }
{  Copyright (c) 2022-2025 (Ethea S.r.l.)                                      }
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
unit Vcl.StyledTaskDialog;

{$INCLUDE StyledComponents.inc}

interface

uses
  System.SysUtils
  , System.Classes
  , System.UITypes
  , WinApi.Windows
  , Vcl.Dialogs
  , Vcl.Graphics
  , Vcl.Forms
  , Vcl.ButtonStylesAttributes
  , Vcl.StyledButton
  ;

const
  DEFAULT_STYLEDDIALOG_ALPHABLEND = 255;
  DEFAULT_STYLEDDIALOG_MIN_WIDTH = 500;
  DEFAULT_STYLEDDIALOG_MIN_HEIGHT = 280;
  DEFAULT_STYLEDDIALOG_BUTTONSWIDTH = 74;
  DEFAULT_STYLEDDIALOG_BUTTONSHEIGHT = 30;
  MIN_STYLEDDIALOG_BUTTONSSIZE = 10;
  MAX_STYLEDDIALOG_BUTTONSWIDTH = 800;
  MAX_STYLEDDIALOG_BUTTONSHEIGHT = 300;

  {$IFDEF Use_Large_Dialog_Icons}
  DEFAULT_MAIN_ICON_SIZE = 128;
  {$ELSE}
  DEFAULT_MAIN_ICON_SIZE = 64;
  {$ENDIF}

  tdiQuestion = 5;

type
  //A new TTaskDialogIcon Value for StyledComponents to
  //include tdiQuestion Value

  TStyledDialogIcons = array[TMsgDlgType] of TIcon;
  TOnFindDialogButtonEvent = function (const AModalResult: TModalResult): TStyledButton of object;
  EStyledTaskDialogException = class(Exception);

{$WARN SYMBOL_PLATFORM OFF}

type
  TTaskDialogShow = procedure(
    const AStyledTaskDialogForm: TForm) of Object;

  { TStyledTaskDialogProgressBar }
  [ComponentPlatforms(pidWin32 or pidWin64)]
  TStyledTaskDialogProgressBar = class(TTaskDialogProgressBar)
  end;

  { TStyledTaskDialog }
  [ComponentPlatforms(pidWin32 or pidWin64)]
  TStyledTaskDialog = class(TTaskDialog)
  private
    FHelpFile: string;
    FParentWnd: HWND;
    FPosition: TPoint;
    FMainIconSize: Integer;
    FDialogButtonsFamily: TStyledButtonFamily;
    FUseTitleInMessageDlg: Boolean;
    FAlphaBlendValue: Byte;
    FButtonsWidth: Integer;
    FButtonsHeight: Integer;
    FAutoClick: Boolean;
    FAutoClickDelay: Integer;
    FOnFindDialogButton: TOnFindDialogButtonEvent;
    FUseAnimations: Boolean;
    FUseAnimationLoop: Boolean;
    FUseAnimationInverse: Boolean;
    FUseMessageDefaultButton: Boolean;
    FMessageDefaultButton: TMsgDlgBtn;
    FOnDialogShow: TTaskDialogShow;
    FHideSystemCloseButton: Boolean;
    function IsDefaultFamily: Boolean;
    procedure SetAutoClick(const AValue: Boolean);
    procedure SetAutoClickDelay(const AValue: Integer);
    procedure SetAlphaBlendValue(const AValue: Byte);
    procedure SetButtonsHeight(const AValue: Integer);
    procedure SetButtonsWidth(const AValue: Integer);
    function GetHandle: HWND;
    procedure SetHandle(const AValue: HWND);
    procedure SetUseCommandLinks(const AValue: Boolean);
    function GetUseCommandLinks: Boolean;
    procedure SetMainIconSize(const AValue: Integer);
    procedure SetUseAnimationInverse(const AValue: Boolean);
    procedure SetUseAnimationLoop(const AValue: Boolean);
    procedure SetUseAnimations(const AValue: Boolean);
  strict protected
    function DoExecute(ParentWnd: HWND): Boolean; override;
    procedure DoOnHelp; override;
  protected
  public
    procedure DoOnExpandButtonClicked(Expanded: Boolean); override;
    procedure DoOnRadioButtonClicked(ButtonID: Integer); override;
    procedure DoOnHyperlinkClicked(const AURL: string); override;
    constructor Create(AOwner: TComponent); override;
    function Execute(ParentWnd: HWND): Boolean; overload; override;
    function FindDialogButton(const AModalResult: TModalResult): TStyledButton;
    function IsCustomPosition: Boolean;
    property MessageDefaultButton: TMsgDlgBtn read FMessageDefaultButton;
    property UseMessageDefaultButton: Boolean read FUseMessageDefaultButton;
    property HelpFile: string read FHelpFile write FHelpFile;
    property Handle: HWND read GetHandle write SetHandle;
    property OnFindDialogButton: TOnFindDialogButtonEvent read FOnFindDialogButton write FOnFindDialogButton;
  published
    //Additional properties compared to TTaskDialog
    property AutoClick: Boolean read FAutoClick write SetAutoClick default False;
    property AutoClickDelay: Integer read FAutoClickDelay write SetAutoClickDelay default DEFAULT_AUTOCLICK_DELAY;
    property DialogButtonsFamily: TStyledButtonFamily read FDialogButtonsFamily write FDialogButtonsFamily stored IsDefaultFamily;
    property UseCommandLinks: Boolean read GetUseCommandLinks write SetUseCommandLinks default False;
    property UseAnimations: Boolean read FUseAnimations write SetUseAnimations default false;
    property UseAnimationLoop: Boolean read FUseAnimationLoop write SetUseAnimationLoop default False;
    property UseAnimationInverse: Boolean read FUseAnimationInverse write SetUseAnimationInverse default False;
    property UseTitleInMessageDlg: Boolean read FUseTitleInMessageDlg write FUseTitleInMessageDlg default True;
    property AlphaBlendValue: Byte read FAlphaBlendValue write SetAlphaBlendValue default DEFAULT_STYLEDDIALOG_ALPHABLEND;
    property ButtonsWidth: Integer read FButtonsWidth write SetButtonsWidth default DEFAULT_STYLEDDIALOG_BUTTONSWIDTH;
    property ButtonsHeight: Integer read FButtonsHeight write SetButtonsHeight default DEFAULT_STYLEDDIALOG_BUTTONSHEIGHT;
    property HideSystemCloseButton: Boolean read FHideSystemCloseButton write FHideSystemCloseButton default False;
    property Position: TPoint read FPosition write FPosition stored IsCustomPosition;
    property MainIconSize: Integer read FMainIconSize write SetMainIconSize default DEFAULT_MAIN_ICON_SIZE;
    property OnDialogShow: TTaskDialogShow read FOnDialogShow write FOnDialogShow;
  end;

  //Abstraction of a Dialog Launcher
  ITaskDialogLauncher = interface
    ['{B2F16F98-C163-4706-A803-E624126D8DF6}']
    function DoExecute(ParentWnd: HWND;
      const ADialogType: TMsgDlgType;
      const ATaskDialog: TStyledTaskDialog;
      const ADialogBtnFamily: TStyledButtonFamily): boolean;
  end;


//Deprecated functions
function StyledMessageDlg(const Title, Msg: string; DlgType: TMsgDlgType;
  Buttons: TMsgDlgButtons; HelpCtx: Longint): Integer; overload; deprecated 'use StyledTaskMessageDlg(...) instead'
function StyledMessageDlg(const Title, Msg: string; DlgType: TMsgDlgType;
  Buttons: TMsgDlgButtons; HelpCtx: Longint; DefaultButton: TMsgDlgBtn): Integer; overload; deprecated 'use StyledTaskMessageDlgPos(...) instead'
function StyledTaskDlgPos(const Title, Msg: string; DlgType: TMsgDlgType;
  Buttons: TMsgDlgButtons; HelpCtx: Longint;
  X: Integer = -1; Y: Integer = -1): Integer; overload; deprecated 'use StyledTaskMessageDlgPos(...) or StyledTaskMessageDlg(...) instead'
function StyledTaskDlgPos(const Title, Msg: string; DlgType: TMsgDlgType;
  Buttons: TMsgDlgButtons; DefaultButton: TMsgDlgBtn; HelpCtx: Longint;
  X: Integer = -1; Y: Integer = -1): Integer; overload; deprecated 'use StyledTaskMessageDlgPos(...) or StyledTaskMessageDlg(...) instead'
function StyledMessageDlgPos(const Msg: string; DlgType: TMsgDlgType;
  Buttons: TMsgDlgButtons; DefaultButton: TMsgDlgBtn; HelpCtx: Longint;
  X: Integer; Y: Integer): Integer; overload; deprecated 'use StyledMessageDlgPos(...) changing position of DefaultButton param, after HelpCtx'

//Equivalent functions of standard MessageDlg functions
function StyledMessageDlg(const Msg: string; DlgType: TMsgDlgType;
  Buttons: TMsgDlgButtons; HelpCtx: Longint; DefaultButton: TMsgDlgBtn): Integer; overload;
function StyledMessageDlg(const Msg: string; DlgType: TMsgDlgType;
  Buttons: TMsgDlgButtons; HelpCtx: Longint): Integer; overload;
function StyledMessageDlg(const Msg: string; DlgType: TMsgDlgType;
  Buttons: TMsgDlgButtons; HelpCtx: Longint; DefaultButton: TMsgDlgBtn;
  CustomButtonCaptions: array of string): Integer; overload;

//Equivalent functions of standard MessageDlgPos functions
function StyledMessageDlgPos(const Msg: string; DlgType: TMsgDlgType;
  Buttons: TMsgDlgButtons; HelpCtx: Longint;
  X: Integer; Y: Integer): Integer; overload;
function StyledMessageDlgPos(const Msg: string; DlgType: TMsgDlgType;
  Buttons: TMsgDlgButtons; HelpCtx: Longint; DefaultButton: TMsgDlgBtn;
  X: Integer; Y: Integer): Integer; overload;

//Equivalent functions of standard TaskMessageDlg functions
function StyledTaskMessageDlg(const Title, Msg: string; DlgType: TMsgDlgType;
  Buttons: TMsgDlgButtons; HelpCtx: Longint): Integer; overload;
function StyledTaskMessageDlg(const Title, Msg: string; DlgType: TMsgDlgType;
  Buttons: TMsgDlgButtons; HelpCtx: Longint; DefaultButton: TMsgDlgBtn): Integer; overload;
//Equivalent functions of standard TaskMessageDlgPos functions
function StyledTaskMessageDlgPos(const Title, Msg: string; DlgType: TMsgDlgType;
  Buttons: TMsgDlgButtons; HelpCtx: Longint;
  X: Integer; Y: Integer): Integer; overload;
function StyledTaskMessageDlgPos(const Title, Msg: string; DlgType: TMsgDlgType;
  Buttons: TMsgDlgButtons; HelpCtx: Longint; DefaultButton: TMsgDlgBtn;
  X: Integer; Y: Integer): Integer; overload;

//Extra functions DoStyledTaskMessageDlg
function DoStyledTaskMessageDlg(const Instruction, Msg: string; DlgType: TMsgDlgType;
  Buttons: TMsgDlgButtons; HelpCtx : Longint = 0;
  const AutoClickDelay: Integer = -1; const UseCommandLinks: Boolean = False): Integer; overload;
function DoStyledTaskMessageDlgPos(const Instruction, Msg: string; DlgType: TMsgDlgType;
  Buttons: TMsgDlgButtons; HelpCtx: Longint; X: Integer; Y: Integer;
  const AutoClickDelay: Integer = -1; const UseCommandLinks: Boolean = False): Integer; overload;
function DoStyledTaskMessageDlgPosHelp(const Instruction, Msg: string; DlgType: TMsgDlgType;
  Buttons: TMsgDlgButtons; HelpCtx: Longint; X: Integer = -1; Y: Integer = -1;
  const AutoClickDelay: Integer = -1; const UseCommandLinks: Boolean = False;
  const HelpFileName: string = ''): Integer; overload;
function DoStyledTaskMessageDlgPosHelp(const Instruction, Msg: string; DlgType: TMsgDlgType;
  Buttons: TMsgDlgButtons; HelpCtx: Longint; DefaultButton: TMsgDlgBtn;
  X: Integer = -1; Y: Integer = -1;
  const AutoClickDelay: Integer = -1; const UseCommandLinks: Boolean = False;
  const HelpFileName: string = ''): Integer; overload;
function DoMessageDlgPosHelp(const Msg: string; DlgType: TMsgDlgType;
  Buttons: TMsgDlgButtons; HelpCtx: Integer; DefaultButton: TMsgDlgBtn;
  X: Integer = -1; Y: Integer = -1;
  const AutoClickDelay: Integer = -1;  const UseCommandLinks: Boolean = False;
  const HelpFileName: string = ''): Integer; overload;
function DoMessageDlgPosHelp(const Msg: string; DlgType: TMsgDlgType;
  Buttons: TMsgDlgButtons; HelpCtx: Longint;
  X: Integer = -1; Y: Integer = -1;
  const AutoClickDelay: Integer = -1; const UseCommandLinks: Boolean = False;
  const HelpFileName: string = ''): Integer; overload;

//Equivalent function of standard ShowMessage function
procedure StyledShowMessage(const Msg: string); overload;
procedure StyledShowMessageFmt(const Msg: string; Params: array of const); overload;

procedure SetUseTitleInMessageDlg(AValue: boolean);
procedure RegisterCustomExecute(const AShowStyledTaskDialog: ITaskDialogLauncher;
  const AButtonFamily: TStyledButtonFamily = '');
procedure UnregisterCustomExecute;
function GetTaskDlgType(const AIcon: TTaskDialogIcon): TMsgDlgType;
function GetDialogFont: TFont;
function GetDialogBtnFamily: TStyledButtonFamily;

function GetDialogTypeTitle(const DlgType: TMsgDlgType): string;
function GetDialogAlphaBlendValue: Byte;
function MsgDlgBtnToDefaultBtn(const ABtn: TMsgDlgBtn): TTaskDialogCommonButton;

//Global Initialization procedures (different versions)
procedure InitializeStyledTaskDialogs(AFont: TFont); overload;

procedure InitializeStyledTaskDialogs(AUseAnimations: Boolean); overload;

procedure InitializeStyledTaskDialogs(
  const ADialogButtonsFamily: TStyledButtonFamily = '';
  const AUseCommandLinks: Boolean = False;
  const AAutoClickDelay: Integer = -1); overload;

procedure InitializeStyledTaskDialogs(
  AUseTitleInMessageDlg: Boolean; AFont: TFont;
  const AUseCommandLinks: Boolean = False;
  const ADialogButtonsFamily: TStyledButtonFamily = '';
  const AAlphaBlendValue: Byte = DEFAULT_STYLEDDIALOG_ALPHABLEND;
  const AButtonsWidth: Integer = DEFAULT_STYLEDDIALOG_BUTTONSWIDTH;
  const AButtonsHeight: Integer = DEFAULT_STYLEDDIALOG_BUTTONSHEIGHT;
  const AAutoClickDelay: Integer = -1); overload;

implementation

uses
  System.TypInfo
  , System.Math
  , System.Types
  , System.Rtti
  , Vcl.Themes
  , Winapi.CommCtrl
  , System.WideStrUtils
  , Winapi.MultiMon
  , System.HelpIntfs
  , Vcl.Controls
  , Vcl.StdCtrls
  , Vcl.ExtCtrls
  , Vcl.Consts
  , System.RTLConsts
  , Winapi.ShellApi
  , Vcl.StyledCmpMessages
  , Vcl.StandardButtonStyles
  , Vcl.StyledCmpStrUtils
  , Vcl.StyledTaskDialogFormUnit
  , Vcl.StyledTaskDialogStdUnit
  ;

var
  _TaskDialogExecute: ITaskDialogLauncher;
  _DialogButtonsFamily: TStyledButtonFamily;
  _CustomIcons: TStyledDialogIcons;
  _UseAnimations: Boolean;
  _DialogFont: TFont;
  _UseCommandLinks: Boolean;
  _UseTitleInMessageDlg: boolean;
  _AlphaBlendValue: Byte;
  _ButtonsWidth: Integer;
  _ButtonsHeight: Integer;
  _AutoClickDelay: Integer;

  ButtonNames: array[TMsgDlgBtn] of string = (
    'Yes', 'No', 'OK', 'Cancel', 'Abort', 'Retry', 'Ignore', 'All', 'NoToAll',
    'YesToAll','Help','Close');
  ModalResults: array[TMsgDlgBtn] of Integer = (
    mrYes, mrNo, mrOk, mrCancel, mrAbort, mrRetry, mrIgnore, mrAll, mrNoToAll,
    mrYesToAll, 0, mrClose);

function GetDefaultButton(const Buttons: TMsgDlgButtons): TMsgDlgBtn;
begin
  if      mbYes       in Buttons then Result := TMsgDlgBtn.mbYes
  else if mbOK        in Buttons then Result := TMsgDlgBtn.mbOK
  else if mbNo        in Buttons then Result := TMsgDlgBtn.mbNo
  else if mbCancel    in Buttons then Result := TMsgDlgBtn.mbCancel
  else if mbAbort     in Buttons then Result := TMsgDlgBtn.mbAbort
  else if mbRetry     in Buttons then Result := TMsgDlgBtn.mbRetry
  else if mbIgnore    in Buttons then Result := TMsgDlgBtn.mbIgnore
  else if mbAll       in Buttons then Result := TMsgDlgBtn.mbAll
  else if mbNoToAll   in Buttons then Result := TMsgDlgBtn.mbNoToAll
  else if mbYesToAll  in Buttons then Result := TMsgDlgBtn.mbYesToAll
  else if mbHelp      in Buttons then Result := TMsgDlgBtn.mbHelp
  else if mbClose     in Buttons then Result := TMsgDlgBtn.mbClose
  else Result := TMsgDlgBtn.mbRetry;
end;

//Internal Function to Display a Styled TaskMessage Dialog
//using a TStyledTaskDialog component
function InternalDoTaskMessageDlgPosHelp(const Instruction, Msg: string;
  const DlgType: TMsgDlgType;
  Buttons: TMsgDlgButtons; HelpCtx: Longint;
  const X, Y: Integer;
  const HelpFileName: string;
  const DefaultButton: TMsgDlgBtn;
  const AutoClickDelay: Integer;
  const UseCommandLinks: Boolean;
  const UseAlternateTaskDlgFlags: Boolean;
  const AlternateTaskDlgFlags: TTaskDialogFlags;
  const CustomButtonCaptions: array of string): Integer;
const
  IconMap: array[TMsgDlgType] of TTaskDialogIcon = (tdiWarning, tdiError,
    tdiInformation, tdiQuestion, tdiShield);
  LModalResults: array[TMsgDlgBtn] of Integer = (mrYes, mrNo, mrOk, mrCancel,
    mrAbort, mrRetry, mrIgnore, mrAll, mrNoToAll, mrYesToAll, mrHelp, mrClose);
var
  DlgBtn: TMsgDlgBtn;
  LTaskDialog: TStyledTaskDialog;
  LTaskDialogButtonItem: TTaskDialogButtonItem;
  LCommandLinkPresent: Boolean;
  LIndex: Integer;
begin
  //At least OK Button
  if Buttons = [] then
    Buttons := [TMsgDlgBtn.mbOK];

  Application.ModalStarted;
  LTaskDialog := TStyledTaskDialog.Create(nil);
  try
    //Assign custom position to Dialog
    LTaskDialog.Position := TPoint.Create(X, Y);
    //Reset default CommonButtons
    LTaskDialog.CommonButtons := [];
    //To inform the Dialog Form to use the Default Button specified in MessageDlg
    LTaskDialog.FUseMessageDefaultButton := True;
    LTaskDialog.FMessageDefaultButton := DefaultButton;
    LTaskDialog.UseAnimations :=  _UseAnimations;
    LTaskDialog.AutoClick := AutoClickDelay > 0;
    if LTaskDialog.AutoClick then
      LTaskDialog.AutoClickDelay := AutoClickDelay;
    LTaskDialog.UseCommandLinks := UseCommandLinks;
    if UseAlternateTaskDlgFlags then
      LTaskDialog.Flags := AlternateTaskDlgFlags; //Replace the default Flags to AlternateTaskDlgFlags.
    // Assign buttons
    LIndex := -1;
    LCommandLinkPresent := False;
    for DlgBtn := Low(TMsgDlgBtn) to High(TMsgDlgBtn) do
    begin
      //Check for particular case using command links:
      if UseCommandLinks then
      begin
        //if mbYes and mbOK are both present with "UseCommandLinks", then tcbOk is used as CommonButton
        if (DlgBtn = TMsgDlgBtn.mbOK) and (DlgBtn in Buttons) and (TMsgDlgBtn.mbYes in Buttons) then
        begin
          LTaskDialog.CommonButtons := LTaskDialog.CommonButtons + [TTaskDialogCommonButton.tcbOk];
          Continue;
        end;
        //if mbNo and mbCancel are both present with "UseCommandLinks", then tcbCancel is used as CommonButton
        if (DlgBtn = TMsgDlgBtn.mbCancel) and (DlgBtn in Buttons) and (TMsgDlgBtn.mbNo in Buttons) then
        begin
          LTaskDialog.CommonButtons := LTaskDialog.CommonButtons + [TTaskDialogCommonButton.tcbCancel];
          Continue;
        end;
      end;

      if DlgBtn in Buttons then
      begin
        //if a Button is "common", add only if not using CommandLinks
        LTaskDialogButtonItem := LTaskDialog.Buttons.Add as TTaskDialogButtonItem;
        Inc(LIndex);
        if LIndex <= High(CustomButtonCaptions) then
          LTaskDialogButtonItem.Caption := CustomButtonCaptions[LIndex]
        else
        begin
          //use Captions defined in Vcl.StyledCmpMessages.pas
          case DlgBtn of
            mbYes: LTaskDialogButtonItem.Caption := STR_YES;
            mbNo: LTaskDialogButtonItem.Caption := STR_NO;
            mbOK: LTaskDialogButtonItem.Caption := STR_OK;
            mbCancel: LTaskDialogButtonItem.Caption := STR_CANCEL;
            mbAbort: LTaskDialogButtonItem.Caption := STR_ABORT;
            mbRetry: LTaskDialogButtonItem.Caption := STR_RETRY;
            mbIgnore: LTaskDialogButtonItem.Caption := STR_IGNORE;
            mbAll: LTaskDialogButtonItem.Caption := STR_ALL;
            mbNoToAll: LTaskDialogButtonItem.Caption := STR_NOTOALL;
            mbYesToAll: LTaskDialogButtonItem.Caption := STR_YESTOALL;
            mbHelp: LTaskDialogButtonItem.Caption := STR_HELP;
            mbClose: LTaskDialogButtonItem.Caption := STR_CLOSE;
          end;
        end;

        //Set CommandLinkHint based on DlgBtn when using a Warning or Confirmation
        if DlgType in [TMsgDlgType.mtWarning, TMsgDlgType.mtConfirmation] then
        case DlgBtn of
          mbOK: LTaskDialogButtonItem.CommandLinkHint := Format('%s %s', [STR_CONFIRM, STR_THE_OPERATION]);
          mbYes: LTaskDialogButtonItem.CommandLinkHint := Format('%s %s', [STR_CONFIRM, STR_THE_OPERATION]);
          mbNo: LTaskDialogButtonItem.CommandLinkHint := Format('%s %s', [STR_CANCEL, STR_THE_OPERATION]);
          mbCancel: LTaskDialogButtonItem.CommandLinkHint := Format('%s %s', [STR_CANCEL, STR_THE_OPERATION]);
          mbAbort: LTaskDialogButtonItem.CommandLinkHint := Format('%s %s', [STR_ABORT, STR_THE_OPERATION]);
          mbRetry: LTaskDialogButtonItem.CommandLinkHint := Format('%s %s', [STR_RETRY, STR_THE_OPERATION]);
          mbIgnore: LTaskDialogButtonItem.CommandLinkHint := Format('%s %s', [STR_IGNORE, STR_THE_OPERATION]);
          mbNoToAll: LTaskDialogButtonItem.CommandLinkHint := Format('%s %s', [STR_CANCEL, STR_THE_OPERATION]);
          mbYesToAll: LTaskDialogButtonItem.CommandLinkHint := Format('%s %s', [STR_CONFIRM, STR_THE_OPERATION]);
        end;

        if LTaskDialogButtonItem.CommandLinkHint <> '' then
          LCommandLinkPresent := UseCommandLinks;

        if DlgBtn = DefaultButton then
          LTaskDialogButtonItem.Default := True;
        LTaskDialogButtonItem.ModalResult := LModalResults[DlgBtn];
      end;
    end;

    //use Captions defined in Vcl.StyledCmpMessages.pas
    LTaskDialog.Caption := GetDialogTypeTitle(DlgType);
    if Not UseAlternateTaskDlgFlags and Application.UseRightToLeftReading then
      LTaskDialog.Flags := LTaskDialog.Flags + [tfRtlLayout];
    if pos('<A HREF=',Msg) > 0 then
      LTaskDialog.Flags := LTaskDialog.Flags + [tfEnableHyperlinks];

    if LTaskDialog.UseCommandLinks and LCommandLinkPresent then
      LTaskDialog.Flags := LTaskDialog.Flags + [tfUseCommandLinks];

    LTaskDialog.HelpContext := HelpCtx;
    LTaskDialog.HelpFile := HelpFileName;
    LTaskDialog.MainIcon := IconMap[DlgType];
    if (X <> -1) and (Y <> -1) then
      LTaskDialog.Position := Point(X, Y);
    LTaskDialog.Text := Msg;

    if (Instruction = '') and (LTaskDialog.UseTitleInMessageDlg) then
      LTaskDialog.Title := LTaskDialog.Caption
    else
      LTaskDialog.Title := Instruction;

    // Show dialog and return result
    Result := mrNone;
    if LTaskDialog.Execute then
      Result := LTaskDialog.ModalResult;
  finally
    LTaskDialog.Free;
    Application.ModalFinished;
  end;
end;

function DoStyledTaskMessageDlg(const Instruction, Msg: string; DlgType: TMsgDlgType;
  Buttons: TMsgDlgButtons; HelpCtx: Longint = 0;
  const AutoClickDelay: Integer = -1; const UseCommandLinks: Boolean = False): Integer; overload;
begin
  Result := InternalDoTaskMessageDlgPosHelp(Instruction, Msg, DlgType,
    Buttons, HelpCtx, -1, -1, '', GetDefaultButton(Buttons),
    AutoClickDelay, UseCommandLinks, False, [], []);
end;

function DoStyledTaskMessageDlgPos(const Instruction, Msg: string; DlgType: TMsgDlgType;
  Buttons: TMsgDlgButtons; HelpCtx: Longint; X: Integer; Y: Integer;
  const AutoClickDelay: Integer = -1; const UseCommandLinks: Boolean = False): Integer; overload;
begin
  Result := InternalDoTaskMessageDlgPosHelp(Instruction, Msg, DlgType,
    Buttons, HelpCtx, X, Y, '', GetDefaultButton(Buttons),
    AutoClickDelay, UseCommandLinks, False, [], []);
end;

function DoStyledTaskMessageDlgPosHelp(const Instruction, Msg: string; DlgType: TMsgDlgType;
  Buttons: TMsgDlgButtons; HelpCtx: Longint; X: Integer = -1; Y: Integer = -1;
  const AutoClickDelay: Integer = -1;  const UseCommandLinks: Boolean = False;
  const HelpFileName: string = ''): Integer; overload;
begin
  Result := InternalDoTaskMessageDlgPosHelp(Instruction, Msg, DlgType,
    Buttons, HelpCtx, X, Y, HelpFileName, GetDefaultButton(Buttons),
    AutoClickDelay, UseCommandLinks, False, [], []);
end;

function DoStyledTaskMessageDlgPosHelp(const Instruction, Msg: string; DlgType: TMsgDlgType;
  Buttons: TMsgDlgButtons; HelpCtx: Longint; DefaultButton: TMsgDlgBtn;
  X: Integer = -1; Y: Integer = -1;
  const AutoClickDelay: Integer = -1; const UseCommandLinks: Boolean = False;
  const HelpFileName: string = ''): Integer; overload;
begin
  Result := InternalDoTaskMessageDlgPosHelp(Instruction, Msg, DlgType,
    Buttons, HelpCtx, X, Y, HelpFileName, DefaultButton,
    AutoClickDelay, UseCommandLinks, False, [], []);
end;

function DoMessageDlgPosHelp(const Msg: string; DlgType: TMsgDlgType;
  Buttons: TMsgDlgButtons; HelpCtx: Integer; DefaultButton: TMsgDlgBtn;
  X: Integer = -1; Y: Integer = -1;
  const AutoClickDelay: Integer = -1; const UseCommandLinks: Boolean = False;
  const HelpFileName: string = ''): Integer; overload;
begin
  Result := InternalDoTaskMessageDlgPosHelp('', Msg, DlgType,
    Buttons, HelpCtx, X, Y, HelpFileName, DefaultButton,
    AutoClickDelay, UseCommandLinks, False, [], []);
end;

function DoMessageDlgPosHelp(const Msg: string; DlgType: TMsgDlgType;
  Buttons: TMsgDlgButtons; HelpCtx: Longint;
  X: Integer = -1; Y: Integer = -1;
  const AutoClickDelay: Integer = -1; const UseCommandLinks: Boolean = False;
  const HelpFileName: string = ''): Integer; overload;
begin
  Result := InternalDoTaskMessageDlgPosHelp('', Msg, DlgType,
    Buttons, HelpCtx, X, Y, HelpFileName, GetDefaultButton(Buttons),
    AutoClickDelay, UseCommandLinks, False, [], []);
end;

function GetDialogAlphaBlendValue: Byte;
begin
  Result := _AlphaBlendValue;
end;

function GetDialogButtonsWidth: Integer;
begin
  Result := _ButtonsWidth;
end;

function GetDialogFont: TFont;
begin
  Result := _DialogFont;
end;

function GetDialogBtnFamily: TStyledButtonFamily;
begin
  Result := _DialogButtonsFamily;
end;

function StyledMessageDlg(const Msg: string; DlgType: TMsgDlgType;
  Buttons: TMsgDlgButtons; HelpCtx: Longint): Integer; overload;
begin
  Result := DoStyledTaskMessageDlgPosHelp('', Msg, DlgType, Buttons, HelpCtx,
    -1, -1, _AutoClickDelay, _UseCommandLinks);
end;

function StyledMessageDlg(const Title, Msg: string; DlgType: TMsgDlgType;
  Buttons: TMsgDlgButtons; HelpCtx: Longint): Integer;
begin
  Result := DoStyledTaskMessageDlgPosHelp(Title, Msg, DlgType, Buttons, HelpCtx);
end;

function StyledMessageDlg(const Title, Msg: string; DlgType: TMsgDlgType;
  Buttons: TMsgDlgButtons; HelpCtx: Longint; DefaultButton: TMsgDlgBtn): Integer; overload;
begin
  Result := DoStyledTaskMessageDlgPosHelp(Title, Msg, DlgType, Buttons, HelpCtx, DefaultButton);
end;

function StyledMessageDlgPos(const Msg: string; DlgType: TMsgDlgType;
  Buttons: TMsgDlgButtons; DefaultButton: TMsgDlgBtn; HelpCtx: Longint;
  X: Integer; Y: Integer): Integer; overload;
begin
  Result := DoStyledTaskMessageDlgPosHelp('', Msg, DlgType, Buttons, HelpCtx,
    DefaultButton, X, Y);
end;

function StyledMessageDlg(const Msg: string; DlgType: TMsgDlgType;
  Buttons: TMsgDlgButtons; HelpCtx: Longint; DefaultButton: TMsgDlgBtn): Integer; overload;
begin
  Result := DoStyledTaskMessageDlgPosHelp('', Msg, DlgType, Buttons, HelpCtx, DefaultButton);
end;

function StyledMessageDlg(const Msg: string; DlgType: TMsgDlgType;
  Buttons: TMsgDlgButtons; HelpCtx: Longint; DefaultButton: TMsgDlgBtn;
  CustomButtonCaptions: array of string): Integer; overload;
begin
  Result := InternalDoTaskMessageDlgPosHelp('', Msg, DlgType, Buttons, HelpCtx,
    -1, -1, '', DefaultButton, -1,
    False, False, [], CustomButtonCaptions);
end;

function StyledMessageDlgPos(const Msg: string; DlgType: TMsgDlgType;
  Buttons: TMsgDlgButtons; HelpCtx: Longint; X: Integer; Y: Integer): Integer;
begin
  Result := DoStyledTaskMessageDlgPosHelp('', Msg, DlgType, Buttons, HelpCtx, X, Y);
end;

function StyledMessageDlgPos(const Msg: string; DlgType: TMsgDlgType;
  Buttons: TMsgDlgButtons; HelpCtx: Longint; DefaultButton: TMsgDlgBtn;
  X: Integer; Y: Integer): Integer;
begin
  Result := DoStyledTaskMessageDlgPosHelp('', Msg, DlgType, Buttons,
    HelpCtx, DefaultButton, X, Y, -1, _UseCommandLinks, '');
end;

procedure InitializeStyledTaskDialogs(AUseAnimations: Boolean);
begin
  _UseAnimations  := AUseAnimations;
end;

procedure InitializeStyledTaskDialogs(AFont: TFont);
begin
  if Assigned(AFont) then
  begin
    if not Assigned(_DialogFont) then
      _DialogFont := TFont.Create;
    _DialogFont.Assign(AFont);
  end
  else
    FreeAndNil(_DialogFont);
end;

procedure InitializeStyledTaskDialogs(
  const ADialogButtonsFamily: TStyledButtonFamily = '';
  const AUseCommandLinks: Boolean = False;
  const AAutoClickDelay: Integer = -1); overload;
begin
  InitializeStyledTaskDialogs(True, Screen.MessageFont,
    AUseCommandLinks, ADialogButtonsFamily,
    DEFAULT_STYLEDDIALOG_ALPHABLEND,
    DEFAULT_STYLEDDIALOG_BUTTONSWIDTH,
    DEFAULT_STYLEDDIALOG_BUTTONSHEIGHT,
    AAutoClickDelay);
end;

procedure InitializeStyledTaskDialogs(AUseTitleInMessageDlg: Boolean; AFont: TFont;
  const AUseCommandLinks: Boolean = False;
  const ADialogButtonsFamily: TStyledButtonFamily = '';
  const AAlphaBlendValue: Byte = DEFAULT_STYLEDDIALOG_ALPHABLEND;
  const AButtonsWidth: Integer = DEFAULT_STYLEDDIALOG_BUTTONSWIDTH;
  const AButtonsHeight: Integer = DEFAULT_STYLEDDIALOG_BUTTONSHEIGHT;
  const AAutoClickDelay: Integer = -1);
begin
  CheckValue('TStyledTaskDialog.ButtonsHeight', AButtonsHeight,
    MIN_STYLEDDIALOG_BUTTONSSIZE, MAX_STYLEDDIALOG_BUTTONSHEIGHT);
  CheckValue('TStyledTaskDialog.SetButtonsWidth', AButtonsWidth,
    MIN_STYLEDDIALOG_BUTTONSSIZE, MAX_STYLEDDIALOG_BUTTONSWIDTH);
  if Assigned(AFont) then
  begin
    if not Assigned(_DialogFont) then
      _DialogFont := TFont.Create;
    _DialogFont.Assign(AFont);
  end
  else
    FreeAndNil(_DialogFont);
  _UseCommandLinks := AUseCommandLinks;
  _UseTitleInMessageDlg := AUseTitleInMessageDlg;
  _DialogButtonsFamily := ADialogButtonsFamily;
  _AlphaBlendValue := AAlphaBlendValue;
  _ButtonsWidth := AButtonsWidth;
  _ButtonsHeight := AButtonsHeight;
  _AutoClickDelay := AAutoClickDelay;
end;

procedure UnregisterCustomIcons;
begin
  _CustomIcons[mtWarning] := nil;
  _CustomIcons[mtError] := nil;
  _CustomIcons[mtInformation] := nil;
  _CustomIcons[mtConfirmation] := nil;
  _CustomIcons[mtCustom] := nil;
end;

procedure RegisterCustomIcons(const ACustomIcons: TStyledDialogIcons);
begin
  UnregisterCustomIcons;
  _CustomIcons := ACustomIcons;
end;

procedure SetUseTitleInMessageDlg(AValue: boolean);
begin
  _UseTitleInMessageDlg := AValue;
end;

procedure RegisterCustomExecute(const AShowStyledTaskDialog: ITaskDialogLauncher;
  const AButtonFamily: TStyledButtonFamily = '');
begin
  _TaskDialogExecute := AShowStyledTaskDialog;
end;

procedure UnRegisterCustomExecute;
begin
  _TaskDialogExecute := nil;
end;

function GetTaskDlgType(
  const AIcon: TTaskDialogIcon): TMsgDlgType;
begin
  if AIcon = tdiNone then
    Result := TMsgDlgType.mtCustom
  else if AIcon = tdiWarning then
    Result := TMsgDlgType.mtWarning
  else if AIcon = tdiError then
    Result := TMsgDlgType.mtError
  else if AIcon = tdiInformation then
    Result := TMsgDlgType.mtInformation
  else if AIcon = tdiQuestion then
    Result := TMsgDlgType.mtConfirmation
  else if AIcon = tdiShield then
    Result := TMsgDlgType.mtCustom
  else
    Result := TMsgDlgType.mtInformation;
end;

function StyledTaskMessageDlg(const Title, Msg: string; DlgType: TMsgDlgType;
  Buttons: TMsgDlgButtons; HelpCtx: Longint): Integer; overload;
begin
  Result := DoStyledTaskMessageDlgPosHelp(Title, Msg, DlgType, Buttons, HelpCtx,
    -1, -1, _AutoClickDelay, _UseCommandLinks);
end;

function StyledTaskMessageDlg(const Title, Msg: string; DlgType: TMsgDlgType;
  Buttons: TMsgDlgButtons; HelpCtx: Longint; DefaultButton: TMsgDlgBtn): Integer; overload;
begin
  Result := DoStyledTaskMessageDlgPosHelp(Title, Msg, DlgType, Buttons, HelpCtx, DefaultButton,
    -1, -1, _AutoClickDelay, _UseCommandLinks, '');
end;

function StyledTaskDlgPos(const Title, Msg: string; DlgType: TMsgDlgType;
  Buttons: TMsgDlgButtons; HelpCtx: Longint; X: Integer = -1; Y: Integer = -1): Integer;
begin
  Result := DoStyledTaskMessageDlgPosHelp(Title, Msg, DlgType, Buttons, HelpCtx, X, Y,
    _AutoClickDelay, _UseCommandLinks, '');
end;

function StyledTaskMessageDlgPos(const Title, Msg: string; DlgType: TMsgDlgType;
  Buttons: TMsgDlgButtons; HelpCtx: Longint; X: Integer; Y: Integer): Integer;
begin
  Result := DoStyledTaskMessageDlgPosHelp(Title, Msg, DlgType, Buttons, HelpCtx, X, Y,
    _AutoClickDelay, _UseCommandLinks, '');
end;

function StyledTaskDlgPos(const Title, Msg: string; DlgType: TMsgDlgType;
  Buttons: TMsgDlgButtons; DefaultButton: TMsgDlgBtn; HelpCtx: Longint;
  X: Integer = -1; Y: Integer = -1): Integer;
begin
  Result := StyledTaskMessageDlgPos(Title, Msg, DlgType, Buttons, HelpCtx, DefaultButton, X, Y);
end;

function StyledTaskMessageDlgPos(const Title, Msg: string; DlgType: TMsgDlgType;
  Buttons: TMsgDlgButtons; HelpCtx: Longint; DefaultButton: TMsgDlgBtn;
  X: Integer; Y: Integer): Integer;
var
  MsgWithTitle: string;
begin
  if Title <> '' then
    MsgWithTitle := UpperCase(Title)+sLineBreak+Msg
  else
    MsgWithTitle := Msg;
    Result := StyledMessageDlgPos(MsgWithTitle, DlgType, Buttons, HelpCtx, DefaultButton, -1, -1);
end;

function StyledTaskMessageDlgPosHelp(const Title, Msg: string; DlgType: TMsgDlgType;
  Buttons: TMsgDlgButtons; HelpCtx: Longint; X, Y: Integer;
  const HelpFileName: string; DefaultButton: TMsgDlgBtn; CustomButtonCaptions: array of string): Integer;
begin
  Result := InternalDoTaskMessageDlgPosHelp(Title, Msg, DlgType, Buttons, HelpCtx,
    X, Y, HelpFileName, DefaultButton, -1, _UseCommandLinks, False, [], []);
end;

procedure StyledShowMessage(const Msg: string); overload;
begin
  StyledMessageDlg(Msg, TMsgDlgType.mtInformation, [TMsgDlgBtn.mbOK], 0);
end;

procedure StyledShowMessageFmt(const Msg: string; Params: array of const); overload;
begin
  StyledMessageDlg(Format(Msg, Params), TMsgDlgType.mtInformation, [TMsgDlgBtn.mbOK], 0);
end;

{ TStyledTaskDialog }

constructor TStyledTaskDialog.Create(AOwner: TComponent);
begin
  inherited;
  //Default Position: -1, -1 (is not a custom position)
  FPosition := TPoint.Create(-1,-1);
  FDialogButtonsFamily := _DialogButtonsFamily;
  UseCommandLinks := _UseCommandLinks;
  FUseTitleInMessageDlg := _UseTitleInMessageDlg;
  AlphaBlendValue := _AlphaBlendValue;
  ButtonsWidth := _ButtonsWidth;
  ButtonsHeight := _ButtonsHeight;
  FAutoClick := _AutoClickDelay > 0;
  if FAutoClick then
    FAutoClickDelay := _AutoClickDelay
  else
    FAutoClickDelay := DEFAULT_AUTOCLICK_DELAY;
  FMainIconSize := DEFAULT_MAIN_ICON_SIZE;
  DoOnDialogCreated;
end;

function TStyledTaskDialog.DoExecute(ParentWnd: HWND): Boolean;
var
  LTaskDlgType: TMsgDlgType;
begin
  LTaskDlgType := GetTaskDlgType(MainIcon);
  //Use a custom interface for StyledTaskDialog, if registered
  if Assigned(_TaskDialogExecute) then
    Result := _TaskDialogExecute.DoExecute(ParentWnd,
      LTaskDlgType, Self, Self.FDialogButtonsFamily)
  else
    Result := inherited DoExecute(ParentWnd);
end;

procedure TStyledTaskDialog.DoOnExpandButtonClicked(Expanded: Boolean);
begin
  inherited DoOnExpandButtonClicked(not Expanded);
end;

procedure TStyledTaskDialog.DoOnHelp;
var
  LHelpFile: string;
  LHelpSystem: IHelpSystem;
begin
  if HelpContext <> 0 then
  begin
    if FHelpFile = '' then
      LHelpFile := Application.HelpFile
    else
      LHelpFile := HelpFile;
    if System.HelpIntfs.GetHelpSystem(LHelpSystem) then
    try
      LHelpSystem.Hook(Application.Handle, LHelpFile, HELP_CONTEXT, HelpContext);
    except
      on E: Exception do
        ShowHelpException(E);
    end;
  end;
end;

procedure TStyledTaskDialog.DoOnHyperlinkClicked(const AURL: string);
begin
  inherited DoOnHyperlinkClicked(AURL);
end;

procedure TStyledTaskDialog.DoOnRadioButtonClicked(ButtonID: Integer);
begin
  inherited DoOnRadioButtonClicked(ButtonID);
end;

function TStyledTaskDialog.Execute(ParentWnd: HWND): Boolean;
begin
  FParentWnd := ParentWnd;
  Result := inherited Execute(ParentWnd);
end;

function TStyledTaskDialog.FindDialogButton(
  const AModalResult: TModalResult): TStyledButton;
begin
  if Assigned(FOnFindDialogButton) then
    Result := FOnFindDialogButton(AModalResult)
  else
    Result := nil;
end;

function TStyledTaskDialog.GetHandle: HWND;
begin
  Result := inherited Handle;
end;

function TStyledTaskDialog.GetUseCommandLinks: Boolean;
begin
  Result := TTaskDialogFlag.tfUseCommandLinks in Flags;
end;

function TStyledTaskDialog.IsCustomPosition: Boolean;
begin
  Result := (FPosition.X <> -1) and (FPosition.Y <> -1);
end;

function TStyledTaskDialog.IsDefaultFamily: Boolean;
begin
  Result := FDialogButtonsFamily <> DEFAULT_CLASSIC_FAMILY;
end;

procedure TStyledTaskDialog.SetAlphaBlendValue(const AValue: Byte);
begin
  FAlphaBlendValue := AValue;
end;

procedure TStyledTaskDialog.SetUseAnimationInverse(const AValue: Boolean);
begin
  if FUseAnimationInverse <> AValue then
  begin
    if AValue then
      UseAnimations := True;
    FUseAnimationInverse := AValue;
  end;
end;

procedure TStyledTaskDialog.SetUseAnimationLoop(const AValue: Boolean);
begin
  if FUseAnimationLoop <> AValue then
  begin
    if AValue then
      UseAnimations := True;
    FUseAnimationLoop := AValue;
  end;
end;

procedure TStyledTaskDialog.SetAutoClick(const AValue: Boolean);
begin
  FAutoClick := AValue;
end;

procedure TStyledTaskDialog.SetAutoClickDelay(const AValue: Integer);
begin
  FAutoClickDelay := AValue;
end;

procedure TStyledTaskDialog.SetButtonsHeight(const AValue: Integer);
begin
  CheckValue('TStyledTaskDialog.ButtonsHeight', AValue,
    MIN_STYLEDDIALOG_BUTTONSSIZE, MAX_STYLEDDIALOG_BUTTONSHEIGHT);
  FButtonsHeight := AValue;
end;

procedure TStyledTaskDialog.SetButtonsWidth(const AValue: Integer);
begin
  CheckValue('TStyledTaskDialog.SetButtonsWidth', AValue,
    MIN_STYLEDDIALOG_BUTTONSSIZE, MAX_STYLEDDIALOG_BUTTONSWIDTH);
  FButtonsWidth := AValue;
end;

procedure TStyledTaskDialog.SetHandle(const AValue: HWND);
var
  Context: TRttiContext;
  RttiType: TRttiType;
  Field: TRttiField;
begin
  Context := TRttiContext.Create;
  try
    RttiType := Context.GetType(Self.ClassType);
    Field := RttiType.GetField('FHandle');
    if Assigned(Field) then
      Field.SetValue(Self, AValue);
  finally
    Context.Free;
  end;
end;

procedure TStyledTaskDialog.SetMainIconSize(const AValue: Integer);
begin
  if AValue < 0 then
    raise EArgumentOutOfRangeException.CreateRes(@SArgumentOutOfRange);
  FMainIconSize := AValue;
end;

procedure TStyledTaskDialog.SetUseAnimations(const AValue: Boolean);
begin
  if FUseAnimations <> AValue then
  begin
    if AValue and not AnimatedTaskDialogFormRegistered then
      raise EStyledTaskDialogException.CreateFmt(
        ERR_DIALOG_FORM_NOT_REGISTERED,
        ['Skia.Vcl.StyledTaskDialogAnimatedUnit.pas']);
    FUseAnimations := AValue;
  end;
end;

procedure TStyledTaskDialog.SetUseCommandLinks(const AValue: Boolean);
begin
  if AValue then
    Flags :=  Flags + [TTaskDialogFlag.tfUseCommandLinks]
  else
    Flags :=  Flags - [TTaskDialogFlag.tfUseCommandLinks];
end;

function GetDialogTypeTitle(const DlgType: TMsgDlgType): string;
begin
  case DlgType of
    mtWarning      : Result := STR_WARNING;
    mtError        : Result := STR_ERROR;
    mtInformation  : Result := STR_INFORMATION;
    mtConfirmation : Result := STR_CONFIRM;
    mtCustom       : Result := STR_INFORMATION;
  end;
end;

function MsgDlgBtnToDefaultBtn(const ABtn: TMsgDlgBtn): TTaskDialogCommonButton;
begin
  case ABtn of
    mbYes: Result := tcbYes;
    mbNo: Result := tcbNo;
    mbOK: Result := tcbOk;
    mbCancel: Result := tcbCancel;
    mbRetry: Result := tcbRetry;
    mbClose: Result := tcbClose;
  else
    Result := tcbOk;
  end;
end;

initialization
  _DialogButtonsFamily := DEFAULT_CLASSIC_FAMILY;
  _UseCommandLinks := False;
  _UseTitleInMessageDlg := True;
  _AlphaBlendValue := DEFAULT_STYLEDDIALOG_ALPHABLEND;
  _ButtonsWidth := DEFAULT_STYLEDDIALOG_BUTTONSWIDTH;
  _ButtonsHeight := DEFAULT_STYLEDDIALOG_BUTTONSHEIGHT;
  _DialogFont := nil;
  RegisterTaskDialogFormClass(TStyledTaskDialogStd);

finalization
  if Assigned(_DialogFont) then
    _DialogFont.Free;
  UnRegisterTaskDialogFormClass(TStyledTaskDialogStd);

end.
