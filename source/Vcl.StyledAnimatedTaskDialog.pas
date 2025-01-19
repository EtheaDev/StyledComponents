{******************************************************************************}
{                                                                              }
{  StyledAnimatedTaskDialog: a Task Dialog Component with StyleButtons         }
{  and animations using Skia4Delphi                                            }
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
unit Vcl.StyledAnimatedTaskDialog;

{$INCLUDE StyledComponents.inc}

interface

uses
  System.SysUtils
  , System.Classes
  , WinApi.Windows
  , Vcl.StyledTaskDialog
  ;

//{$WARN SYMBOL_PLATFORM OFF}
type
  TStyledAnimatedTaskDialog = class(TStyledTaskDialog)
  private
  strict protected
  public
  end;

implementation

uses
  Skia.Vcl.StyledTaskDialogAnimatedUnit //to register StyledTaskDialogAnimatedUnit
  ;

initialization

finalization

end.
