{******************************************************************************}
{                                                                              }
{  StyledComponentsHooks: an interposer Unit to use Styled Components          }
{  using Standard Delphi Controls Class Names                                  }
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
unit Vcl.StyledComponentsHooks;

interface

{$INCLUDE StyledComponents.inc}

uses
  Vcl.StyledButton
  , Vcl.StyledDbNavigator
  , Vcl.StyledToolbar
  , Vcl.StyledButtonGroup
  , Vcl.StyledCategoryButtons
  ;

type
  //Interposer Class for TButton -> TStyledButton
  TButton = class(TStyledButton) end;

  //Interposer Class for TBitBtn -> TStyledBitBtn
  TBitBtn = class(TStyledBitBtn) end;

  //Interposer Class for TBitBtn -> TStyledSpeedButton
  TSpeedButton = class(TStyledSpeedButton) end;

  //Interposer Class for TDbNavigator -> TStyledDbNavigator
  TDbNavigator = class(TStyledDbNavigator) end;

  //Interposer Class for TBindNavigator -> TStyledBindNavigator
  TBindNavigator = class(TStyledBindNavigator) end;

  //Interposer Class for TToolbar -> TStyledToolbar
  TToolbar = class(TStyledToolbar) end;

  //Interposer Class for TToolbutton -> TStyledToolButton
  TToolbutton = class(TStyledToolbutton) end;

  //Interposer Class for TButtonGroup -> TStyledButtonGroup
  TButtonGroup = class(TStyledButtonGroup) end;

  //Interposer Class for TCategoryButtons -> TStyledCategoryButtons
  TCategoryButtons = class(TStyledCategoryButtons) end;

  //Interposer Class for TButtonCategory -> TStyledButtonCategory
  TButtonCategory = class(TStyledButtonCategory) end;

implementation

end.
