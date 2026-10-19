# Guida: review del codice e suite di test automatici

Questa guida serve ad aprire una sessione di Claude Code in `D:\ETHEA\StyledComponents` e a ripetere il lavoro fatto su SVGIconImageList (versione 4.8.0): review completa del codice, suite di test DUnitX, correzione delle anomalie con test che falliscono prima e passano dopo, commit a blocchi su SVN, aggiornamento di versione e documentazione.

Riferimenti da consultare durante il lavoro:
- `D:\ETHEA\SvgIconImageList\Test` — la suite costruita per SVGIconImageList (struttura, `run_tests.cmd`, utility di test, pattern per i test di rendering)
- `D:\ETHEA\KittoX\Test` — il modello originale di struttura e di `run_tests.cmd`

Prompt suggerito per iniziare la sessione:

> Leggi REVIEW_AND_TEST_GUIDE.md e procedi con la fase 1 (review). Poi fermati e mostrami il report.

---

## Fase 0 — Preparazione

1. `svn status` e `svn info`: annotare la revisione di partenza (serve per il "red run", vedi Fase 4) e le modifiche locali già presenti. **Le modifiche locali non fatte da Claude non vanno mai nei commit.**
2. Leggere `CLAUDE.md` del progetto e `source\StyledComponents.inc` (define di versione e di feature).
3. Verificare che la cartella `Test` non esista già in SVN con file mancanti (su SVGIconImageList la vecchia `Test` era stata spostata a mano in `ManualTest`, non versionata). Qui `ManualTest` esiste già: la nuova suite va in `Test`.
4. Anche se il progetto è sotto SVN, copiare `source` nello scratchpad prima di modificare: serve per il red run e per produrre un diff unico.

## Fase 1 — Review

### Come dividerla

Il codice di `source` è di circa 27.000 righe. Conviene lanciare **agenti in parallelo** (tipo `general-purpose`, sola lettura), un'area ciascuno, con la richiesta di finding verificati sul codice, con `file:riga`, gravità, scenario concreto di errore e fix suggerito. Divisione proposta:

| Agente | File |
| - | - |
| 1. Motore di rendering | `Vcl.ButtonStylesAttributes.pas` (TStyledButtonRender, TStyledButtonAttributes, IStyledButtonAttributes), `Vcl.StyledGraphicsUtils.pas`, `Vcl.StyledCmpStrUtils.pas` |
| 2. Pulsanti | `Vcl.StyledButton.pas` (6.800 righe: TStyledGraphicButton, TStyledButton, TStyledSpeedButton, TStyledBitBtn) |
| 3. Contenitori | `Vcl.StyledToolbar.pas`, `Vcl.StyledDbNavigator.pas`, `Vcl.StyledButtonGroup.pas`, `Vcl.StyledCategoryButtons.pas`, `Vcl.StyledPanel.pas` |
| 4. Famiglie di stile | `Vcl.StandardButtonStyles.pas`, `Vcl.BootstrapButtonStyles.pas`, `Vcl.AngularButtonStyles.pas`, `Vcl.ColorButtonStyles.pas`, `Vcl.TemplateButtonStyles.pas` |
| 5. Dialoghi, hook, animazioni | `Vcl.StyledTaskDialog.pas`, `Vcl.StyledTaskDialogFormUnit.pas`, `Vcl.StyledTaskDialogStdUnit.pas`, `Vcl.StyledMessagesHooks.pas`, `Vcl.StyledComponentsHooks.pas`, `Vcl.StyledCmpMessages.pas`, `Vcl.StyledAnimated*.pas`, `Skia.Vcl.StyledTaskDialogAnimatedUnit.pas`, `Vcl.SkAnimatedImageHelper.pas` |

Cosa far cercare, oltre ai soliti leak, try/finally mancanti, use-after-free e off-by-one:
- **Streaming DFM**: `default`/`stored` coerenti con i valori del costruttore; proprietà che cambiano valore in base ad altre (StyleFamily → StyleClass → StyleAppearance) e l'ordine di lettura dal DFM (`Loaded`, `csLoading`); valori "ereditati" che finiscono salvati come propri (è stato un bug reale in SVGIconImageList FMX).
- **Propagazione delle modifiche**: setter che non invalidano o non ridisegnano; `RegisterDefaultRenderingStyle` che non si applica a componenti già creati.
- **Stati**: Normal/Pressed/Selected/Hot/Disabled, e `Enabled` che cambia mentre il mouse è sopra.
- **DPI**: `GetOwnerScaleFactor`, `ChangeScale`, doppio scaling, font.
- **Risorse GDI/GDI+**: pen, brush, bitmap, handle non rilasciati nei cicli di paint.
- **Hook e interposer**: i routine sostituiti hanno la stessa semantica degli originali (valori di ritorno, `DefaultButton`, `HelpCtx`).
- **TaskDialog**: mappatura dei pulsanti e dei ModalResult, pulsanti custom, radio button, verifica, timeout.
- **Condizionali**: `{$IFDEF}` su versioni e feature (`INCLUDE_*ButtonStyles`, `Use_Large_Dialog_Icons`, SKIA).

### Verificare prima di fidarsi

I report degli agenti **non vanno riportati così come sono**. Su SVGIconImageList due finding "gravi" erano falsi:
- `{$IFDEF D10_3}` ritenuto codice morto perché il file .inc definisce `D10_3+`: in realtà **Delphi ignora il `+` finale** nei simboli condizionali, quindi `D10_3+` e `D10_3` sono lo stesso simbolo. Vale anche qui (`DXE6+`, `D10_0+`, ...).
- Un presunto alone nell'export PNG che con il motore di default non si verificava.

Il modo più efficace di verificare è scrivere il test: se non fallisce sul codice originale, il finding era sbagliato (il test resta comunque come guardia).

Report all'utente: tabella per gravità, con i finding verificati di persona marcati, e con l'elenco di ciò che non è verificabile (per esempio rami per versioni di Delphi non installate).

## Fase 2 — Struttura della suite

Copiare la struttura di `D:\ETHEA\SvgIconImageList\Test`:

```
Test\
  run_tests.cmd                     build + esecuzione, exit code 0 = tutto verde
  Source\
    StyledTestUtils.pas             utility: bitmap, pixel probe, form ospite, stream
    StyledRenderTests.pas           TStyledButtonRender / attributi / famiglie
    StyledButtonTests.pas           pulsanti
    StyledContainerTests.pas        toolbar, navigator, button group, category buttons, panel
    StyledDialogTests.pas           task dialog e hook (senza mostrare dialoghi modali)
  Projects\D13\
    StyledComponentsTests.dpr/.dproj
    (StyledAnimatedTests.dpr/.dproj, se si testano i componenti Skia)
  Bin\, Lib\                        output (svn:ignore)
```

Punti chiave, già risolti in SVGIconImageList (copiare da lì):
- **`.dpr`**: console, `{$STRONGLINKTYPES ON}`, logger console più NUnit XML, `FailsOnNoAsserts := False`.
- **`.dproj`**: `FrameworkType` VCL, `DCC_UnitSearchPath` verso `..\..\Source` e `..\..\..\source`, namespace `Winapi;System.Win;Vcl;Vcl.Imaging;...`, **range check e overflow check attivi in Debug**, `ProjectVersion` 20.5 (Delphi 13.2).
- **Cartella DCU separata per ogni progetto** (`Lib\D13\$(Platform)\$(Config)\<NomeProgetto>`): se due progetti compilano le stesse unit con define diversi, una cartella condivisa li mescola.
- **`run_tests.cmd`**: trova la versione di Delphi più recente dal registro, chiama `rsvars.bat`, usa una variabile `...PLATFORM` diversa da `PLATFORM` (rsvars la azzera), copia le DLL necessarie accanto all'exe (`sk4d.dll` da `bin`/`bin64` se si testa Skia).
- **`--xmlfile:` sempre con percorso completo**: DUnitX chiama `ForceDirectories` sulla cartella, e con un nome file senza cartella solleva `EInOutError`.
- Un attributo per i test parametrici: in SVGIconImageList `[AllEngines]` (una sottoclasse di `CustomTestCaseSourceAttribute`) fa girare lo stesso test su ogni motore. Qui l'equivalente naturale è un `[AllFamilies]` che produce un caso per ogni StyleFamily registrata.

## Fase 3 — Come scrivere i test

### Rendering: pixel probe
Il rendering si verifica disegnando su una `TBitmap` pf32bit con uno sfondo noto e leggendo i pixel (`ScanLine`), con una tolleranza per l'antialiasing. Per esempio: pulsante Bootstrap Primary → pixel al centro ≈ colore Primary; stato Disabled → colore dell'attributo Disabled; `StyleDrawType` rounded/rect → pixel nell'angolo di sfondo o di bordo.

Controlli senza finestra:
- **TGraphicControl** (TStyledGraphicButton, TStyledSpeedButton): `Control.Perform(WM_PAINT, WPARAM(Bitmap.Canvas.Handle), 0)` disegna sul DC indicato, anche senza parent.
- **TWinControl** (TStyledButton, TStyledPanel, toolbar...): serve un parent. Si crea `TForm.CreateNew(nil)` non visibile, si assegna il parent e si usa `PaintTo(Bitmap.Canvas.Handle, 0, 0)`.
- Meglio ancora, dove possibile, chiamare direttamente `TStyledButtonRender.DrawButton` su un canvas.

### Streaming
Per verificare che una proprietà published venga salvata e riletta: `Stream.WriteComponent(C)`, poi `Stream.ReadComponent(Copia)` su un'istanza nuova (con l'istanza passata non serve `RegisterClass`), e infine si confrontano le proprietà. È il test che scopre `default`/`stored` sbagliati e valori ereditati salvati come propri.

### Famiglie di stile
Per ogni famiglia registrata, per ogni classe e appearance: gli attributi dei 5 stati non sono nil, i colori sono coerenti (per esempio Disabled diverso da Normal dove previsto), `GetStyleByModalResult` è coerente con la mappatura documentata (mrOk/mrYes → Success/Primary, mrNo/mrAbort → Danger, mrCancel → Warning, mrHelp → Info).

### Dialoghi
**Non mostrare mai dialoghi modali nei test**, perché bloccano l'esecuzione. Conviene testare le parti che si possono isolare: la costruzione del form del dialogo (senza `ShowModal`), la mappatura pulsanti ↔ ModalResult, le funzioni di utilità, la conversione dei parametri. Se serve eseguire un dialogo, chiuderlo con un timer o con un messaggio postato (`PostMessage` di un click), e solo se è davvero necessario.

### DB navigator
Usare un `TClientDataSet` o un `TFDMemTable` in memoria, senza database: stato dei pulsanti con dataset vuoto, in modifica, all'inizio e alla fine.

### Regole generali
- Il nome e la `<summary>` di ogni test di regressione descrivono l'anomalia ("Regression. ..."): il test documenta il bug.
- Un test per ogni finding corretto, **scritto prima del fix**, che deve fallire sul codice originale.
- I limiti noti di codice di terze parti (Skia, Lottie) si tengono come test `[Ignore('motivo')]`.
- Se un test su un'anomalia sospetta passa già sul codice originale, lo si tiene come guardia e si segnala che il finding era falso.

## Fase 4 — Ciclo red → fix → green

1. Scrivere tutti i test e lanciarli: devono fallire quelli dei bug (primo "red run").
2. Correggere un'area alla volta e rilanciare.
3. **Red run finale sui sorgenti originali**: si compila la stessa suite puntando `DCC_UnitSearchPath` alla copia di backup (o a un `svn export -r <rev iniziale> source`), con output in una cartella di scratch. Ogni test di regressione deve fallire lì e passare sul codice corretto. In msbuild il `;` nei valori delle proprietà va scritto `%3B` (in un `.cmd`, `%%3B`).
4. Girare la suite su **Win32 e Win64**.
5. **Verificare la compilazione dei package reali** D13 (runtime e design-time), mandando l'output in una cartella di scratch con `/p:DCC_BplOutput=... /p:DCC_DcpOutput=... /p:DCC_DcuOutput=...`, per non sovrascrivere le BPL installate nell'IDE. msbuild genera i `.res` mancanti nella cartella dei package: se prima non c'erano, vanno cancellati.
6. Non ricompilare le demo nella loro cartella di output se l'utente le sta usando nel debugger: anche qui si usa un output alternativo.

## Accorgimenti pratici (Windows, Delphi, strumenti)

- **Encoding**: i sorgenti Delphi sono spesso ASCII o ANSI con CRLF (in SVGIconImageList il file `.inc` era ISO-8859-1). Per modifiche multi-riga conviene usare piccoli script Python salvati su file, che leggono e scrivono con `newline=''` e l'encoding giusto (in SVGIconImageList: `scratchpad\edit_util.py`, sostituzioni con conteggio esatto delle occorrenze). I heredoc di bash si rompono con gli apici del codice Pascal.
- **Python**: `C:\Users\carlo\AppData\Local\Python\pythoncore-3.14-64\python.exe` (percorso completo, non è nel PATH).
- **DUnitX**: `Assert.WillNotRaiseAny` con un metodo anonimo può dare "no overloaded version": basta chiamare direttamente il codice, perché un'eccezione fa comunque fallire il test. I metodi protected si raggiungono con una classe "cracker" (`TXxxAccess = class(TXxx)`).
- Un `TListView` o un altro TWinControl che crea l'handle vuole un parent: form nascosto.
- I test che scrivono file usano `%TEMP%` con un nome GUID e cancellano in `finally`.

## Fase 5 — Commit, versione, documentazione

- **Commit su SVN a blocchi**, uno per area (motore di rendering, pulsanti, contenitori, famiglie, dialoghi, suite di test), con **messaggi in italiano** nello stile del log esistente (`svn log -l 10`) e la riga `Co-Authored-By` finale. Prima di ogni commit si fa `svn status`; si committano solo i file toccati, elencati esplicitamente. **Chiedere conferma prima di committare.**
- I messaggi di log non si possono correggere dopo (il server non ha l'hook pre-revprop-change): vanno scritti con attenzione, un blocco coerente per commit.
- `svn:ignore` su `Test` per `Bin` e `Lib`.
- **Nuova versione**: individuare tutti i punti in cui compare la versione (costanti nei sorgenti, `README.md`, `Setup\Setup.iss`, eventuali docs VitePress in `D:\ETHEA\DocEthea\VitePress\...`) e salvare l'elenco in memoria per le release successive. I package non vanno versionati (preferenza dell'utente). `README.htm` lo aggiorna l'utente a mano.
- **README e docs**: una sezione "Automated Test Suite" e le release notes. Nelle release notes i nomi di tag SVG/HTML (`<use>`, `<style>`) vanno tra backtick: GitHub li nasconde e VitePress (Vue) non compila.
- **Setup**: includere la cartella `Test` escludendo `\Test\Bin\*` e `\Test\Lib\*`, escludere `\ManualTest`, aggiungere le voci in `[UninstallDelete]`. Verificare compilando lo script con ISCC con `/O<cartella scratch>` e contando nell'output di ISCC le righe "Compressing" per cartella.
- Aggiornare `CLAUDE.md` del progetto con una sezione Testing.

## Checklist finale

- [ ] Report di review consegnato, con i finding verificati e quelli falsi segnalati
- [ ] `Test\` con `run_tests.cmd`, progetti D13 e sorgenti dei test, in CRLF
- [ ] Red run: ogni test di regressione fallisce sulla revisione iniziale
- [ ] Green su Win32 e Win64
- [ ] Package D13 compilati senza errori né warning nuovi
- [ ] Commit a blocchi su SVN, solo file propri
- [ ] Versione, README, CLAUDE.md, Setup, docs aggiornati
- [ ] Limiti di terze parti documentati come test `[Ignore]` e in Known issues
