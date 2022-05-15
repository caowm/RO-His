# PaxCompiler脚本扩展功能设计说明

- 脚本存放在数据库表Sys_Script，结构如下：

```sql
CREATE TABLE Sys_Script (
    ScriptID varchar(50) NOT NULL,
    SystemID varchar(20) NOT NULL,
    Category varchar(200) NOT NULL,
    Caption varchar(50) NOT NULL,
    ImageName varchar(50) NOT NULL,
    Access varchar(20) NOT NULL,
    ShortKey varchar(20) NOT NULL,
    Flag int NOT NULL,
    CustomAttributes varchar(2000) NULL,
    Script text NOT NULL,
    OrderNum varchar(10) NOT NULL,
    Disabled int DEFAULT ((0)) NOT NULL,
    Memo varchar(50) NULL,
    CONSTRAINT PK_Sys_Script PRIMARY KEY CLUSTERED (ScriptID)
)
```

- 客户端启动时导入脚本定义，然后初始化脚本加载器：

```delphi
unit HisClient_Organizer;

uses
  App_PaxCompiler,
  IMPORT_App_Function,   <----------
  Import_App_Common,
  IMPORT_App_DAModel,
  IMPORT_App_DAView,
  Import_App_DevExpress,
  Import_App_Class,
  Import_App_FastReport,
  IMPORT_dxLayoutContainer,   <----------
  
constructor THisClientOrganizer.Create(AOwner: TComponent);
begin
  AppCore.ID := sAppID;
  AppCore.Version := sAppVer;
  AppCore.ResourceVersion := AppCore.Config.ReadString(sAppSection, 'ResourceVersion', '');

  FScriptLoader := TDBScriptLoader.Create(Self);       <----------
  FScriptLoader.ScriptData := TCustomData.Create(Self, HisConnection,
    sDataServiceSystem, sDataNameSysScript);
  inherited;
end;
```

- 用户登录之后加载脚本，并创建TPaxOperation：

```delphi
procedure THisClientOrganizer.DoAppEvent(Sender: TObject);
begin
  inherited;
  case AppCore.State of
    asBeginning:
      begin
        FScriptLoader.LoadScriptOperation;      <-------
        LoadMedicineInOutOperation;
        HisUser.EnableOperations;
      end;
  end;
end;

procedure TDBScriptLoader.LoadScriptOperation;
begin
  AppCore.Logger.Write('正在加载脚本...', mtInfo, 0);
  with FScriptData do
  begin
    // 打开没有停用的
    OpenByFieldValue('Disabled', 0);
    while not Eof do
    begin
      with TPaxOperation.Create(AsString['ScriptID']) do    <-------
      begin
        Category := AsString['Category'];
        Caption := AsString['Caption'];
        Access := AsString['Access'];
        ImageName := AsString['ImageName'];
        ShortKey := TextToShortCut(AsString['ShortKey']);
        Flag := AsInteger['Flag'];
        Order := AsString['OrderNum'];
        CustomAttributes.Text := AsString['CustomAttributes'];
        Script := AsString['Script'];
        OnLoadScript := DoLoadScript;
      end;
      Next;
    end;
    Close;
  end;
end;
```

- TPaxOperation继承自TBaseOperation，TBaseOperation在创建时会自动添加到AppCore.Operations，系统据此创建菜单和左边的操作树。点击菜单或者操作树统一执行函数TBaseOperation.Execute。TPaxOperation.DoExecute会先编译脚本，然后执行脚本(编译和执行只发生一次)，最后调用脚本里面的DoExecute函数，当然DoExecute可以根据不同的参数完成不同的功能。TPaxOperation就代表一个可执行脚本的操作对象，TViewOperation代表一个展示视图的操作对象，TProcOperation代表一个执行指定函数的操作对象！

```delphi
constructor TBaseOperation.Create(const AID: string; AOwner: TOperations = nil);
begin
  FCustomAttributes := TStringList.Create;
  FEnabled := True;
  FVisible := True;
  FGUID := AID;
  FClicks := 0;

  if AOwner = nil then
    AOwner := AppCore.Operations;
  AOwner.Add(Self);    <-----

  State := osCreating;
end;

function TBaseOperation.Execute(CommandID: Integer;
  const Param: array of Variant): Variant;
begin
  if Enabled then
  begin
    DoBeforeExecute;
    Result := DoExecute(CommandID, Param);   <-----
    Inc(FClicks);
    DoAfterExecute;
  end;
end;

function TProcOperation.DoExecute(CommandID: Integer;
  const Param: array of Variant): Variant;
begin
  if Assigned(FOnExecute) then
    Result := FOnExecute(Self, CommandID, Param);    <-------
end;

function TViewOperation.DoExecute(CommandID: Integer;
  const Param: array of Variant): Variant;
begin
  // 防止插件视图在程序退出时释放后又被创建
  if not(AppCore.State in [asAskQuit, asEnding]) or (FView <> nil) then
    Result := View.DoExecute(CommandID, Param);     <-------
end;

function TBaseView.DoExecute(CommandID: Integer;
  const Param: array of Variant): Variant;
begin
  case CommandID of
    iOperationCommand_Default:
      AppCore.MainView.ShowView(Self);     <-------
  end;
end;

function TPaxOperation.DoExecute(CommandID: Integer; const Param: array of Variant): Variant;
var
  P: Pointer;

  procedure CompileAndExecute();
  begin
    if not FCompiled or (FProgram.CodePtr = nil) then
    begin
      AppCore.Logger.Write('正在编译脚本:' + Caption, mtInfo, 0);

      if (FScript = '') and Assigned(FOnNeedScript) then
        FOnNeedScript(GUID, FScript);

      if (FScript = '') then
        raise Exception.CreateFmt('%s-%s:说好的脚本呢', [Category, Caption]);

      if CompileProgram(FScript, FProgram, FSelfVariable) then
      begin
        FCompiled := True;
        FProgram.Run; //初始化
      end
      else begin
        FScript := ''; // 自动清空脚本后可重新加载
        OutputCompileError();
      end
    end;

    //全名是这种形式: 'UnitName.Something', 这里源文件不能带单元名!!!
    P := FProgram.GetAddress('DOEXECUTE');  // 注意：运行错误不会重新加载脚本
    if Assigned(P) then
      Result := TPaxExecProc(P)(CommandID, Param);     <-------
  end;

```

- 看看其中一个收入统计脚本(ScriptID=pas_stat_hosp_fee_office)：

```delphi
uses Classes, Forms, SysUtils, uDAWhere, App_Common, App_DAModel, App_DAView;

type
  TScriptView = class(TPivotDataView)
  private
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy(); override;
    procedure DataQuery(); override;
  end;

var ScriptView: TScriptView;

constructor TScriptView.Create(AOwner: TComponent);
begin
  inherited;
  ViewData := TCustomData.Create(Self, DataConnection, 'HisService_Stat', 'Stat_Hosp_Fee_Office');
  PeriodGroup.Visible := True;
  SetPeriodFormatYearMonth();
  Operation := MyOperation;
end;

destructor TScriptView.Destroy();
begin
  inherited;
  ScriptView := nil; // 这里很重要
end;

procedure TScriptView.DataQuery();
begin
  if not AppCore.User.HaveAccess('5050') then
    ViewData.OpenByList(['StatDate', 'StatDate', 'OfficeID'],
      [FormatDateTime('YYYYMM', BeginDate), FormatDateTime('YYYYMM', EndDate), 
        AppCore.User.OfficeID],
      [dboGreaterOrEqual, dboLessOrEqual, dboEqual], [dboAnd, dboAnd])
  else
    ViewData.OpenByBetween('StatDate', FormatDateTime('YYYYMM', BeginDate), 
      FormatDateTime('YYYYMM', EndDate))
end;

function DoExecute(CommandID: Integer; const Param: array of Variant): Variant;
begin
  if ScriptView = nil then
  begin
    ScriptView := TScriptView.Create(Application);
  end;
  AppCore.MainView.ShowView(ScriptView);
end;

procedure DoClear;
begin
  ScriptView.Free
end;

begin
end.
```

- 只要知道OperationID，通过TOperations.SearchOperation可找到任何一个TBaseOperation，这样任何一个TBaseOperation都可以调用其他TBaseOperation，这就是客户端各部分之间交互通讯机制。

- 用户登录之后会执行带有iOperationFlag_Start标志的TBaseOperation

```delphi
procedure TAppCore.DoOnReady(Sender: TObject);
begin
  if FConfig.ReadBool(sAppSection, 'LoadOperations', False) then
    FOperations.LoadOperation(FConfig);

  if not Assigned(FOldAppException) then
  begin
    FOldAppException := Application.OnException;
    Application.OnException := OnAppException;
  end;

  FOperations.SortByOrder;
  FOperations.StartOperation; // 启动操作     <-------
end;

procedure TOperations.StartOperation;
begin
  ForEach(DoOperationStart);
end;

procedure TOperations.DoOperationStart(Operation: TBaseOperation);
begin
  if (Operation.Flag and iOperationFlag_Start) <> 0 then
    Operation.Execute(iOperationCommand_Start, []);
end;
```

- 脚本里面有一个启动项(ScriptID=pas_base_init):

```delphi

uses Classes, Variants, App_Function, App_Common, App_DAModel;

type
  TDictData = class(TCustomData)
  private
  procedure DoBeforePost(Sender: TObject);
  public
    constructor Create(AOwner: TComponent; ADataConnection: TDataConnection;
      const AServiceName, ALogicalName: string); override;
  end;

  TDictDataClass = class of TDictData;

constructor TDictData.Create(AOwner: TComponent;  ADataConnection: TDataConnection;
  const AServiceName, ALogicalName: string);
begin
  inherited Create(AOwner, ADataConnection, AServiceName, ALogicalName);
  OnBeforePost := DoBeforePost;
end;

procedure TDictData.DoBeforePost(Sender: TObject);
var
  S1: string;
begin
  S1 := CustomAttributes.Values['ChineseField'];
    if (S1 <> '') and (AsString['IME_PY'] = VarToStr(OldValue['IME_PY'])) then
      AsString['IME_PY']:= GetPyHeadOfHzs(AsString[S1]);
end;

procedure RegisterDataDict();
begin     
  DataContainer.RegisterData(DataConnection, 'HisService_YB', 'D_YB_ZZCode', TCustomData, 0, '');  
  DataContainer.RegisterData(DataConnection, 'HisService_YB', 'D_YB_Area', TCustomData, 0, '');
  DataContainer.RegisterData(DataConnection, 'HisService_YB', 'D_YB_10_JSBZLX', TCustomData, 0, '');
  DataContainer.RegisterData(DataConnection, 'HisService_XNH', 'XNH_Illness', TCustomData, 0, '');
  DataContainer.RegisterData(DataConnection, 'HisService_XNH', 'XNH_bxgs', TCustomData, 0, '');  
  DataContainer.RegisterData(DataConnection, 'HisService_Dict', 'D_UrgentSite', TDictData, 0, '');  
  DataContainer.RegisterData(DataConnection, 'HisService_System', 'Sys_Op_OnDuty', TCustomData, 0, '');
  DataContainer.RegisterData(DataConnection, 'HisService_Clinic', 'Pro_Clin_Reg_Patient', TCustomData, 0, '');
end;

begin
  RegisterDataDict();
end.
```



