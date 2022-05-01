# Schema自定义扩展说明

DataAbstract Schema是数据服务的核心，Schema里面提供了CustomAttributes自定义属性配置，我们正是在这里面加入了很多属性来实现一些非常繁琐而又实用的功能。这些功能主要是为了方便客户端开发，而同样可以实现服务端功能开发，比如在CustomAttributes配置role属性，实现服务端RBAC。目前RBAC只是在客户端上实现了，而后台是没有实现的，后台只有Session控制，Session只在用户登录时创建，用户退出时销毁。

Schema CustomAttributes主要用于各种基础视图的创建，包括表单窗体(TFormDataView)，表格视图(TTableGridDataView)，Card视图(TCardGridDataView)，Tree视图(TTreeDataView)等等，基础视图定义核心文件：App_DAView.pas和App_DevExpress.pas。

创建视图的时候，我们最繁重的工作是根据业务数据来创建视图的内容，而Schema DataTable的CustomAttributes就是我们加进去的业务视图信息。我们看看表格视图在设置数据时的代码：
```delphi
procedure TTableGridDataView.SetData(Data: TCustomData);
begin
  TableView.DataController.DataSource := nil;
  TableView.ClearItems;
  inherited;

  if Data = nil then
    Exit;
  { 重新建立视图 }
  DevExpress.BuildTableView(TableView, FCustomData.Source);
  BuildEditButtonEvent;
  BuildFilterFields;
  RestoreViewLayout(AppCore.UserIni);

  ConfigViewAfterData;
end;


procedure TDevExpressModule.BuildTableView(TableView: TcxGridDBTableView;
  DataSource: TDADataSource);
var
  Value: string;
  Attr: TStrings;
  DataTable: TDADataTable;
begin
  Assert(DataSource <> nil);
  Assert(DataSource.DataTable <> nil);

  // RegisterGridStyleListener(TableView);

  // 确保DataTable的Schema可用
  DataTable := DataSource.DataTable;
  Attr := DataTable.CustomAttributes;
  with TableView do
  begin
    BeginUpdate;
    try
      OnMouseWheel := DoTableViewMouseWheel;
      ClearItems;
      DataController.Summary.DefaultGroupSummaryItems.Clear;
      // FilterBox.Visible := fvNonEmpty;

      // 不区分大小写对筛选性能有一点影响
      DataController.Filter.Options := DataController.Filter.Options +
        [fcoCaseInsensitive];
      OptionsCustomize.ColumnHidingOnGrouping := False;
      OptionsCustomize.ColumnsQuickCustomization := True;
      OptionsCustomize.DataRowSizing := True;
      OptionsCustomize.ColumnFiltering := True;
      OptionsData.Deleting := False;
      OptionsData.Inserting := False;
      OptionsData.Editing := DataSource.AutoEdit;

      // OptionsSelection.MultiSelect := True;
      // OptionsSelection.HideSelection := False;
      // OptionsSelection.InvertSelect := True;

      OptionsView.GroupByBox := True;
      OptionsView.GroupSummaryLayout := gslAlignWithColumnsAndDistribute;
      OptionsView.Indicator := True;
      OptionsView.IndicatorWidth := iIndicatorWidth;
      OptionsView.DataRowHeight := iDefaultDataRowHeight;
      // OptionsView.GroupFooters := gfVisibleWhenExpanded;
      // OptionsView.ShowEditButtons := gsebForFocusedRecord;
      // OptionsView.Navigator := False;
      // OptionsView.HeaderHeight := iDefaultDataRowHeight;
      // OptionsView.GroupRowHeight := iDefaultDataRowHeight;

      OptionsBehavior.NavigatorHints := True;
      OptionsBehavior.GoToNextCellOnEnter := True;
      OptionsBehavior.FocusCellOnCycle := True;
      OptionsBehavior.FocusFirstCellOnNewRecord := True;
      OptionsBehavior.BestFitMaxRecordCount := 50;

      DataController.DataSource := DataSource;
      OnCustomDrawIndicatorCell := cxTableViewCustomDrawIndicatorCell;
      Styles := GetGridStyle;

      DataController.CreateAllItems;
      DataController.KeyFieldNames := Attr.Values['KeyFieldNames'];

      Value := Attr.Values['OptionsData.Editing'];
      if Value <> '' then
        OptionsData.Editing := StrToBoolDef(Value, False);

      Value := Attr.Values['OptionsData.Deleting'];
      if Value <> '' then
        OptionsData.Deleting := StrToBoolDef(Value, False);

      Value := Attr.Values['OptionsData.Inserting'];
      if Value <> '' then
        OptionsData.Inserting := StrToBoolDef(Value, False);

      Value := Attr.Values['OptionsView.GroupByBox'];
      if Value <> '' then
        OptionsView.GroupByBox := StrToBoolDef(Value, False);

      Value := Attr.Values['OptionsView.ShowEditButtons'];
      if Value <> '' then
        OptionsView.ShowEditButtons := TcxGridShowEditButtons
          (GetEnumValue(TypeInfo(TcxGridShowEditButtons), Value));

      Value := Attr.Values['OptionsView.IndicatorWidth'];
      if Value <> '' then
        OptionsView.IndicatorWidth := StrToIntDef(Value, iIndicatorWidth);

      Value := Attr.Values['OptionsView.DataRowHeight'];
      if Value <> '' then
        OptionsView.DataRowHeight := StrToIntDef(Value, iDefaultDataRowHeight);

      Value := Attr.Values['OptionsView.CellAutoHeight'];
      if Value <> '' then
        OptionsView.CellAutoHeight := StrToBoolDef(Value, False);

      Value := Attr.Values['OptionsView.Footer'];
      if Value <> '' then
        OptionsView.Footer := StrToBoolDef(Value, False);

      Value := Attr.Values['OptionsSelection.MultiSelect'];
      if Value <> '' then
        OptionsSelection.MultiSelect := StrToBoolDef(Value, False);

      Value := Attr.Values['OptionsCustomize.ColumnSorting'];
      if Value <> '' then
        OptionsCustomize.ColumnSorting := StrToBoolDef(Value, True);

      Value := Attr.Values['BackgroundBitmaps'];
      if Value <> '' then
        BackgroundBitmaps.Background.Assign(AppCore.ImageCenter.Get(Value));

      // Value := Attr.Values['Preview.Column'];
      // if Value <> '' then
      // begin
      // Preview.Visible := True;
      // Preview.Column := GetColumnByFieldName(Value);
      // end;

      BuildGridColumns(TableView, DataSource);

      if Assigned(FOnBuildTableView) then
        FOnBuildTableView(TableView);
    finally
      EndUpdate;
    end;
  end;
end;

procedure TDevExpressModule.BuildGridColumns(TableView: TcxGridDBTableView;
  DataSource: TDADataSource);
var
  I: Integer;
  Column: TcxGridDBColumn;
  DataTable: TDADataTable;
begin
  DataTable := DataSource.DataTable;
  for I := 0 to DataTable.FieldCount - 1 do
  begin
    Column := TableView.GetColumnByFieldName(DataTable.Fields[I].Name);
    if Column <> nil then
      BuildGridColumn(Column, DataTable.Fields[I], DataSource);
  end;
end;


procedure TDevExpressModule.BuildGridColumn(Column: TcxGridColumn;
  DAField: TDAField; DataSource: TDADataSource);
var
  Attr: TStrings;
  Value: string;
  CommonPropertiesCount: Integer;
begin
  Assert(Column <> nil);
  Assert(DAField <> nil);

  try
    Attr := DAField.CustomAttributes;

    Column.HeaderAlignmentHorz := taCenter;
    Column.HeaderAlignmentVert := vaCenter;

    if DAField.DisplayWidth <= 0 then
    begin
      if DAField.DataType in [datDateTime, datFloat, datCurrency, datAutoInc,
        datInteger, datLargeInt, datBoolean, datDecimal, datCardinal,
        datLargeAutoInc, datByte, datShortInt, datWord, datSmallInt,
        datLargeUInt, datSingleFloat] then
        Column.Width := FNumColumnWidth
      else
        Column.Width := FTextColumnWidth
    end;

    // 优化速度! 注意这里!!! 小心代码被屏蔽!!!
    CommonPropertiesCount := Attr.Count;
    if CommonPropertiesCount = 0 then
      Exit;

    Value := Attr.Values['MultiEditorField'];
    if Value <> '' then
    begin
      Column.Tag := Integer(TcxGridDBTableView(Column.GridView)
        .GetColumnByFieldName(Value));
      if Column.Tag <> 0 then
        Column.OnGetPropertiesForEdit := DoTableViewGetProperties;
      Exit;
    end;

    Value := Attr.Values['Properties'];
    Column.PropertiesClass := GetPropertiesClass(Value);
    Column.Properties := GetProperties(Value);
    if Value <> '' then
    begin
      Dec(CommonPropertiesCount);
    end;

    Value := Attr.Values['Options.Editing'];
    if Value <> '' then
    begin
      Column.Options.Editing := StrToBoolDef(Value, False);
      Column.Options.Focusing := Column.Options.Editing;
      Dec(CommonPropertiesCount);
    end;

    BuildProperties(Column, Column.Properties, DAField, DataSource);

    // 优化速度! 注意这里!!! 小心代码被屏蔽!!!
    if CommonPropertiesCount <= 0 then
      Exit;

    Value := Attr.Values['Options.Focusing'];
    if Value <> '' then
    begin
      Column.Options.Focusing := StrToBoolDef(Value, True);
    end;

    Value := Attr.Values['Styles.Content'];
    if Value <> '' then
    begin
      Column.Styles.Content := TcxStyle(GetStyle(Value));
    end;

    Value := Attr.Values['Styles.Footer'];
    if Value <> '' then
    begin
      Column.Styles.Footer := TcxStyle(GetStyle(Value));
    end;

    {
      Value := Attr.Values['Options.CellMerging'];
      if Value <> '' then
      begin
      Column.Options.CellMerging := StrToBoolDef(Value, False);
      end;

      Value := Attr.Values['Options.Focusing'];
      if Value <> '' then
      Column.Options.Focusing := StrToBoolDef(Value, False);
    }

    Value := Attr.Values['GroupIndex'];
    if Value <> '' then
      Column.GroupIndex := StrToIntDef(Value, 0);

    // TcxSummaryKind = (skNone, skSum, skMin, skMax, skCount, skAverage);
    Value := Attr.Values['Summary.FooterKind'];
    if Value <> '' then
      Column.Summary.FooterKind :=
        TcxSummaryKind(GetEnumValue(TypeInfo(TcxSummaryKind), Value));

    Value := Attr.Values['Summary.FooterFormat'];
    if Value <> '' then
      Column.Summary.FooterFormat := Value;

    Value := Attr.Values['Summary.GroupFooterKind'];
    if Value <> '' then
    begin
      Column.Summary.GroupFooterKind :=
        TcxSummaryKind(GetEnumValue(TypeInfo(TcxSummaryKind), Value));
      Column.Summary.GroupKind := Column.Summary.GroupFooterKind;
    end;

    Value := Attr.Values['Summary.GroupFooterFormat'];
    if Value <> '' then
    begin
      Column.Summary.GroupFooterFormat := Value;
      Column.Summary.GroupFormat := Value
    end;

  except
    on E: Exception do
      AppCore.Logger.Write('(BuildGridColumn)' + E.Message, mtError, 0)
  end;
end;
```
上面的代码自动帮我们设置好表头和表尾，以及每列的编辑属性。看代码我们可以知道在DataTable CustomAttributes可以设置哪些属性，Fields CustomAttribute可以设置哪些属性。

再来看看表单视图在设置数据时的代码：
```delphi
procedure TFormDataView.SetData(ACustomData: TCustomData);
begin
  inherited;
  ToolBarGroup.Visible := StrToBoolDef(ACustomData.CustomAttributes.Values
    ['FormToolBar'], True);
  ImageName := ACustomData.CustomAttributes.Values['FormImageName'];
  BuildDataEditor;
end;

procedure TFormDataView.BuildDataEditor;
begin
  { 根据Data创建表单 }
  ViewLayout.BeginUpdate;
  try
    FreeAndNil(FEditorContainer);
    if ViewData = nil then
      Exit;
    FEditorContainer := FEditorGroup.CreateGroup();
    FEditorContainer.ShowBorder := False;
    DevExpress.BuildFormView(FEditorContainer, ViewData);
  finally
    ViewLayout.EndUpdate;
  end;
end;

{ 选取可见的字段进行表单创建 }

procedure TDevExpressModule.BuildFormView(AGroup: TdxlayoutGroup;
  ACustomData: TCustomData);
var
  I, J: Integer;
  VisibleFields: TStringDynArray;
  FormEditFields: string;
  FormControlWidth: Integer;
  CaptionAlignTop: Boolean;
begin

  with ACustomData.Table do
  begin
    FormEditFields := CustomAttributes.Values['FormEditFields'];
    FormControlWidth :=
      StrToIntDef(CustomAttributes.Values['FormControlWidth'], 0);
    CaptionAlignTop := False;

    if FormEditFields <> '' then
      VisibleFields := DelimitedTextToArray(FormEditFields)
    else
    begin
      // 分配足够的空间
      SetLength(VisibleFields, FieldCount);
      J := 0;
      for I := 0 to FieldCount - 1 do
      begin
        if Fields[I].Visible then
        begin
          VisibleFields[J] := Fields[I].Name;
          Inc(J);
        end;
      end;
      // 分配实际的空间
      SetLength(VisibleFields, J);
    end;
  end;
  BuildFormView2(AGroup, ACustomData.Source, VisibleFields, FormControlWidth,
    CaptionAlignTop);
end;

{ 根据选取字段进行表单创建 }

procedure TDevExpressModule.BuildFormView2(AGroup: TdxlayoutGroup;
  ADataSource: TDADataSource; AFields: array of string; AControlWidth: Integer;
  ACaptionAlignTop: Boolean);
var
  I: Integer;
  Editor: TcxCustomEdit;
  BandGroup: TdxlayoutGroup;
  EditorGroup: TdxlayoutGroup;
  Value: Variant;
  DefaultBeginsLayer: Boolean;
  DAField: TDAField;
begin
  EditorGroup := nil;

  DefaultBeginsLayer :=
    StrToBoolDef(ADataSource.DataTable.CustomAttributes.Values
    ['DefaultBeginsLayer'], True);
  if AControlWidth <= 0 then
    AControlWidth := StrToIntDef(ADataSource.DataTable.CustomAttributes.Values
      ['FormControlWidth'], 0);

  for I := 0 to Length(AFields) - 1 do
  begin
    DAField := ADataSource.DataTable.FieldByName(AFields[I]);

    Value := DAField.CustomAttributes.Values['BandCaption'];
    if Value <> '' then
    begin
      BandGroup := AGroup.CreateGroup();
      BandGroup.Caption := Value;
      // BandGroup.ButtonOptions.ShowExpandButton := True;
      EditorGroup := nil;
    end;

    Value := DAField.CustomAttributes.Values['BeginsLayer'];
    if (EditorGroup = nil) or StrToBoolDef(Value, DefaultBeginsLayer) then
    begin
      if BandGroup <> nil then
        EditorGroup := BandGroup.CreateGroup()
      else
        EditorGroup := AGroup.CreateGroup();

      EditorGroup.ShowBorder := False;
      EditorGroup.LayoutDirection := ldHorizontal;
      // EditorGroup.ItemControlAreaAlignment := catAuto;
    end;

    Editor := BuildFormEditor(ADataSource, DAField, AGroup, AControlWidth);

    with EditorGroup.CreateItemForControl(Editor) do
    begin
      Caption := DAField.DisplayLabel + ':';
      if StrToBoolDef(DAField.CustomAttributes.Values['AlignHorz'], False) then
        AlignHorz := ahClient;
      AlignVert := avCenter;
      // CaptionOptions.AlignVert := tavTop;
      if ACaptionAlignTop then
        CaptionOptions.Layout := clTop;
    end;
    // if Assigned(FOnWrapControl) then FOnWrapControl(Editor, Fields[I]);
  end;
end;


function TDevExpressModule.BuildFormEditor(ADataSource: TDADataSource;
  ADAField: TDAField; AOwner: TComponent; AEditorWidth: Integer): TcxCustomEdit;
var
  Att: string;
  CommonPropertiesCount: Integer;
begin
  CommonPropertiesCount := ADAField.CustomAttributes.Count;

  Att := ADAField.CustomAttributes.Values['Properties'];
  if Att <> '' then
    Dec(CommonPropertiesCount);

  Result := GetDBControlClass(Att).Create(AOwner);
  with TcxDBEditDataBinding(TcxCustomEditAccess(Result).DataBinding) do
  begin
    DataSource := ADataSource;
    DataField := ADAField.Name;
  end;

  // 设置控件宽度
  Att := ADAField.CustomAttributes.Values['ControlWidth'];
  if Att <> '' then
    Result.Width := StrToIntDef(Att, Round(TextColumnWidth * 1.8))
  else
    Result.Width := IfThen(AEditorWidth <= 0, Round(TextColumnWidth * 1.8),
      AEditorWidth);

  if Result.InheritsFrom(TcxCustomCheckBox) then
    TcxCustomCheckBox(Result).Transparent := True;

  Att := ADAField.CustomAttributes.Values['Options.Editing'];
  if Att <> '' then
  begin
    Result.TabStop := StrToBoolDef(Att, False);
    Dec(CommonPropertiesCount);
  end;

  BuildProperties(Result, Result.ActiveProperties, ADAField, ADataSource);

  // 优化速度! 注意这里!!! 小心代码被屏蔽!!!
  if CommonPropertiesCount <= 0 then
    Exit;

  Att := ADAField.CustomAttributes.Values['Options.Focusing'];
  if Att <> '' then
  begin
    Result.TabStop := StrToBoolDef(Att, True);
  end;

  Att := ADAField.CustomAttributes.Values['Style.Color'];
  if Att <> '' then
  begin
    Result.Style.Color := StringToColor(Att);
  end;

  Att := ADAField.CustomAttributes.Values['Style.TextColor'];
  if Att <> '' then
  begin
    Result.Style.TextColor := StringToColor(Att);
  end;

  Att := ADAField.CustomAttributes.Values['Style.TextStyle.fsBold'];
  if (Att <> '') and StrToBoolDef(Att, False) then
  begin
    Result.Style.TextStyle := Result.Style.TextStyle + [fsBold];
  end;

  Att := ADAField.CustomAttributes.Values['Style.Font.Size'];
  if (Att <> '') then
  begin
    Result.Style.Font.Size := StrToIntDef(Att, 14);
  end;

  Att := ADAField.CustomAttributes.Values['Style.Font.Name'];
  if (Att <> '') then
  begin
    Result.Style.Font.Name := Att;
  end;
end;

```
这些重要的基础视图的创建都是基于DevExpress，窗体布局基于DevExpress Layout。这些控件既美观又好用，还能减少代码量。

下面查看HisService_Clinic.daSchema里面的Clin_Reg_Help，该DataTable用于创建挂号视图。DataTable CustomAttributes：
```yaml
DefaultBeginsLayer=False
FormControlWidth=200
FormImageName=misc\clin_reg.png
```

字段 OfficeName CustomAttributes：
```yaml
BeginsLayer=True
Properties.IMEMode=imClose
Style.Font.Size=20
Properties=PopupEdit
IME.DictName=Sys_ClinicOffice
IME.GetValueField=OfficeID;OfficeName
IME.SetValueField=OfficeID;OfficeName
```

字段DiagPrice CustomAttributes：
```yaml
Options.Editing=False
Style.Font.Size=20
Properties=CurrencyEdit
Properties.DecimalPlaces=2
Properties.DisplayFormat=0.00
```

最后我们可以检查 ***doc/Schema自定义属性说明.xls*** 文件，看看哪些地方可以设置哪些属性。

