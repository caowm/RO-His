
# IdeaService 数据集成服务说明

>数据集成服务提供功能类似DataAbstract RelativityServer。我们用DataAbstract提供的工具Schema Modeler创建schema后，把信息添加到配置文件Idea.xml，相应数据服务就可以用了，无需修改编译服务器代码。

>数据集成服务源码目录：base\ideas\source
>核心文件：IdeaService_Impl.pas
>核心类：TIdeaManager, TIdeaItem, TIdeaService(继承自TDataAbstractService，数据服务模板)

**实现原理：管理类IdeaManager加载Idea.xml文件，每个IdeaItem按照RO规范注册服务工厂。**

```delphi
// 来自IdeaService_Impl.pas
function TIdeaItem.RegisterService: TROClassFactory;
begin
  FSchema.ConnectionManager := IdeaManager.ConnectionManager;
  RegisterOperation;
  RefreshSchema(True);
  Result := TIdeaFactory.Create(ServiceName, Create_IdeaService,
    TIdeaService_Invoker, Self);
  FFactory := Result;
end;

procedure Create_IdeaService(out anInstance: IUnknown; aData: TIdeaItem);
var
  Service: TIdeaService;
begin
  Service := TIdeaService.Create(nil);
  with Service do
  begin
    FRefIdea := aData;
    ConnectionName := aData.ConnectionName;
    SessionManager := IdeaManager.SessionManager;
    EventRepository := IdeaManager.EventRepository;
  end;
  anInstance := Service;
end;

```

**Idea.xml配置文件说明**

```xml
<?xml version="1.0" encoding="UTF-8"?>
<TIdeaManager>
	<Services Type="TIdeaCollection" IsObject="true" IsCollection="true" Count="2" ItemClass="TIdeaItem">
		<Item0>
			<ConnectionName>LIS</ConnectionName>
			<ServiceName>IdeaService_Lis</ServiceName>
			<SchemaName>IdeaService_Lis.daSchema</SchemaName>
		</Item0>
		<Item1>
			<ConnectionName>PACS</ConnectionName>
			<ServiceName>IdeaService_Pacs</ServiceName>
			<SchemaName>IdeaService_Pacs.daSchema</SchemaName>
		</Item1>
	</Services>
</TIdeaManager>
```
字段说明

| 字段  |  说明 |
| ------------ | ------------ |
| ConnectionName | 数据库连接名称 |
| ServiceName | 数据服务名称 |
| SchemaName | Schema文件  |


