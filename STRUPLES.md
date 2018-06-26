

## implementation

### concepts

struple token
struple struct

enum token
enum struct

### enum data in type

token val
struple val
struple type
enum type

### enum data in val

token val
struple val
enum val
struple type
enum type

### single type

token val
struple val
enum val
user type
typeval constant

### leema code

```
enum FieldValue
|IndexedField(TypeValue)
|NamedField(Str, TypeValue)
--

struple TypeValueField
.name: Option<Str>
.type: TypeValue
--

struple TypeValue
.name: Str
.fields: [TypeValueField]
--
```

