

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

struple val
enum val
struple type
enum type

### single type

**winner?**

token val(typeval)
struple val(Opt<typeval>, struple)
enum token val -> (typeval, lstr)
enum struple val -> (typeval, lstr, struple)
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
-
.fields: [TypeValueField]
--
```

