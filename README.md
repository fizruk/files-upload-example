Run the server:

```
$ stack build && stack exec files-upload
```

Send `"Hello, world!"` as a file:

```
$ echo "Hello, world" >sample.txt
$ curl -v -F key1=value1 -F upload=@sample.txt http://localhost:8083/files
```
