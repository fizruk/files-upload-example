Run the server:

```
$ stack build && stack exec files-upload
```

Send `"Hello, world!"` as a file:

```
$ echo "Hello, world" >sample.txt
$ curl -v -F key1=value1 -F upload=@sample.txt http://localhost:8083/files
```

Sample output on the server-side:

```
Input name: "upload"
File name: "sample.txt"
Content type: "text/plain"
------- Content --------
Hello, world

------------------------
("key1","value1")
```
