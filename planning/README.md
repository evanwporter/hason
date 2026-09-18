parseElement (pE)

```
case
   "<" -> pE xs
   " " -> pE xs
   isAlpha -> pN [x, xs]
```



