# BUILD

quarto add r-wasm/quarto-live 


Must be be via a server.


But the following appears to work without a server
coatless/webr

https://quarto-webr.thecoatlessprofessor.com/qwebr-first-steps.html

engine: knitr
filters:
  - webr

Then use:

````{text}
```{webr-r}
fit = lm(mpg ~ am, data = mtcars)
summary(fit)
plot(fit)
```

````

quarto render load-telemetry.Rmd

quarto render src/load_full_telemetry.Rmd --output-dir ../dist

python3 -m http.server 8123 --directory ./dist


python3 -m http.server 8124 --directory ./shinysite
Shiny:
 R -e "shinylive::export('./src/shinyapp', './shinysite')"

 python3 -m http.server 8124 --directory ./shinysite