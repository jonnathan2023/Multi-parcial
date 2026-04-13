# Contexto del Dataset: Calidad del Café Arábica en la Selva Alta del Perú

---

## ¿De qué trata este conjunto de datos?

Cuando uno compra un café de especialidad, en el empaque suele decir cosas como "notas a fruta, acidez brillante, aroma floral". Esas características no son capricho del catador ni del marketing: **son el resultado directo de dónde y cómo creció el café**. La altitud, el clima, la cantidad de lluvia, las horas de sol y la calidad del suelo moldean la composición química del grano mucho antes de que llegue a la taza.

Este dataset recoge información de **199 lotes de café Coffea arabica** producidos en la selva alta del Perú, una de las regiones cafetaleras más reconocidas del mundo. Cada lote proviene de una finca diferente, con su propia ubicación y microclima. Todos fueron procesados de la misma manera (método lavado) y con la misma variedad (Typica), de modo que las diferencias entre lotes reflejen únicamente el efecto del ambiente, no del proceso.

Cada observación en la base de datos representa un lote de café con las siguientes condiciones fijas para todos los lotes:

- Misma variedad: Typica (la más tradicional del Perú)
- Mismo método de procesamiento: vía húmeda (lavado), que es el estándar en la selva alta peruana
- Misma época de cosecha: campaña principal (mayo–agosto)
- Granos recolectados en su punto óptimo de madurez (fruto rojo)

Fijar estas condiciones es clave: si cada lote tuviera diferente variedad o diferente método de procesamiento, nunca sabríamos si las diferencias químicas se deben al ambiente o a esas otras causas. Al dejarlas fijas, el único factor que varía entre lotes es el ambiente de la finca.

---

## Un poco de contexto: el café y la altitud en Perú

Perú es el tercer exportador mundial de café orgánico. Sus zonas cafetaleras más importantes —Junín, San Martín, Cajamarca, Amazonas y Cusco— están ubicadas en la llamada **selva alta**, una franja de montaña que oscila entre los 1 000 y los 2 200 metros sobre el nivel del mar (msnm).

A mayor altitud, el clima es más fresco. Ese frío hace que el fruto del café madure **más lentamente**, lo cual parece malo a primera vista pero resulta excelente para la calidad: el grano tiene más tiempo para acumular azúcares, ácidos y compuestos aromáticos. Es el mismo principio por el que una fruta que madura despacio en el árbol suele tener más sabor que una cosechada verde y madurada artificialmente.

Por eso, cuando se dice que un café viene de "altura" o de "alta montaña", no es solo geografía: es una promesa de mayor complejidad química en la taza.

---

## Diccionario de datos

El dataset tiene 8 columnas: 5 describen el ambiente de la finca y 3 describen la calidad química del café producido.

---

### Variables del ambiente (predictoras)

#### Altitud
**Unidad:** metros sobre el nivel del mar (msnm)  
**Rango en los datos:** 1 100 — 2 000 msnm  
**Media:** 1 517 msnm  
**Precisión de medida:** número entero (sin decimales)

La altitud es la variable ambiental más influyente sobre la calidad del café. Actúa como un "regulador" natural de temperatura: por cada 100 metros que se sube, la temperatura baja aproximadamente 0.6 °C. Esa relación es tan consistente que conocer la altitud de una finca permite estimar con bastante precisión su temperatura promedio.

> **Dato curioso:** El café que se produce entre 1 200 y 1 800 msnm en Perú es clasificado internacionamente como "strictly high grown" (SHG), una categoría de alta cotización en los mercados de especialidad.

---

#### Temperatura media anual
**Unidad:** grados Celsius (°C)  
**Rango en los datos:** 13.0 — 22.0 °C  
**Media:** 17.5 °C  
**Precisión de medida:** 1 decimal

La temperatura determina la velocidad de maduración del fruto. El rango ideal para Coffea arabica de calidad está entre 15 y 20 °C: lo suficientemente fresco para una maduración lenta, pero no tanto como para dañar el cultivo.

> **Ojo estadístico:** La temperatura y la altitud están muy fuertemente correlacionadas en este dataset (r ≈ −0.95). Es casi redundante tener las dos variables a la vez: cuando una sube, la otra baja de manera predecible. Esto genera un problema estadístico llamado multicolinealidad que veremos en el análisis.

---

#### Precipitación anual
**Unidad:** milímetros por año (mm/año)  
**Rango en los datos:** 1 000 — 2 500 mm/año  
**Media:** 1 608 mm/año  
**Precisión de medida:** número entero

La lluvia es el agua que alimenta la planta durante todo el año. El café de altura necesita entre 1 200 y 2 000 mm anuales para desarrollarse bien. Muy poca lluvia estresa la planta y produce granos pequeños y poco desarrollados; demasiada lluvia puede diluir los compuestos del grano o favorecer hongos.

El efecto neto de la precipitación sobre la calidad química resulta ser débil en este dataset, porque las zonas cafetaleras de la selva alta peruana están dentro del rango aceptable y la variación entre fincas no alcanza a traducirse en diferencias químicas detectables estadísticamente. Esto tiene una consecuencia importante en el análisis.

> **Patrón a notar:** A diferencia de la temperatura, la precipitación no sigue un patrón tan claro con la altitud: dos fincas a la misma altura pueden recibir cantidades de lluvia muy distintas dependiendo de la orientación del valle o la cercanía a ríos. Eso es precisamente lo que la hace una variable "independiente" en términos estadísticos.

---

#### Horas de sol directo por día
**Unidad:** horas por día (h/día)  
**Rango en los datos:** 3.0 — 8.0 h/día  
**Media:** 5.5 h/día  
**Precisión de medida:** 1 decimal

No todas las fincas a la misma altitud reciben la misma cantidad de sol. Una ladera orientada al este queda en sombra desde el mediodía; una ladera abierta al norte puede recibir sol casi todo el día. Las horas de sol afectan directamente la fotosíntesis: más sol equivale a más producción de azúcares en el fruto, lo que luego se traduce en mayor concentración de compuestos aromáticos y acidez en el grano.

Esta variable es valiosa porque **aporta información que la altitud no captura**. Dos fincas a 1 500 msnm pueden tener perfiles de calidad muy distintos si una está expuesta al sol y la otra está sombreada por el bosque.

> **Dato curioso:** En Perú, muchas fincas de especialidad cultivan el café bajo sombra de árboles nativos deliberadamente, no solo para proteger el ecosistema sino porque la sombra regulada produce granos con aromas más complejos.

---

#### Materia orgánica del suelo
**Unidad:** porcentaje del peso seco del suelo (%)  
**Rango en los datos:** 2.0 — 9.0 %  
**Media:** 5.6 %  
**Precisión de medida:** 2 decimales

La materia orgánica es todo el material descompuesto —hojas caídas, raíces muertas, microorganismos— que convierte el suelo en un sistema vivo y fértil. Un suelo con alto contenido orgánico retiene mejor la humedad, libera nutrientes de forma gradual y permite un desarrollo radicular más profundo. Todo eso se traduce en una planta mejor nutrida que produce granos con más complejidad química.

En la selva alta peruana, el contenido orgánico del suelo varía mucho según el historial de la parcela: un suelo que estuvo bajo bosque nativo tendrá más materia orgánica que uno que lleva décadas cultivado sin abonos.

> **Patrón a considerar:** Esta variable es casi independiente del clima y la altitud. Su valor depende más de las decisiones del caficultor (compostar, no quemar, mantener cobertura vegetal) que de la geografía. Eso la convierte en una palanca de mejora directamente accionable.

---

### Variables de calidad química del café (respuesta)

Estas tres variables se miden en el laboratorio sobre cada lote, siguiendo siempre el mismo protocolo para que los resultados sean comparables entre fincas.

---

#### Compuestos aromáticos volátiles totales (VOCs)
**Unidad:** miligramos por kilogramo de café tostado (mg/kg)  
**Rango en los datos:** 80 — 240 mg/kg  
**Media:** 166 mg/kg  
**Precisión de medida:** 2 decimales

Los VOCs son las moléculas que literalmente "vuelan" del café cuando se abre el empaque o se prepara la bebida. Son responsables de todo lo que el olfato percibe: notas florales, frutales, a caramelo, a nuez. Son cientos de compuestos distintos (furanos, terpenos, aldehídos, ésteres…), y aquí se reporta su suma total medida sobre grano con tostado medio estandarizado.

Un café de altura con maduración lenta acumula más precursores de estos compuestos durante el desarrollo del fruto; al tostar, esos precursores se transforman en una mayor variedad y concentración de aromáticos.

> **Rango de referencia:** Un café de baja calidad o mal procesado puede tener VOCs por debajo de 100 mg/kg. Los cafés de especialidad de altura suelen superar los 150 mg/kg.

---

#### Acidez titulable
**Unidad:** gramos por litro de extracto estándar (g/L)  
**Rango en los datos:** 4.0 — 7.4 g/L  
**Media:** 5.5 g/L  
**Precisión de medida:** 3 decimales

La acidez es probablemente el atributo más malentendido del café de especialidad. Para muchos consumidores "ácido" significa amargo o irritante, pero en el mundo del café de calidad la acidez es un atributo **positivo y deseable**: es la sensación viva, brillante y refrescante que distingue a un buen café de altura de uno plano.

Esta medida se hace en laboratorio sobre un extracto preparado en condiciones idénticas para todos los lotes (misma proporción de café y agua, misma temperatura, mismo tiempo). Los ácidos responsables de esta acidez son principalmente el ácido cítrico, málico y láctico, los mismos que se encuentran en frutas.

A mayor altitud y menor temperatura, el fruto retiene mejor estos ácidos durante la maduración porque las bajas temperaturas frenan su degradación enzimática.

> **Curiosidad:** El café de zonas tropicales a baja altitud suele tener acidez titulable por debajo de 4 g/L, lo que lo hace percibir plano o sin vida en taza. Los cafés de competencia de Perú y Etiopía suelen superar los 6 g/L.

---

#### Sólidos disueltos totales — TDS
**Unidad:** porcentaje del peso del extracto (%)  
**Rango en los datos:** 1.06 — 1.54 %  
**Media:** 1.31 %  
**Precisión de medida:** 4 decimales

El TDS mide cuánto material soluble libera el grano cuando se prepara como bebida en condiciones de laboratorio controladas. Es, en términos simples, una medida de **cuánto café hay en la taza**: a mayor TDS, la bebida tiene más cuerpo, más sabor y más intensidad.

Aquí se mide sobre un extracto estandarizado (misma receta para todos los lotes), de modo que las diferencias entre lotes reflejen propiedades del grano y no diferencias en la preparación.

Un TDS más alto indica que el grano tiene mayor extractabilidad: sus compuestos solubles se transfieren con más eficiencia al agua, lo que se asocia generalmente con granos bien desarrollados de fincas con buenas condiciones de suelo y altitud.

> **Referencia práctica:** La Specialty Coffee Association (SCA) recomienda un TDS de entre 1.15 y 1.45 % para una taza bien preparada. Los datos de este dataset están dentro de ese rango.

---

## ¿Por qué modelar estas variables juntas?

Alguien podría preguntar: ¿por qué no hacer tres análisis separados, uno para VOCs, uno para acidez y uno para TDS?

La respuesta está en que **estas tres variables no son independientes entre sí**. Un lote que tiene muchos aromáticos también tiende a tener más acidez; y un lote con alta extractabilidad (TDS alto) suele ser el mismo que tiene buen perfil aromático. No es coincidencia: las tres propiedades son consecuencia de los mismos factores (altitud, sol, suelo), así que suben y bajan juntas.

Cuando las variables respuesta están correlacionadas, analizarlas por separado produce resultados menos precisos e ignora información valiosa sobre cómo se relacionan entre sí. La **regresión lineal multivariada** permite modelarlas simultáneamente, aprovechando esa correlación para obtener estimaciones más eficientes y una visión más completa de cómo el ambiente afecta la calidad del café.

En términos simples: el ambiente no produce "solo aroma" o "solo acidez"; produce un **perfil completo de calidad**, y ese perfil se captura mejor analizando las tres variables a la vez.

---

## Resumen de los datos

| Variable | Tipo | Unidad | Mínimo | Media | Máximo |
|---|---|---|---:|---:|---:|
| altitud | Predictora | msnm | 1 100 | 1 517 | 2 000 |
| temperatura | Predictora | °C | 13.0 | 17.5 | 22.0 |
| precipitacion | Predictora | mm/año | 1 000 | 1 608 | 2 500 |
| horas_sol | Predictora | h/día | 3.0 | 5.5 | 8.0 |
| mat_organica | Predictora | % | 2.00 | 5.56 | 9.00 |
| VOCs | Respuesta | mg/kg | 80.00 | 165.90 | 239.65 |
| acidez | Respuesta | g/L | 4.000 | 5.548 | 7.443 |
| TDS | Respuesta | % | 1.0560 | 1.3062 | 1.5411 |

**n = 199 lotes** | Coffea arabica variedad Typica | Procesamiento: vía húmeda (lavado) | Cosecha: campaña principal (mayo–agosto)

---

*Documento de contexto elaborado para acompañar el análisis de Regresión Lineal Multivariada — Técnicas Multivariadas.*
