/* Projeto Cargill */
/* Estimações - Modelo Almost Ideal Demand System */
/* Copyright 2005 Cláudio R. Lucinda */

clear all
set memory 128m
set matsize 800
set more off, permanently
version 11
cd "C:\Users\ClaudioLucinda\Dropbox\Aulas\GV\Curso de OI - Pós\2014-2\Demanda\"


insheet using ///
"D_STATA.TXT", clear tab


/* Colocando os Rótulos nas Variáveis */

label variable	vkg_oliva_1 "VENDAS ( EM 1000 KILOS  )-GALLO,AZEITE DE OLIVA "
label variable	vkg_oliva_2 "VENDAS ( EM 1000 KILOS  )-CARBONELL,AZEITE DE OLIVA,CARBONELL "
label variable	vkg_oliva_3 "VENDAS ( EM 1000 KILOS  )-LA ESPANOLA,AZEITE DE OLIVA,CARGILL "
label variable	vkg_oliva_4 "VENDAS ( EM 1000 KILOS  )-ANDORINHA,AZEITE DE OLIVA,SIMAO & CI "
label variable	vkg_oliva_5 "VENDAS ( EM 1000 KILOS  )-BORGES,AZEITE DE OLIVA,oliva BORGES "
label variable	vkg_oliva_6 "VENDAS ( EM 1000 KILOS  )-FAISAO,AZEITE DE OLIVA,OLIMA IND.DE "
label variable	vkg_oliva_7 "VENDAS ( EM 1000 KILOS  )-OUTRA MARCA,AZEITE DE OLIVA,OUTRO FA "
label variable	vkg_oliva_8 "VENDAS ( EM 1000 KILOS  )-OUTRO MANUEL SERRA,AZEITE DE OLIVA,M "
label variable	vkg_oliva_9 "VENDAS ( EM 1000 KILOS  )-ARISCO,AZEITE DE OLIVA,UNILEVER "
label variable	vval_oliva_1 "VENDAS EM VALOR ( 1000 REAIS  )-GALLO,AZEITE DE OLIVA "
label variable	vval_oliva_2 "VENDAS EM VALOR ( 1000 REAIS  )-CARBONELL,AZEITE DE OLIVA,CARBONELL "
label variable	vval_oliva_3 "VENDAS EM VALOR ( 1000 REAIS  )-LA ESPANOLA,AZEITE DE OLIVA,CARGILL "
label variable	vval_oliva_4 "VENDAS EM VALOR ( 1000 REAIS  )-ANDORINHA,AZEITE DE OLIVA,SIMAO & CI "
label variable	vval_oliva_5 "VENDAS EM VALOR ( 1000 REAIS  )-BORGES,AZEITE DE OLIVA,oliva BORGES "
label variable	vval_oliva_6 "VENDAS EM VALOR ( 1000 REAIS  )-FAISAO,AZEITE DE OLIVA,OLIMA IND.DE "
label variable	vval_oliva_7 "VENDAS EM VALOR ( 1000 REAIS  )-OUTRA MARCA,AZEITE DE OLIVA,OUTRO FA "
label variable	vval_oliva_8 "VENDAS EM VALOR ( 1000 REAIS  )-OUTRO MANUEL SERRA,AZEITE DE OLIVA,M "
label variable	vval_oliva_9 "VENDAS EM VALOR ( 1000 REAIS  )-ARISCO,AZEITE DE OLIVA,UNILEVER "
label variable	svol_oliva_1 "SHARE VOLUME-GALLO,AZEITE DE OLIVA "
label variable	svol_oliva_2 "SHARE VOLUME-CARBONELL,AZEITE DE OLIVA,CARBONELL "
label variable	svol_oliva_3 "SHARE VOLUME-LA ESPANOLA,AZEITE DE OLIVA,CARGILL "
label variable	svol_oliva_4 "SHARE VOLUME-ANDORINHA,AZEITE DE OLIVA,SIMAO & CI "
label variable	svol_oliva_5 "SHARE VOLUME-BORGES,AZEITE DE OLIVA,oliva BORGES "
label variable	svol_oliva_6 "SHARE VOLUME-FAISAO,AZEITE DE OLIVA,OLIMA IND.DE "
label variable	svol_oliva_7 "SHARE VOLUME-OUTRA MARCA,AZEITE DE OLIVA,OUTRO FA "
label variable	svol_oliva_8 "SHARE VOLUME-OUTRO MANUEL SERRA,AZEITE DE OLIVA,M "
label variable	svol_oliva_9 "SHARE VOLUME-ARISCO,AZEITE DE OLIVA,UNILEVER "
label variable	sval_oliva_1 "SHARE VALOR-GALLO,AZEITE DE OLIVA "
label variable	sval_oliva_2 "SHARE VALOR-CARBONELL,AZEITE DE OLIVA,CARBONELL "
label variable	sval_oliva_3 "SHARE VALOR-LA ESPANOLA,AZEITE DE OLIVA,CARGILL "
label variable	sval_oliva_4 "SHARE VALOR-ANDORINHA,AZEITE DE OLIVA,SIMAO & CI "
label variable	sval_oliva_5 "SHARE VALOR-BORGES,AZEITE DE OLIVA,oliva BORGES "
label variable	sval_oliva_6 "SHARE VALOR-FAISAO,AZEITE DE OLIVA,OLIMA IND.DE "
label variable	sval_oliva_7 "SHARE VALOR-OUTRA MARCA,AZEITE DE OLIVA,OUTRO FA "
label variable	sval_oliva_8 "SHARE VALOR-OUTRO MANUEL SERRA,AZEITE DE OLIVA,M "
label variable	sval_oliva_9 "SHARE VALOR-ARISCO,AZEITE DE OLIVA,UNILEVER "
label variable	vkg_soja_1 "VENDAS ( EM 1000 KILOS  )-SOJA,CARGILL "
label variable	vkg_soja_2 "VENDAS ( EM 1000 KILOS  )-LIZA,SOJA,CARGILL "
label variable	vkg_soja_3 "VENDAS ( EM 1000 KILOS  )-VELEIRO,SOJA,CARGILL "
label variable	vkg_soja_4 "VENDAS ( EM 1000 KILOS  )-SOJA,BUNGE "
label variable	vkg_soja_5 "VENDAS ( EM 1000 KILOS  )-PRIMOR,SOJA,BUNGE "
label variable	vkg_soja_6 "VENDAS ( EM 1000 KILOS  )-SOYA,SOJA,BUNGE "
label variable	vkg_soja_7 "VENDAS ( EM 1000 KILOS  )-OUTRA MARCA,SOJA,OUTRO FABRICANTE "
label variable	vkg_soja_8 "VENDAS ( EM 1000 KILOS  )-SADIA,SOJA,ADM "
label variable	vkg_soja_9 "VENDAS ( EM 1000 KILOS  )-SINHA,SOJA,CARAMURU "
label variable	vval_soja_1 "VENDAS EM VALOR ( 1000 REAIS  )-SOJA,CARGILL "
label variable	vval_soja_2 "VENDAS EM VALOR ( 1000 REAIS  )-LIZA,SOJA,CARGILL "
label variable	vval_soja_3 "VENDAS EM VALOR ( 1000 REAIS  )-VELEIRO,SOJA,CARGILL "
label variable	vval_soja_4 "VENDAS EM VALOR ( 1000 REAIS  )-SOJA,BUNGE "
label variable	vval_soja_5 "VENDAS EM VALOR ( 1000 REAIS  )-PRIMOR,SOJA,BUNGE "
label variable	vval_soja_6 "VENDAS EM VALOR ( 1000 REAIS  )-SOYA,SOJA,BUNGE "
label variable	vval_soja_7 "VENDAS EM VALOR ( 1000 REAIS  )-OUTRA MARCA,SOJA,OUTRO FABRICANTE "
label variable	vval_soja_8 "VENDAS EM VALOR ( 1000 REAIS  )-SADIA,SOJA,ADM "
label variable	vval_soja_9 "VENDAS EM VALOR ( 1000 REAIS  )-SINHA,SOJA,CARAMURU "
label variable	svol_soja_1 "SHARE VOLUME-SOJA,CARGILL "
label variable	svol_soja_2 "SHARE VOLUME-LIZA,SOJA,CARGILL "
label variable	svol_soja_3 "SHARE VOLUME-VELEIRO,SOJA,CARGILL "
label variable	svol_soja_4 "SHARE VOLUME-SOJA,BUNGE "
label variable	svol_soja_5 "SHARE VOLUME-PRIMOR,SOJA,BUNGE "
label variable	svol_soja_6 "SHARE VOLUME-SOYA,SOJA,BUNGE "
label variable	svol_soja_7 "SHARE VOLUME-OUTRA MARCA,SOJA,OUTRO FABRICANTE "
label variable	svol_soja_8 "SHARE VOLUME-SADIA,SOJA,ADM "
label variable	svol_soja_9 "SHARE VOLUME-SINHA,SOJA,CARAMURU "
label variable	sval_soja_1 "SHARE VALOR-SOJA,CARGILL "
label variable	sval_soja_2 "SHARE VALOR-LIZA,SOJA,CARGILL "
label variable	sval_soja_3 "SHARE VALOR-VELEIRO,SOJA,CARGILL "
label variable	sval_soja_4 "SHARE VALOR-SOJA,BUNGE "
label variable	sval_soja_5 "SHARE VALOR-PRIMOR,SOJA,BUNGE "
label variable	sval_soja_6 "SHARE VALOR-SOYA,SOJA,BUNGE "
label variable	sval_soja_7 "SHARE VALOR-OUTRA MARCA,SOJA,OUTRO FABRICANTE "
label variable	sval_soja_8 "SHARE VALOR-SADIA,SOJA,ADM "
label variable	sval_soja_9 "SHARE VALOR-SINHA,SOJA,CARAMURU "
label variable	vkg_composto_1 "VENDAS ( EM 1000 KILOS  )-OLIVIA,COMPOSTO,CARGILL "
label variable	vkg_composto_2 "VENDAS ( EM 1000 KILOS  )-MARIA,COMPOSTO,VIDA ALIMENTOS "
label variable	vkg_composto_3 "VENDAS ( EM 1000 KILOS  )-SINHA,COMPOSTO,CARAMURU "
label variable	vkg_composto_4 "VENDAS ( EM 1000 KILOS  )-COCINERO,COMPOSTO,MOLINOS "
label variable	vkg_composto_5 "VENDAS ( EM 1000 KILOS  )-OUTRA MARCA,COMPOSTO,OUTRO FABRICANT "
label variable	vval_composto_1 "VENDAS EM VALOR ( 1000 REAIS  )-OLIVIA,COMPOSTO,CARGILL "
label variable	vval_composto_2 "VENDAS EM VALOR ( 1000 REAIS  )-MARIA,COMPOSTO,VIDA ALIMENTOS "
label variable	vval_composto_3 "VENDAS EM VALOR ( 1000 REAIS  )-SINHA,COMPOSTO,CARAMURU "
label variable	vval_composto_4 "VENDAS EM VALOR ( 1000 REAIS  )-COCINERO,COMPOSTO,MOLINOS "
label variable	vval_composto_5 "VENDAS EM VALOR ( 1000 REAIS  )-OUTRA MARCA,COMPOSTO,OUTRO FABRICANT "
label variable	svol_composto_1 "SHARE VOLUME-OLIVIA,COMPOSTO,CARGILL "
label variable	svol_composto_2 "SHARE VOLUME-MARIA,COMPOSTO,VIDA ALIMENTOS "
label variable	svol_composto_3 "SHARE VOLUME-SINHA,COMPOSTO,CARAMURU "
label variable	svol_composto_4 "SHARE VOLUME-COCINERO,COMPOSTO,MOLINOS "
label variable	svol_composto_5 "SHARE VOLUME-OUTRA MARCA,COMPOSTO,OUTRO FABRICANT "
label variable	sval_composto_1 "SHARE VALOR-OLIVIA,COMPOSTO,CARGILL "
label variable	sval_composto_2 "SHARE VALOR-MARIA,COMPOSTO,VIDA ALIMENTOS "
label variable	sval_composto_3 "SHARE VALOR-SINHA,COMPOSTO,CARAMURU "
label variable	sval_composto_4 "SHARE VALOR-COCINERO,COMPOSTO,MOLINOS "
label variable	sval_composto_5 "SHARE VALOR-OUTRA MARCA,COMPOSTO,OUTRO FABRICANT "
label variable	vkg_canola_1 "VENDAS ( EM 1000 KILOS  )-LIZA,CANOLA,CARGILL "
label variable	vkg_canola_2 "VENDAS ( EM 1000 KILOS  )-PURILEV,CANOLA,CARGILL "
label variable	vkg_canola_3 "VENDAS ( EM 1000 KILOS  )-SALADA,CANOLA,BUNGE "
label variable	vkg_canola_4 "VENDAS ( EM 1000 KILOS  )-OUTRA MARCA,CANOLA,OUTRO FABRICANTE "
label variable	vkg_canola_5 "VENDAS ( EM 1000 KILOS  )-SUAVIT,CANOLA,COCAMAR "
label variable	vkg_canola_6 "VENDAS ( EM 1000 KILOS  )-SINHA,CANOLA,CARAMURU "
label variable	vval_canola_1 "VENDAS EM VALOR ( 1000 REAIS  )-LIZA,CANOLA,CARGILL "
label variable	vval_canola_2 "VENDAS EM VALOR ( 1000 REAIS  )-PURILEV,CANOLA,CARGILL "
label variable	vval_canola_3 "VENDAS EM VALOR ( 1000 REAIS  )-SALADA,CANOLA,BUNGE "
label variable	vval_canola_4 "VENDAS EM VALOR ( 1000 REAIS  )-OUTRA MARCA,CANOLA,OUTRO FABRICANTE "
label variable	vval_canola_5 "VENDAS EM VALOR ( 1000 REAIS  )-SUAVIT,CANOLA,COCAMAR "
label variable	vval_canola_6 "VENDAS EM VALOR ( 1000 REAIS  )-SINHA,CANOLA,CARAMURU "
label variable	svol_canola_1 "SHARE VOLUME-LIZA,CANOLA,CARGILL "
label variable	svol_canola_2 "SHARE VOLUME-PURILEV,CANOLA,CARGILL "
label variable	svol_canola_3 "SHARE VOLUME-SALADA,CANOLA,BUNGE "
label variable	svol_canola_4 "SHARE VOLUME-OUTRA MARCA,CANOLA,OUTRO FABRICANTE "
label variable	svol_canola_5 "SHARE VOLUME-SUAVIT,CANOLA,COCAMAR "
label variable	svol_canola_6 "SHARE VOLUME-SINHA,CANOLA,CARAMURU "
label variable	sval_canola_1 "SHARE VALOR-LIZA,CANOLA,CARGILL "
label variable	sval_canola_2 "SHARE VALOR-PURILEV,CANOLA,CARGILL "
label variable	sval_canola_3 "SHARE VALOR-SALADA,CANOLA,BUNGE "
label variable	sval_canola_4 "SHARE VALOR-OUTRA MARCA,CANOLA,OUTRO FABRICANTE "
label variable	sval_canola_5 "SHARE VALOR-SUAVIT,CANOLA,COCAMAR "
label variable	sval_canola_6 "SHARE VALOR-SINHA,CANOLA,CARAMURU "
label variable	vkg_girassol_1 "VENDAS ( EM 1000 KILOS  )-LIZA,GIRASSOL,CARGILL "
label variable	vkg_girassol_2 "VENDAS ( EM 1000 KILOS  )-SALADA,GIRASSOL,BUNGE "
label variable	vkg_girassol_3 "VENDAS ( EM 1000 KILOS  )-SINHA,GIRASSOL,CARAMURU "
label variable	vkg_girassol_4 "VENDAS ( EM 1000 KILOS  )-COCINERO,GIRASSOL,MOLINOS "
label variable	vval_girassol_1 "VENDAS EM VALOR ( 1000 REAIS  )-LIZA,GIRASSOL,CARGILL "
label variable	vval_girassol_2 "VENDAS EM VALOR ( 1000 REAIS  )-SALADA,GIRASSOL,BUNGE "
label variable	vval_girassol_3 "VENDAS EM VALOR ( 1000 REAIS  )-SINHA,GIRASSOL,CARAMURU "
label variable	vval_girassol_4 "VENDAS EM VALOR ( 1000 REAIS  )-COCINERO,GIRASSOL,MOLINOS "
label variable	svol_girassol_1 "SHARE VOLUME-LIZA,GIRASSOL,CARGILL "
label variable	svol_girassol_2 "SHARE VOLUME-SALADA,GIRASSOL,BUNGE "
label variable	svol_girassol_3 "SHARE VOLUME-SINHA,GIRASSOL,CARAMURU "
label variable	svol_girassol_4 "SHARE VOLUME-COCINERO,GIRASSOL,MOLINOS "
label variable	sval_girassol_1 "SHARE VALOR-LIZA,GIRASSOL,CARGILL "
label variable	sval_girassol_2 "SHARE VALOR-SALADA,GIRASSOL,BUNGE "
label variable	sval_girassol_3 "SHARE VALOR-SINHA,GIRASSOL,CARAMURU "
label variable	sval_girassol_4 "SHARE VALOR-COCINERO,GIRASSOL,MOLINOS "
label variable	vkg_milho_1 "VENDAS ( EM 1000 KILOS  )-LIZA,MILHO,CARGILL "
label variable	vkg_milho_2 "VENDAS ( EM 1000 KILOS  )-MAZOLA,MILHO "
label variable	vkg_milho_3 "VENDAS ( EM 1000 KILOS  )-SALADA,MILHO,BUNGE "
label variable	vkg_milho_4 "VENDAS ( EM 1000 KILOS  )-SINHA,MILHO,CARAMURU "
label variable	vkg_milho_5 "VENDAS ( EM 1000 KILOS  )-OUTRA MARCA,MILHO,OUTRO FABRICANTE "
label variable	vkg_milho_6 "VENDAS ( EM 1000 KILOS  )-SUAVIT,MILHO,COCAMAR "
label variable	vkg_milho_7 "VENDAS ( EM 1000 KILOS  )-SIOL,MILHO,SIOL "
label variable	vval_milho_1 "VENDAS EM VALOR ( 1000 REAIS  )-LIZA,MILHO,CARGILL "
label variable	vval_milho_2 "VENDAS EM VALOR ( 1000 REAIS  )-MAZOLA,MILHO "
label variable	vval_milho_3 "VENDAS EM VALOR ( 1000 REAIS  )-SALADA,MILHO,BUNGE "
label variable	vval_milho_4 "VENDAS EM VALOR ( 1000 REAIS  )-SINHA,MILHO,CARAMURU "
label variable	vval_milho_5 "VENDAS EM VALOR ( 1000 REAIS  )-OUTRA MARCA,MILHO,OUTRO FABRICANTE "
label variable	vval_milho_6 "VENDAS EM VALOR ( 1000 REAIS  )-SUAVIT,MILHO,COCAMAR "
label variable	vval_milho_7 "VENDAS EM VALOR ( 1000 REAIS  )-SIOL,MILHO,SIOL "
label variable	svol_milho_1 "SHARE VOLUME-LIZA,MILHO,CARGILL "
label variable	svol_milho_2 "SHARE VOLUME-MAZOLA,MILHO "
label variable	svol_milho_3 "SHARE VOLUME-SALADA,MILHO,BUNGE "
label variable	svol_milho_4 "SHARE VOLUME-SINHA,MILHO,CARAMURU "
label variable	svol_milho_5 "SHARE VOLUME-OUTRA MARCA,MILHO,OUTRO FABRICANTE "
label variable	svol_milho_6 "SHARE VOLUME-SUAVIT,MILHO,COCAMAR "
label variable	svol_milho_7 "SHARE VOLUME-SIOL,MILHO,SIOL "
label variable	sval_milho_1 "SHARE VALOR-LIZA,MILHO,CARGILL "
label variable	sval_milho_2 "SHARE VALOR-MAZOLA,MILHO "
label variable	sval_milho_3 "SHARE VALOR-SALADA,MILHO,BUNGE "
label variable	sval_milho_4 "SHARE VALOR-SINHA,MILHO,CARAMURU "
label variable	sval_milho_5 "SHARE VALOR-OUTRA MARCA,MILHO,OUTRO FABRICANTE "
label variable	sval_milho_6 "SHARE VALOR-SUAVIT,MILHO,COCAMAR "
label variable	sval_milho_7 "SHARE VALOR-SIOL,MILHO,SIOL "



/* Comecando a Analise */
tsset tempo

/* Gerando a soma por categorias - Valor*/
foreach x in soja milho girassol canola composto oliva {
	egen tval_`x'=rowtotal(vval_`x'_*)
	egen tvkg_`x'=rowtotal(vkg_`x'_*)
}

/* Gerando o Preco Medio por Categoria */
foreach x in soja milho girassol canola composto oliva {
	gen p_`x'=tval_`x'/tvkg_`x'
}

/* Gerando o Share de Valor para Cada uma das Categorias */
egen totalval=rowtotal(tval_*)
foreach x in soja milho girassol canola composto oliva {
	gen tsval_`x'=tval_`x'/totalval
}

/* Gerando os Logs dos Precos */
foreach x in soja milho girassol canola composto oliva {
	gen l_p_`x'=ln(p_`x')
}

/* Gerando o Ln_Y e o Ln_Big_P*/
gen l_y=ln(totalval)
foreach x in soja milho girassol canola composto oliva {
	egen m_sval_`x'=mean(tsval_`x')
}
gen Big_P=m_sval_soja*ln(p_soja)+m_sval_oliva*ln(p_oliva)+m_sval_canola*ln(p_canola)+m_sval_girassol*ln(p_girassol)+m_sval_composto*ln(p_composto)+m_sval_milho*ln(p_milho)
egen Big_Q=rowtotal(tvkg_*)
gen l_Big_P=ln(Big_P)
gen l_Big_Q=ln(Big_Q)
gen l_y_P=l_y-l_Big_P

save "Neo_data.dta", replace

