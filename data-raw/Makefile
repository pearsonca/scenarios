
# assumes running from *within* data-raw
DIR := ../data

R = $(strip Rscript $^ $(1) $@)

default: ${DIR}/covid_abm_eg.rds

${DIR}:
	mkdir -p $@

${DIR}/covid_abm_eg.rds: ../../covid-abm/exp/active-vac/covid-active-v7.sqlite

${DIR}/%.rds: %.R
	$(call R)
