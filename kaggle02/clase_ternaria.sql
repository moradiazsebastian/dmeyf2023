CREATE OR REPLACE TABLE competencia_02 AS FROM read_csv_auto('../datasets/competencia_02_crudo.csv'); 

CREATE OR REPLACE TABLE ternaria AS FROM (
    WITH partitions AS (
        SELECT numero_de_cliente,
            foto_mes,
            lead(foto_mes,2) OVER w AS lead_2,
            lead(foto_mes,1) OVER w AS lead_1,
            last_value(foto_mes) OVER w AS last_value,
            (SELECT max(foto_mes) FROM competencia_02) AS max_date
            FROM competencia_02
            WINDOW w AS (
                PARTITION BY numero_de_cliente 
                ORDER BY foto_mes 
                RANGE BETWEEN UNBOUNDED PRECEDING 
                AND UNBOUNDED FOLLOWING
            )
    )
    SELECT *,
        (
            CASE 
            WHEN lead_2 IS NOT NULL 
                THEN 'CONTINUA'
            WHEN lead_2 IS NULL
                AND last_value < p.max_date
                AND lead_1 IS NULL 
                OR (
                    lead_1 IS NOT NULL 
                    AND lead_1-p.foto_mes != 1
                )               
                THEN 'BAJA+1'
            WHEN lead_2 IS NULL 
                AND last_value < p.max_date
                AND lead_1 IS NOT NULL 
                THEN 'BAJA+2'
            ELSE NULL
            END
        ) AS clase_ternaria
    FROM competencia_02 c 
    JOIN partitions p USING (numero_de_cliente,foto_mes) 
);

ALTER TABLE ternaria DROP COLUMN lead_2;
ALTER TABLE ternaria DROP COLUMN lead_1;
ALTER TABLE ternaria DROP COLUMN last_value;
ALTER TABLE ternaria DROP COLUMN max_date;

SELECT COUNT(*),clase_ternaria FROM ternaria GROUP BY clase_ternaria;

COPY ternaria TO 'competencia_02.csv.gz' (FORMAT CSV, HEADER);