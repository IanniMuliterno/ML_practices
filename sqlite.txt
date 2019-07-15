--O objetivo é retornar uma tabela com soma de renda por país para importação e exportação a partir de duas tabelas, uma contendo transações e outra com identificação das lojas e seus paises correspondentes (COM NULL = 0)

CREATE TABLE trades(id text, seller text, buyer text, value numeric);

INSERT INTO trades values (20121107, 'Lil Mermaid', 'Alice s.p.', 10);
INSERT INTO trades values (20123112, 'Arcus t.g.', 'Y-zap', 30);
INSERT INTO trades values (20120125, 'Alice s.p.', 'Arcus t.g.', 100);
INSERT INTO trades values (20120216, 'Lil Mermaid', 'Absolute', 30);
INSERT INTO trades values (20120217, 'Lil Mermaid', 'Absolute', 50);

CREATE TABLE companies(name text PRIMARY KEY, country text); 

INSERT INTO companies VALUES('Alice s.p.','Wonderland'); 
INSERT INTO companies VALUES('Y-zap','Wonderland'); 
INSERT INTO companies VALUES('Absolute','Mathlands'); 
INSERT INTO companies VALUES('Arcus t.g.','Mathlands'); 
INSERT INTO companies VALUES('Lil Mermaid','Underwater Kingdom'); 
INSERT INTO companies VALUES('none at all','Nothingland');

CREATE TABLE store1(country text, export numeric);
INSERT INTO store1
SELECT country, sum(value)
FROM trades
LEFT JOIN companies
ON trades.seller = companies.name
GROUP BY country;

CREATE TABLE store2(country text, import numeric);

INSERT INTO store2
SELECT country, sum(value)
FROM trades
LEFT JOIN companies
ON trades.buyer = companies.name
GROUP BY country;

SELECT companies.country,IFNULL(export,0),IFNULL(import,0)
FROM companies
LEFT JOIN store1
ON companies.country = store1.country
LEFT JOIN store2
ON companies.country = store2.country
GROUP BY companies.country
ORDER BY companies.country
