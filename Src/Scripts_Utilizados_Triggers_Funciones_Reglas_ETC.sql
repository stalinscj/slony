--Validar que un curso sea prelado un curso de nivel inferior
CREATE OR REPLACE FUNCTION validar_nivel_prelacion()
RETURNS trigger AS $BODY$
DECLARE nivel_curso integer;
DECLARE nivel_prelacion integer;

BEGIN
	SELECT nivel
	FROM cursos AS c
	WHERE NEW.cod_curso = c.cod_curso
	INTO nivel_curso;

	SELECT nivel
	FROM cursos AS c
	WHERE NEW.cod_prelacion = c.cod_curso
	INTO nivel_prelacion;

	IF (nivel_curso>nivel_prelacion) THEN
		RETURN NEW;
	ELSE
		RAISE EXCEPTION 'El nivel del curso(%) debe ser mayor al nivel de la prelación(%)', nivel_curso, nivel_prelacion;
	END IF;
END; $BODY$
LANGUAGE plpgsql VOLATILE;

--Asociar trigger con la funcion de validar_nivel_prelacion
CREATE trigger antes_insertar_prelacion
BEFORE INSERT OR UPDATE ON prelaciones
FOR EACH ROW
EXECUTE PROCEDURE validar_nivel_prelacion(); 


--validar que el instructor domine el área para ser instructor de algún curso de dicha área
CREATE OR REPLACE FUNCTION validar_aprobacion_de_area()
RETURNS trigger AS $BODY$
DECLARE c record;
DECLARE nomArea character varying; 
BEGIN
	
	SELECT DISTINCT INTO nomArea cursos.area
	FROM cursos
	WHERE cursos.cod_curso = NEW.cod_curso;

	FOR c IN (SELECT cod_curso 
			  FROM cursos as cu
			  WHERE cu.area = nomArea ) LOOP
		

		IF EXISTS (SELECT * 
				   FROM participantes AS pa 
				   WHERE pa.cod_curso = c.cod_curso 
				   AND NOT pa.nota = 'A'
				   AND pa.ficha = NEW.instructor ) THEN
			
			RAISE EXCEPTION 'No se ha podido insertar la edicion porque el instructor no domina toda el area';
		END IF;

		IF NOT EXISTS (SELECT * 
				       FROM participantes AS pa 
				       WHERE pa.cod_curso = c.cod_curso 
				       AND pa.ficha = NEW.instructor ) THEN
			
			RAISE EXCEPTION 'No se ha podido insertar la edicion porque el instructor no domina toda el area';
		END IF;


	END LOOP;

	IF c IS NULL THEN 
		RAISE EXCEPTION 'No se ha podido insertar la edicion porque el instructor no domina toda el area';
	END IF;

	RETURN NEW;

END; $BODY$
LANGUAGE plpgsql VOLATILE;

--Asociar trigger con la funcion validar_aprobacion_de_area
CREATE TRIGGER antes_insertarOModificar_edicion_instructorArea
BEFORE INSERT OR UPDATE ON ediciones
FOR EACH ROW
EXECUTE PROCEDURE validar_aprobacion_de_area();


--validar que el participante cumpla con las prelaciones del curso
CREATE OR REPLACE FUNCTION validar_prelaciones()
RETURNS trigger AS $BODY$
DECLARE p record;
BEGIN 
	FOR p IN (SELECT * 
			  FROM prelaciones AS pr 
	          WHERE pr.cod_curso = NEW.cod_curso) LOOP
		
		IF NOT EXISTS (SELECT * 
					   FROM participantes AS pa 
					   WHERE pa.cod_curso = p.cod_prelacion 
					   AND pa.nota = 'A' 
					   AND pa.ficha = NEW.ficha) THEN
			
			RAISE EXCEPTION 'No se ha podido insertar al partipante porque no cumple con las prelaciones';
		END IF;
	END LOOP;

	RETURN NEW;
END; $BODY$
LANGUAGE plpgsql VOLATILE;

--Asociar trigger con la funcion validar_prelaciones
CREATE TRIGGER antes_insertarOModificar_participante_prelaciones
BEFORE INSERT OR UPDATE ON participantes
FOR EACH ROW
EXECUTE PROCEDURE validar_prelaciones();



--validar que el participante no repita un curso que ya aprobó
CREATE OR REPLACE FUNCTION validar_repeticion_curso_aprobado()
RETURNS trigger AS $BODY$
BEGIN 
		
	IF EXISTS (SELECT * 
				FROM participantes AS p 
				WHERE p.cod_curso = NEW.cod_curso AND 
					  p.nota = 'A' AND
					  p.ficha = NEW.ficha ) THEN
			
		RAISE EXCEPTION 'No se ha podido insertar al partipante porque ya aprobó este curso';
	END IF;

	RETURN NEW;
END; $BODY$
LANGUAGE plpgsql VOLATILE;

--Asociar trigger con la funcion validar_repeticion_curso_aprobado
CREATE TRIGGER antes_insertar_participante_curso_aprobado
BEFORE INSERT ON participantes
FOR EACH ROW
EXECUTE PROCEDURE validar_repeticion_curso_aprobado();

--validar que el partipante no sea instructor de si mismo

CREATE OR REPLACE FUNCTION validar_participante_instrcutor()
RETURNS trigger AS $BODY$
BEGIN
	IF EXISTS ( SELECT 1
				FROM ediciones AS ed
				WHERE ed.instructor = NEW.ficha 
				AND ed.lapso = NEW.lapso 
				AND ed.cod_curso = NEW.cod_curso) THEN
		RAISE EXCEPTION 'No se pudo insertar al partipante porque es él instructor';
	END IF;

	RETURN NEW;
END; $BODY$
LANGUAGE plpgsql VOLATILE;

--Asociar trigger con la funcion validar_participante_instrcutor
CREATE TRIGGER antes_insertarOModificar_participanteInstructor
BEFORE INSERT OR UPDATE ON participantes 
FOR EACH ROW
EXECUTE PROCEDURE validar_participante_instrcutor();

--validar que el instructor no sea participante de si mismo

CREATE OR REPLACE FUNCTION validar_instrcutor_participante()
RETURNS trigger AS $BODY$
BEGIN
	IF EXISTS ( SELECT 1
				FROM participantes AS p
				WHERE p.ficha = NEW.ficha 
				AND p.lapso = NEW.lapso 
				AND p.cod_curso = NEW.cod_curso) THEN
		RAISE EXCEPTION 'No se pudo insertar al instructor porque el es partipante del curso';
	END IF;

	RETURN NEW;
END; $BODY$
LANGUAGE plpgsql VOLATILE;

--Asociar trigger con la funcion validar_instrcutor_participante
CREATE TRIGGER antes_insertarOModificar_InstructorParticipante
BEFORE INSERT OR UPDATE ON ediciones 
FOR EACH ROW
EXECUTE PROCEDURE validar_instrcutor_participante();


-- validar que los participantes sean de la misma sede en la cual se dicta el curso

CREATE OR REPLACE FUNCTION validar_participantes_sede()
RETURNS trigger AS $BODY$
DECLARE cod_sede_emp character(3);
DECLARE p record;
BEGIN
	
	SELECT cod_sede
	FROM empleados AS emp
	WHERE emp.ficha = NEW.FICHA
	INTO cod_sede_emp;

	FOR p IN (SELECT * 
			  FROM ediciones AS ed 
	          WHERE ed.cod_curso = NEW.cod_curso AND
	          	    ed.lapso = NEW.lapso) LOOP
		
		IF (cod_sede_emp = p.cod_sede) THEN
			RETURN NEW;
		END IF;
	END LOOP;

	RAISE EXCEPTION 'No se ha podido insertar al partipante porque no es de la misma sede en la cual se dicta el curso';
	
END; $BODY$
LANGUAGE plpgsql VOLATILE;

--Asociar trigger con la funcion validar_participanteSede
CREATE TRIGGER antes_insertarOModificar_participanteSede
BEFORE INSERT OR UPDATE ON participantes 
FOR EACH ROW
EXECUTE PROCEDURE validar_participantes_sede();

--Crear grupos
CREATE GROUP grupo_sede1;
CREATE GROUP grupo_sede2;
CREATE GROUP grupo_sede3;
CREATE GROUP grupo_admin;

--Crear perfiles para cada grupo
CREATE USER usuario_sede1 WITH PASSWORD 'sede1';
CREATE USER usuario_sede2 WITH PASSWORD 'sede2';
CREATE USER usuario_sede3 WITH PASSWORD 'sede3';
CREATE USER usuario_admin WITH PASSWORD 'admin';

--Añadir perfiles a grupos
ALTER GROUP grupo_sede1 ADD USER usuario_sede1;
ALTER GROUP grupo_sede2 ADD USER usuario_sede2;
ALTER GROUP grupo_sede3 ADD USER usuario_sede3;
ALTER GROUP grupo_admin ADD USER usuario_admin;

--Fragmentar datos en vistas para cada sede

CREATE TABLE empleados_sede1( 
	CHECK (cod_sede = '001')
) INHERITS (empleados);

CREATE TABLE empleados_sede2( 
	CHECK (cod_sede = '002')
) INHERITS (empleados);

CREATE TABLE empleados_sede3( 
	CHECK (cod_sede = '003')
) INHERITS (empleados);

CREATE TABLE ediciones_sede1( 
	CHECK (cod_sede = '001')
) INHERITS (ediciones);

CREATE TABLE ediciones_sede2( 
	CHECK (cod_sede = '002')
) INHERITS (ediciones);

CREATE TABLE ediciones_sede3( 
	CHECK (cod_sede = '003')
) INHERITS (ediciones);

CREATE TABLE participantes_sede1() INHERITS (participantes);

CREATE TABLE participantes_sede2() INHERITS (participantes);

CREATE TABLE participantes_sede3() INHERITS (participantes);

--Reglas para actualizar vistas

CREATE OR REPLACE RULE empleados_sede1_upd AS
ON UPDATE TO empleados_sede1 WHERE OLD.cod_sede='001' DO INSTEAD
UPDATE empleados 
SET ficha = NEW.ficha,
	nombre = NEW.nombre,
	cargo = NEW.cargo,
	sueldo = NEW.sueldo,
	fecha_ingreso = NEW.fecha_ingreso,
	status = NEW.status,
	cod_dpto = NEW.cod_dpto
WHERE ficha = OLD.ficha;

CREATE OR REPLACE RULE empleados_sede2_upd AS
ON UPDATE TO empleados_sede2 WHERE OLD.cod_sede='002' DO INSTEAD
UPDATE empleados 
SET ficha = NEW.ficha,
	nombre = NEW.nombre,
	cargo = NEW.cargo,
	sueldo = NEW.sueldo,
	fecha_ingreso = NEW.fecha_ingreso,
	status = NEW.status,
	cod_dpto = NEW.cod_dpto
WHERE ficha = OLD.ficha;

CREATE OR REPLACE RULE empleados_sede3_upd AS
ON UPDATE TO empleados_sede3 WHERE OLD.cod_sede='003' DO INSTEAD
UPDATE empleados 
SET ficha = NEW.ficha,
	nombre = NEW.nombre,
	cargo = NEW.cargo,
	sueldo = NEW.sueldo,
	fecha_ingreso = NEW.fecha_ingreso,
	status = NEW.status,
	cod_dpto = NEW.cod_dpto
WHERE ficha = OLD.ficha;

CREATE OR REPLACE RULE ediciones_sede1_upd AS
ON UPDATE TO ediciones_sede1 WHERE OLD.cod_sede='001' DO INSTEAD
UPDATE ediciones 
SET cod_curso = NEW.cod_curso,
	lapso = NEW.lapso,
	status = NEW.status,
	instructor = NEW.instructor,
	cod_sede = NEW.cod_sede
WHERE cod_curso = OLD.cod_curso AND lapso = OLD.lapso AND cod_sede = OLD.cod_sede;

CREATE OR REPLACE RULE ediciones_sede2_upd AS
ON UPDATE TO ediciones_sede2 WHERE OLD.cod_sede='002' DO INSTEAD
UPDATE ediciones 
SET cod_curso = NEW.cod_curso,
	lapso = NEW.lapso,
	status = NEW.status,
	instructor = NEW.instructor,
	cod_sede = NEW.cod_sede
WHERE cod_curso = OLD.cod_curso AND lapso = OLD.lapso AND cod_sede = OLD.cod_sede;

CREATE OR REPLACE RULE ediciones_sede3_upd AS
ON UPDATE TO ediciones_sede3 WHERE OLD.cod_sede='003' DO INSTEAD
UPDATE ediciones 
SET cod_curso = NEW.cod_curso,
	lapso = NEW.lapso,
	status = NEW.status,
	instructor = NEW.instructor,
	cod_sede = NEW.cod_sede
WHERE cod_curso = OLD.cod_curso AND lapso = OLD.lapso AND cod_sede = OLD.cod_sede;

CREATE OR REPLACE RULE participantes_sede1_upd AS
ON UPDATE TO participantes_sede1 DO INSTEAD
UPDATE participantes 
SET cod_curso = NEW.cod_curso,
	lapso = NEW.lapso,
	nota = NEW.nota,
	ficha = NEW.ficha
WHERE ficha = OLD.ficha;

CREATE OR REPLACE RULE participantes_sede2_upd AS
ON UPDATE TO participantes_sede2 DO INSTEAD
UPDATE participantes 
SET cod_curso = NEW.cod_curso,
	lapso = NEW.lapso,
	nota = NEW.nota,
	ficha = NEW.ficha
WHERE ficha = OLD.ficha;

CREATE OR REPLACE RULE participantes_sede3_upd AS
ON UPDATE TO participantes_sede3 DO INSTEAD
UPDATE participantes 
SET cod_curso = NEW.cod_curso,
	lapso = NEW.lapso,
	nota = NEW.nota,
	ficha = NEW.ficha
WHERE ficha = OLD.ficha;

CREATE OR REPLACE RULE empleados_sede1_ins AS
ON INSERT TO empleados_sede1 WHERE NEW.cod_sede='001' DO INSTEAD
INSERT INTO empleados 
VALUES(
	NEW.ficha,
	NEW.nombre,
	NEW.cargo,
	NEW.sueldo,
	NEW.fecha_ingreso,
	NEW.status,
	NEW.cod_dpto,
	NEW.cod_sede
);

CREATE OR REPLACE RULE empleados_sede2_ins AS
ON INSERT TO empleados_sede2 WHERE NEW.cod_sede='002' DO INSTEAD
INSERT INTO empleados 
VALUES(
	NEW.ficha,
	NEW.nombre,
	NEW.cargo,
	NEW.sueldo,
	NEW.fecha_ingreso,
	NEW.status,
	NEW.cod_dpto,
	NEW.cod_sede
);

CREATE OR REPLACE RULE empleados_sede3_ins AS
ON INSERT TO empleados_sede3 WHERE NEW.cod_sede='003' DO INSTEAD
INSERT INTO empleados 
VALUES(
	NEW.ficha,
	NEW.nombre,
	NEW.cargo,
	NEW.sueldo,
	NEW.fecha_ingreso,
	NEW.status,
	NEW.cod_dpto,
	NEW.cod_sede
);

CREATE OR REPLACE RULE ediciones_sede1_ins AS
ON INSERT TO ediciones_sede1 WHERE NEW.cod_sede='001' DO INSTEAD
INSERT INTO ediciones 
VALUES(
	NEW.cod_curso,
	NEW.lapso,
	NEW.status,
	NEW.instructor,
	NEW.cod_sede
);

CREATE OR REPLACE RULE ediciones_sede2_ins AS
ON INSERT TO ediciones_sede2 WHERE NEW.cod_sede='002' DO INSTEAD
INSERT INTO ediciones 
VALUES(
	NEW.cod_curso,
	NEW.lapso,
	NEW.status,
	NEW.instructor,
	NEW.cod_sede
);

CREATE OR REPLACE RULE ediciones_sede3_ins AS
ON INSERT TO ediciones_sede3 WHERE NEW.cod_sede='003' DO INSTEAD
INSERT INTO ediciones 
VALUES(
	NEW.cod_curso,
	NEW.lapso,
	NEW.status,
	NEW.instructor,
	NEW.cod_sede
);

CREATE OR REPLACE RULE participantes_sede1_ins AS
ON INSERT TO participantes_sede1 DO INSTEAD
INSERT INTO participantes 
VALUES(
	NEW.cod_curso,
    NEW.lapso,
    NEW.nota,
    NEW.ficha
);

CREATE OR REPLACE RULE participantes_sede2_ins AS
ON INSERT TO participantes_sede2 DO INSTEAD
INSERT INTO participantes 
VALUES(
	NEW.cod_curso,
    NEW.lapso,
    NEW.nota,
    NEW.ficha
);

CREATE OR REPLACE RULE participantes_sede3_ins AS
ON INSERT TO participantes_sede3 DO INSTEAD
INSERT INTO participantes 
VALUES(
	NEW.cod_curso,
    NEW.lapso,
    NEW.nota,
    NEW.ficha
);

CREATE OR REPLACE RULE empleados_sede1_del AS
ON DELETE TO empleados_sede1 WHERE OLD.cod_sede='001' DO INSTEAD
DELETE 
FROM empleados 
WHERE ficha = OLD.ficha;

CREATE OR REPLACE RULE empleados_sede2_del AS
ON DELETE TO empleados_sede2 WHERE OLD.cod_sede='002' DO INSTEAD
DELETE 
FROM empleados 
WHERE ficha = OLD.ficha;

CREATE OR REPLACE RULE empleados_sede3_del AS
ON DELETE TO empleados_sede3 WHERE OLD.cod_sede='003' DO INSTEAD
DELETE 
FROM empleados 
WHERE ficha = OLD.ficha;

CREATE OR REPLACE RULE ediciones_sede1_del AS
ON DELETE TO ediciones_sede1 WHERE OLD.cod_sede='001' DO INSTEAD
DELETE 
FROM ediciones 
WHERE cod_curso = OLD.cod_curso AND lapso = OLD.lapso AND cod_sede = OLD.cod_sede;

CREATE OR REPLACE RULE ediciones_sede2_del AS
ON DELETE TO ediciones_sede2 WHERE OLD.cod_sede='002' DO INSTEAD
DELETE 
FROM ediciones 
WHERE cod_curso = OLD.cod_curso AND lapso = OLD.lapso AND cod_sede = OLD.cod_sede;

CREATE OR REPLACE RULE ediciones_sede3_del AS
ON DELETE TO ediciones_sede3 WHERE OLD.cod_sede='003' DO INSTEAD
DELETE 
FROM ediciones 
WHERE cod_curso = OLD.cod_curso AND lapso = OLD.lapso AND cod_sede = OLD.cod_sede;

CREATE OR REPLACE RULE participantes_sede1_del AS
ON DELETE TO participantes_sede1 DO INSTEAD
DELETE
FROM participantes
WHERE ficha = OLD.ficha;

CREATE OR REPLACE RULE participantes_sede2_del AS
ON DELETE TO participantes_sede2 DO INSTEAD
DELETE
FROM participantes
WHERE ficha = OLD.ficha;

CREATE OR REPLACE RULE participantes_sede3_del AS
ON DELETE TO participantes_sede3 DO INSTEAD
DELETE
FROM participantes
WHERE ficha = OLD.ficha;

--Validar q los participantes fragmentados sean de las sedes a la cual pertenecen las tablas

CREATE OR REPLACE FUNCTION validar_participantes_fragmento_sede1()
RETURNS trigger AS $BODY$
DECLARE cod_sede_emp character(3);
BEGIN
	IF (TG_OP = 'UPDATE') THEN
		SELECT cod_sede
		FROM empleados AS emp
		WHERE emp.ficha = OLD.FICHA
		INTO cod_sede_emp;

	ELSIF (TG_OP = 'INSERT') THEN
		SELECT cod_sede
		FROM empleados AS emp
		WHERE emp.ficha = NEW.FICHA
		INTO cod_sede_emp;

	ELSIF (TG_OP = 'DELETE') THEN
		SELECT cod_sede
		FROM empleados AS emp
		WHERE emp.ficha = OLD.FICHA
		INTO cod_sede_emp;

	END IF;

	IF (cod_sede_emp = '001') THEN
		RETURN NEW;
	ELSE
		RAISE EXCEPTION 'No se ha podido escribir partipante porque no es de la misma sede';
	END IF;
END; $BODY$
LANGUAGE plpgsql VOLATILE;

--Asociar trigger con la funcion validar_participantes_fragmento_sede1
CREATE TRIGGER antes_insertarOModificar_participantesSede1
BEFORE INSERT OR UPDATE OR DELETE ON participantes_sede1
FOR EACH ROW
EXECUTE PROCEDURE validar_participantes_fragmento_sede1();


CREATE OR REPLACE FUNCTION validar_participantes_fragmento_sede2()
RETURNS trigger AS $BODY$
DECLARE cod_sede_emp character(3);
BEGIN
	
	IF (TG_OP = 'UPDATE') THEN
		SELECT cod_sede
		FROM empleados AS emp
		WHERE emp.ficha = OLD.FICHA
		INTO cod_sede_emp;

	ELSIF (TG_OP = 'INSERT') THEN
		SELECT cod_sede
		FROM empleados AS emp
		WHERE emp.ficha = NEW.FICHA
		INTO cod_sede_emp;

	ELSIF (TG_OP = 'DELETE') THEN
		SELECT cod_sede
		FROM empleados AS emp
		WHERE emp.ficha = OLD.FICHA
		INTO cod_sede_emp;

	END IF;

	IF (cod_sede_emp = '002') THEN
		RETURN NEW;
	ELSE
		RAISE EXCEPTION 'No se ha podido escribir en partipante porque no es de la misma sede';
	END IF;
END; $BODY$
LANGUAGE plpgsql VOLATILE;

--Asociar trigger con la funcion validar_participantes_fragmento_sede2
CREATE TRIGGER antes_insertarOModificar_participantesSede2
BEFORE INSERT OR UPDATE OR DELETE ON participantes_sede2
FOR EACH ROW
EXECUTE PROCEDURE validar_participantes_fragmento_sede1();

--Validar q los participantes fragmentados sean de las sedes a la cual pertenecen las tablas

CREATE OR REPLACE FUNCTION validar_participantes_fragmento_sede3()
RETURNS trigger AS $BODY$
DECLARE cod_sede_emp character(3);
BEGIN
	
	IF (TG_OP = 'UPDATE') THEN
		SELECT cod_sede
		FROM empleados AS emp
		WHERE emp.ficha = OLD.FICHA
		INTO cod_sede_emp;

	ELSIF (TG_OP = 'INSERT') THEN
		SELECT cod_sede
		FROM empleados AS emp
		WHERE emp.ficha = NEW.FICHA
		INTO cod_sede_emp;

	ELSIF (TG_OP = 'DELETE') THEN
		SELECT cod_sede
		FROM empleados AS emp
		WHERE emp.ficha = OLD.FICHA
		INTO cod_sede_emp;

	END IF;

	IF (cod_sede_emp = '003') THEN
		RETURN NEW;
	ELSE
		RAISE EXCEPTION 'No se ha podido escribir en partipantes porque no es de la misma sede';
	END IF;
END; $BODY$
LANGUAGE plpgsql VOLATILE;

--Asociar trigger con la funcion validar_participantes_fragmento_sede3
CREATE TRIGGER antes_insertarOModificar_participantesSede3
BEFORE INSERT OR UPDATE OR DELETE ON participantes_sede3
FOR EACH ROW
EXECUTE PROCEDURE validar_participantes_fragmento_sede3();

--Asignando permisos a cada grupo para que lea de todas las sedes

GRANT SELECT ON ALL TABLES IN SCHEMA public TO grupo_sede1;
GRANT SELECT ON ALL TABLES IN SCHEMA public TO grupo_sede2;
GRANT SELECT ON ALL TABLES IN SCHEMA public TO grupo_sede3;

-- Asignando permisos a cada grupo para que escriba en su sede

GRANT INSERT, UPDATE, DELETE ON empleados_sede1 TO grupo_sede1;
GRANT INSERT, UPDATE, DELETE ON ediciones_sede1 TO grupo_sede1;
GRANT INSERT, UPDATE, DELETE ON participantes_sede1 TO grupo_sede1;

GRANT INSERT, UPDATE, DELETE ON empleados_sede2 TO grupo_sede2;
GRANT INSERT, UPDATE, DELETE ON ediciones_sede2 TO grupo_sede2;
GRANT INSERT, UPDATE, DELETE ON participantes_sede2 TO grupo_sede2;

GRANT INSERT, UPDATE, DELETE ON empleados_sede3 TO grupo_sede3;
GRANT INSERT, UPDATE, DELETE ON ediciones_sede3 TO grupo_sede3;
GRANT INSERT, UPDATE, DELETE ON participantes_sede3 TO grupo_sede3;

-- Asignando permisospara que el grupo admin modifique la estructura de la BD

ALTER TABLE cursos OWNER TO grupo_admin;
ALTER TABLE departamentos OWNER TO grupo_admin;
ALTER TABLE ediciones OWNER TO grupo_admin;
ALTER TABLE ediciones_sede1 OWNER TO grupo_admin;
ALTER TABLE ediciones_sede2 OWNER TO grupo_admin;
ALTER TABLE ediciones_sede3 OWNER TO grupo_admin;
ALTER TABLE empleados OWNER TO grupo_admin;
ALTER TABLE empleados_sede1 OWNER TO grupo_admin;
ALTER TABLE empleados_sede2 OWNER TO grupo_admin;
ALTER TABLE empleados_sede3 OWNER TO grupo_admin;
ALTER TABLE participantes OWNER TO grupo_admin;
ALTER TABLE participantes_sede1 OWNER TO grupo_admin;
ALTER TABLE participantes_sede2 OWNER TO grupo_admin;
ALTER TABLE participantes_sede3 OWNER TO grupo_admin;
ALTER TABLE prelaciones OWNER TO grupo_admin;
ALTER TABLE sedes OWNER TO grupo_admin;
REVOKE SELECT, INSERT, DELETE, UPDATE ON ALL TABLES IN SCHEMA public FROM grupo_admin;







