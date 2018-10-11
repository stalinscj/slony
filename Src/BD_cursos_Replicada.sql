--
-- PostgreSQL database dump
--

-- Dumped from database version 9.4.1
-- Dumped by pg_dump version 9.4.1
-- Started on 2015-06-19 21:23:20

SET statement_timeout = 0;
SET lock_timeout = 0;
SET client_encoding = 'UTF8';
SET standard_conforming_strings = on;
SET check_function_bodies = false;
SET client_min_messages = warning;

--
-- TOC entry 7 (class 2615 OID 27340)
-- Name: _slony_cursos; Type: SCHEMA; Schema: -; Owner: postgres
--

CREATE SCHEMA _slony_cursos;


ALTER SCHEMA _slony_cursos OWNER TO postgres;

--
-- TOC entry 219 (class 3079 OID 11855)
-- Name: plpgsql; Type: EXTENSION; Schema: -; Owner: 
--

CREATE EXTENSION IF NOT EXISTS plpgsql WITH SCHEMA pg_catalog;


--
-- TOC entry 2633 (class 0 OID 0)
-- Dependencies: 219
-- Name: EXTENSION plpgsql; Type: COMMENT; Schema: -; Owner: 
--

COMMENT ON EXTENSION plpgsql IS 'PL/pgSQL procedural language';


SET search_path = _slony_cursos, pg_catalog;

--
-- TOC entry 877 (class 1247 OID 27556)
-- Name: vactables; Type: TYPE; Schema: _slony_cursos; Owner: postgres
--

CREATE TYPE vactables AS (
	nspname name,
	relname name
);


ALTER TYPE vactables OWNER TO postgres;

--
-- TOC entry 2634 (class 0 OID 0)
-- Dependencies: 877
-- Name: TYPE vactables; Type: COMMENT; Schema: _slony_cursos; Owner: postgres
--

COMMENT ON TYPE vactables IS 'used as return type for SRF function TablesToVacuum';


SET search_path = public, pg_catalog;

--
-- TOC entry 722 (class 1247 OID 26992)
-- Name: area_curso; Type: DOMAIN; Schema: public; Owner: postgres
--

CREATE DOMAIN area_curso AS character varying(11)
	CONSTRAINT area_curso_check CHECK ((((((VALUE)::text = 'RRHH'::text) OR ((VALUE)::text = 'Computación'::text)) OR ((VALUE)::text = 'Gerencia'::text)) OR ((VALUE)::text = 'Producción'::text)));


ALTER DOMAIN area_curso OWNER TO postgres;

--
-- TOC entry 724 (class 1247 OID 26994)
-- Name: nota_participante; Type: DOMAIN; Schema: public; Owner: postgres
--

CREATE DOMAIN nota_participante AS character(1)
	CONSTRAINT nota_participante_check CHECK (((VALUE = 'A'::bpchar) OR (VALUE = 'R'::bpchar)));


ALTER DOMAIN nota_participante OWNER TO postgres;

--
-- TOC entry 726 (class 1247 OID 26996)
-- Name: status_edicion; Type: DOMAIN; Schema: public; Owner: postgres
--

CREATE DOMAIN status_edicion AS character(1)
	CONSTRAINT status_edicion_check CHECK (((((VALUE = 'O'::bpchar) OR (VALUE = 'E'::bpchar)) OR (VALUE = 'S'::bpchar)) OR (VALUE = 'C'::bpchar)));


ALTER DOMAIN status_edicion OWNER TO postgres;

--
-- TOC entry 728 (class 1247 OID 26998)
-- Name: status_empleado; Type: DOMAIN; Schema: public; Owner: postgres
--

CREATE DOMAIN status_empleado AS character(1)
	CONSTRAINT status_empleado_check CHECK (((((VALUE = 'A'::bpchar) OR (VALUE = 'R'::bpchar)) OR (VALUE = 'S'::bpchar)) OR (VALUE = 'J'::bpchar)));


ALTER DOMAIN status_empleado OWNER TO postgres;

SET search_path = _slony_cursos, pg_catalog;

--
-- TOC entry 363 (class 1255 OID 27685)
-- Name: add_empty_table_to_replication(integer, integer, text, text, text, text); Type: FUNCTION; Schema: _slony_cursos; Owner: postgres
--

CREATE FUNCTION add_empty_table_to_replication(p_set_id integer, p_tab_id integer, p_nspname text, p_tabname text, p_idxname text, p_comment text) RETURNS bigint
    LANGUAGE plpgsql
    AS $$
declare

  prec record;
  v_origin int4;
  v_isorigin boolean;
  v_fqname text;
  v_query text;
  v_rows integer;
  v_idxname text;

begin
-- Need to validate that the set exists; the set will tell us if this is the origin
  select set_origin into v_origin from "_slony_cursos".sl_set where set_id = p_set_id;
  if not found then
	raise exception 'add_empty_table_to_replication: set % not found!', p_set_id;
  end if;

-- Need to be aware of whether or not this node is origin for the set
   v_isorigin := ( v_origin = "_slony_cursos".getLocalNodeId('_slony_cursos') );

   v_fqname := '"' || p_nspname || '"."' || p_tabname || '"';
-- Take out a lock on the table
   v_query := 'lock ' || v_fqname || ';';
   execute v_query;

   if v_isorigin then
	-- On the origin, verify that the table is empty, failing if it has any tuples
        v_query := 'select 1 as tuple from ' || v_fqname || ' limit 1;';
	execute v_query into prec;
        GET DIAGNOSTICS v_rows = ROW_COUNT;
	if v_rows = 0 then
		raise notice 'add_empty_table_to_replication: table % empty on origin - OK', v_fqname;
	else
		raise exception 'add_empty_table_to_replication: table % contained tuples on origin node %', v_fqname, v_origin;
	end if;
   else
	-- On other nodes, TRUNCATE the table
        v_query := 'truncate ' || v_fqname || ';';
	execute v_query;
   end if;
-- If p_idxname is NULL, then look up the PK index, and RAISE EXCEPTION if one does not exist
   if p_idxname is NULL then
	select c2.relname into prec from pg_catalog.pg_index i, pg_catalog.pg_class c1, pg_catalog.pg_class c2, pg_catalog.pg_namespace n where i.indrelid = c1.oid and i.indexrelid = c2.oid and c1.relname = p_tabname and i.indisprimary and n.nspname = p_nspname and n.oid = c1.relnamespace;
	if not found then
		raise exception 'add_empty_table_to_replication: table % has no primary key and no candidate specified!', v_fqname;
	else
		v_idxname := prec.relname;
	end if;
   else
	v_idxname := p_idxname;
   end if;
   return "_slony_cursos".setAddTable_int(p_set_id, p_tab_id, v_fqname, v_idxname, p_comment);
end
$$;


ALTER FUNCTION _slony_cursos.add_empty_table_to_replication(p_set_id integer, p_tab_id integer, p_nspname text, p_tabname text, p_idxname text, p_comment text) OWNER TO postgres;

--
-- TOC entry 2635 (class 0 OID 0)
-- Dependencies: 363
-- Name: FUNCTION add_empty_table_to_replication(p_set_id integer, p_tab_id integer, p_nspname text, p_tabname text, p_idxname text, p_comment text); Type: COMMENT; Schema: _slony_cursos; Owner: postgres
--

COMMENT ON FUNCTION add_empty_table_to_replication(p_set_id integer, p_tab_id integer, p_nspname text, p_tabname text, p_idxname text, p_comment text) IS 'Verify that a table is empty, and add it to replication.  
tab_idxname is optional - if NULL, then we use the primary key.

Note that this function is to be run within an EXECUTE SCRIPT script,
so it runs at the right place in the transaction stream on all
nodes.';


--
-- TOC entry 356 (class 1255 OID 27671)
-- Name: add_missing_table_field(text, text, text, text); Type: FUNCTION; Schema: _slony_cursos; Owner: postgres
--

CREATE FUNCTION add_missing_table_field(p_namespace text, p_table text, p_field text, p_type text) RETURNS boolean
    LANGUAGE plpgsql
    AS $$
DECLARE
  v_row       record;
  v_query     text;
BEGIN
  if not "_slony_cursos".check_table_field_exists(p_namespace, p_table, p_field) then
    raise notice 'Upgrade table %.% - add field %', p_namespace, p_table, p_field;
    v_query := 'alter table ' || p_namespace || '.' || p_table || ' add column ';
    v_query := v_query || p_field || ' ' || p_type || ';';
    execute v_query;
    return 't';
  else
    return 'f';
  end if;
END;$$;


ALTER FUNCTION _slony_cursos.add_missing_table_field(p_namespace text, p_table text, p_field text, p_type text) OWNER TO postgres;

--
-- TOC entry 2636 (class 0 OID 0)
-- Dependencies: 356
-- Name: FUNCTION add_missing_table_field(p_namespace text, p_table text, p_field text, p_type text); Type: COMMENT; Schema: _slony_cursos; Owner: postgres
--

COMMENT ON FUNCTION add_missing_table_field(p_namespace text, p_table text, p_field text, p_type text) IS 'Add a column of a given type to a table if it is missing';


--
-- TOC entry 354 (class 1255 OID 27669)
-- Name: addpartiallogindices(); Type: FUNCTION; Schema: _slony_cursos; Owner: postgres
--

CREATE FUNCTION addpartiallogindices() RETURNS integer
    LANGUAGE plpgsql
    AS $$
DECLARE
	v_current_status	int4;
	v_log			int4;
	v_dummy		record;
	v_dummy2	record;
	idef 		text;
	v_count		int4;
        v_iname         text;
	v_ilen int4;
	v_maxlen int4;
BEGIN
	v_count := 0;
	select last_value into v_current_status from "_slony_cursos".sl_log_status;

	-- If status is 2 or 3 --> in process of cleanup --> unsafe to create indices
	if v_current_status in (2, 3) then
		return 0;
	end if;

	if v_current_status = 0 then   -- Which log should get indices?
		v_log := 2;
	else
		v_log := 1;
	end if;
--                                       PartInd_test_db_sl_log_2-node-1
	-- Add missing indices...
	for v_dummy in select distinct set_origin from "_slony_cursos".sl_set loop
            v_iname := 'PartInd_slony_cursos_sl_log_' || v_log::text || '-node-' 
			|| v_dummy.set_origin::text;
	   -- raise notice 'Consider adding partial index % on sl_log_%', v_iname, v_log;
	   -- raise notice 'schema: [_slony_cursos] tablename:[sl_log_%]', v_log;
            select * into v_dummy2 from pg_catalog.pg_indexes where tablename = 'sl_log_' || v_log::text and  indexname = v_iname;
            if not found then
		-- raise notice 'index was not found - add it!';
        v_iname := 'PartInd_slony_cursos_sl_log_' || v_log::text || '-node-' || v_dummy.set_origin::text;
		v_ilen := pg_catalog.length(v_iname);
		v_maxlen := pg_catalog.current_setting('max_identifier_length'::text)::int4;
                if v_ilen > v_maxlen then
		   raise exception 'Length of proposed index name [%] > max_identifier_length [%] - cluster name probably too long', v_ilen, v_maxlen;
		end if;

		idef := 'create index "' || v_iname || 
                        '" on "_slony_cursos".sl_log_' || v_log::text || ' USING btree(log_txid) where (log_origin = ' || v_dummy.set_origin::text || ');';
		execute idef;
		v_count := v_count + 1;
            else
                -- raise notice 'Index % already present - skipping', v_iname;
            end if;
	end loop;

	-- Remove unneeded indices...
	for v_dummy in select indexname from pg_catalog.pg_indexes i where i.tablename = 'sl_log_' || v_log::text and
                       i.indexname like ('PartInd_slony_cursos_sl_log_' || v_log::text || '-node-%') and
                       not exists (select 1 from "_slony_cursos".sl_set where
				i.indexname = 'PartInd_slony_cursos_sl_log_' || v_log::text || '-node-' || set_origin::text)
	loop
		-- raise notice 'Dropping obsolete index %d', v_dummy.indexname;
		idef := 'drop index "_slony_cursos"."' || v_dummy.indexname || '";';
		execute idef;
		v_count := v_count - 1;
	end loop;
	return v_count;
END
$$;


ALTER FUNCTION _slony_cursos.addpartiallogindices() OWNER TO postgres;

--
-- TOC entry 2637 (class 0 OID 0)
-- Dependencies: 354
-- Name: FUNCTION addpartiallogindices(); Type: COMMENT; Schema: _slony_cursos; Owner: postgres
--

COMMENT ON FUNCTION addpartiallogindices() IS 'Add partial indexes, if possible, to the unused sl_log_? table for
all origin nodes, and drop any that are no longer needed.

This function presently gets run any time set origins are manipulated
(FAILOVER, STORE SET, MOVE SET, DROP SET), as well as each time the
system switches between sl_log_1 and sl_log_2.';


--
-- TOC entry 334 (class 1255 OID 27552)
-- Name: agg_text_sum(text, text); Type: FUNCTION; Schema: _slony_cursos; Owner: postgres
--

CREATE FUNCTION agg_text_sum(txt_before text, txt_new text) RETURNS text
    LANGUAGE plpgsql
    AS $$
DECLARE
  c_delim text;
BEGIN
    c_delim = ',';
	IF (txt_before IS NULL or txt_before='') THEN
	   RETURN txt_new;
	END IF;
	RETURN txt_before || c_delim || txt_new;
END;
$$;


ALTER FUNCTION _slony_cursos.agg_text_sum(txt_before text, txt_new text) OWNER TO postgres;

--
-- TOC entry 2638 (class 0 OID 0)
-- Dependencies: 334
-- Name: FUNCTION agg_text_sum(txt_before text, txt_new text); Type: COMMENT; Schema: _slony_cursos; Owner: postgres
--

COMMENT ON FUNCTION agg_text_sum(txt_before text, txt_new text) IS 'An accumulator function used by the slony string_agg function to
aggregate rows into a string';


--
-- TOC entry 333 (class 1255 OID 27649)
-- Name: altertableaddtriggers(integer); Type: FUNCTION; Schema: _slony_cursos; Owner: postgres
--

CREATE FUNCTION altertableaddtriggers(p_tab_id integer) RETURNS integer
    LANGUAGE plpgsql
    AS $$
declare
	v_no_id				int4;
	v_tab_row			record;
	v_tab_fqname		text;
	v_tab_attkind		text;
	v_n					int4;
	v_trec	record;
	v_tgbad	boolean;
begin
	-- ----
	-- Grab the central configuration lock
	-- ----
	lock table "_slony_cursos".sl_config_lock;

	-- ----
	-- Get our local node ID
	-- ----
	v_no_id := "_slony_cursos".getLocalNodeId('_slony_cursos');

	-- ----
	-- Get the sl_table row and the current origin of the table. 
	-- ----
	select T.tab_reloid, T.tab_set, T.tab_idxname, 
			S.set_origin, PGX.indexrelid,
			"_slony_cursos".slon_quote_brute(PGN.nspname) || '.' ||
			"_slony_cursos".slon_quote_brute(PGC.relname) as tab_fqname
			into v_tab_row
			from "_slony_cursos".sl_table T, "_slony_cursos".sl_set S,
				"pg_catalog".pg_class PGC, "pg_catalog".pg_namespace PGN,
				"pg_catalog".pg_index PGX, "pg_catalog".pg_class PGXC
			where T.tab_id = p_tab_id
				and T.tab_set = S.set_id
				and T.tab_reloid = PGC.oid
				and PGC.relnamespace = PGN.oid
				and PGX.indrelid = T.tab_reloid
				and PGX.indexrelid = PGXC.oid
				and PGXC.relname = T.tab_idxname
				for update;
	if not found then
		raise exception 'Slony-I: alterTableAddTriggers(): Table with id % not found', p_tab_id;
	end if;
	v_tab_fqname = v_tab_row.tab_fqname;

	v_tab_attkind := "_slony_cursos".determineAttKindUnique(v_tab_row.tab_fqname, 
						v_tab_row.tab_idxname);

	execute 'lock table ' || v_tab_fqname || ' in access exclusive mode';

	-- ----
	-- Create the log and the deny access triggers
	-- ----
	execute 'create trigger "_slony_cursos_logtrigger"' || 
			' after insert or update or delete on ' ||
			v_tab_fqname || ' for each row execute procedure "_slony_cursos".logTrigger (' ||
                               pg_catalog.quote_literal('_slony_cursos') || ',' || 
				pg_catalog.quote_literal(p_tab_id::text) || ',' || 
				pg_catalog.quote_literal(v_tab_attkind) || ');';

	execute 'create trigger "_slony_cursos_denyaccess" ' || 
			'before insert or update or delete on ' ||
			v_tab_fqname || ' for each row execute procedure ' ||
			'"_slony_cursos".denyAccess (' || pg_catalog.quote_literal('_slony_cursos') || ');';

	perform "_slony_cursos".alterTableAddTruncateTrigger(v_tab_fqname, p_tab_id);

	perform "_slony_cursos".alterTableConfigureTriggers (p_tab_id);
	return p_tab_id;
end;
$$;


ALTER FUNCTION _slony_cursos.altertableaddtriggers(p_tab_id integer) OWNER TO postgres;

--
-- TOC entry 2639 (class 0 OID 0)
-- Dependencies: 333
-- Name: FUNCTION altertableaddtriggers(p_tab_id integer); Type: COMMENT; Schema: _slony_cursos; Owner: postgres
--

COMMENT ON FUNCTION altertableaddtriggers(p_tab_id integer) IS 'alterTableAddTriggers(tab_id)

Adds the log and deny access triggers to a replicated table.';


--
-- TOC entry 379 (class 1255 OID 27701)
-- Name: altertableaddtruncatetrigger(text, integer); Type: FUNCTION; Schema: _slony_cursos; Owner: postgres
--

CREATE FUNCTION altertableaddtruncatetrigger(i_fqtable text, i_tabid integer) RETURNS integer
    LANGUAGE plpgsql
    AS $$
begin
		execute 'create trigger "_slony_cursos_truncatetrigger" ' ||
				' before truncate on ' || i_fqtable || ' for each statement execute procedure ' ||
				'"_slony_cursos".log_truncate(' || i_tabid || ');';
		execute 'create trigger "_slony_cursos_truncatedeny" ' ||
				' before truncate on ' || i_fqtable || ' for each statement execute procedure ' ||
				'"_slony_cursos".deny_truncate();';
		return 1;
end
$$;


ALTER FUNCTION _slony_cursos.altertableaddtruncatetrigger(i_fqtable text, i_tabid integer) OWNER TO postgres;

--
-- TOC entry 2640 (class 0 OID 0)
-- Dependencies: 379
-- Name: FUNCTION altertableaddtruncatetrigger(i_fqtable text, i_tabid integer); Type: COMMENT; Schema: _slony_cursos; Owner: postgres
--

COMMENT ON FUNCTION altertableaddtruncatetrigger(i_fqtable text, i_tabid integer) IS 'function to add TRUNCATE TRIGGER';


--
-- TOC entry 336 (class 1255 OID 27651)
-- Name: altertableconfiguretriggers(integer); Type: FUNCTION; Schema: _slony_cursos; Owner: postgres
--

CREATE FUNCTION altertableconfiguretriggers(p_tab_id integer) RETURNS integer
    LANGUAGE plpgsql
    AS $$
declare
	v_no_id				int4;
	v_tab_row			record;
	v_tab_fqname		text;
	v_n					int4;
begin
	-- ----
	-- Grab the central configuration lock
	-- ----
	lock table "_slony_cursos".sl_config_lock;

	-- ----
	-- Get our local node ID
	-- ----
	v_no_id := "_slony_cursos".getLocalNodeId('_slony_cursos');

	-- ----
	-- Get the sl_table row and the current tables origin.
	-- ----
	select T.tab_reloid, T.tab_set,
			S.set_origin, PGX.indexrelid,
			"_slony_cursos".slon_quote_brute(PGN.nspname) || '.' ||
			"_slony_cursos".slon_quote_brute(PGC.relname) as tab_fqname
			into v_tab_row
			from "_slony_cursos".sl_table T, "_slony_cursos".sl_set S,
				"pg_catalog".pg_class PGC, "pg_catalog".pg_namespace PGN,
				"pg_catalog".pg_index PGX, "pg_catalog".pg_class PGXC
			where T.tab_id = p_tab_id
				and T.tab_set = S.set_id
				and T.tab_reloid = PGC.oid
				and PGC.relnamespace = PGN.oid
				and PGX.indrelid = T.tab_reloid
				and PGX.indexrelid = PGXC.oid
				and PGXC.relname = T.tab_idxname
				for update;
	if not found then
		raise exception 'Slony-I: alterTableConfigureTriggers(): Table with id % not found', p_tab_id;
	end if;
	v_tab_fqname = v_tab_row.tab_fqname;

	-- ----
	-- Configuration depends on the origin of the table
	-- ----
	if v_tab_row.set_origin = v_no_id then
		-- ----
		-- On the origin the log trigger is configured like a default
		-- user trigger and the deny access trigger is disabled.
		-- ----
		execute 'alter table ' || v_tab_fqname ||
				' enable trigger "_slony_cursos_logtrigger"';
		execute 'alter table ' || v_tab_fqname ||
				' disable trigger "_slony_cursos_denyaccess"';
        perform "_slony_cursos".alterTableConfigureTruncateTrigger(v_tab_fqname,
				'enable', 'disable');
	else
		-- ----
		-- On a replica the log trigger is disabled and the
		-- deny access trigger fires in origin session role.
		-- ----
		execute 'alter table ' || v_tab_fqname ||
				' disable trigger "_slony_cursos_logtrigger"';
		execute 'alter table ' || v_tab_fqname ||
				' enable trigger "_slony_cursos_denyaccess"';
        perform "_slony_cursos".alterTableConfigureTruncateTrigger(v_tab_fqname,
				'disable', 'enable');

	end if;

	return p_tab_id;
end;
$$;


ALTER FUNCTION _slony_cursos.altertableconfiguretriggers(p_tab_id integer) OWNER TO postgres;

--
-- TOC entry 2641 (class 0 OID 0)
-- Dependencies: 336
-- Name: FUNCTION altertableconfiguretriggers(p_tab_id integer); Type: COMMENT; Schema: _slony_cursos; Owner: postgres
--

COMMENT ON FUNCTION altertableconfiguretriggers(p_tab_id integer) IS 'alterTableConfigureTriggers (tab_id)

Set the enable/disable configuration for the replication triggers
according to the origin of the set.';


--
-- TOC entry 381 (class 1255 OID 27703)
-- Name: altertableconfiguretruncatetrigger(text, text, text); Type: FUNCTION; Schema: _slony_cursos; Owner: postgres
--

CREATE FUNCTION altertableconfiguretruncatetrigger(i_fqname text, i_log_stat text, i_deny_stat text) RETURNS integer
    LANGUAGE plpgsql
    AS $$
begin
		execute 'alter table ' || i_fqname || ' ' || i_log_stat ||
				' trigger "_slony_cursos_truncatetrigger";';
		execute 'alter table ' || i_fqname || ' ' || i_deny_stat ||
				' trigger "_slony_cursos_truncatedeny";';
		return 1;
end $$;


ALTER FUNCTION _slony_cursos.altertableconfiguretruncatetrigger(i_fqname text, i_log_stat text, i_deny_stat text) OWNER TO postgres;

--
-- TOC entry 2642 (class 0 OID 0)
-- Dependencies: 381
-- Name: FUNCTION altertableconfiguretruncatetrigger(i_fqname text, i_log_stat text, i_deny_stat text); Type: COMMENT; Schema: _slony_cursos; Owner: postgres
--

COMMENT ON FUNCTION altertableconfiguretruncatetrigger(i_fqname text, i_log_stat text, i_deny_stat text) IS 'Configure the truncate triggers according to origin status.';


--
-- TOC entry 335 (class 1255 OID 27650)
-- Name: altertabledroptriggers(integer); Type: FUNCTION; Schema: _slony_cursos; Owner: postgres
--

CREATE FUNCTION altertabledroptriggers(p_tab_id integer) RETURNS integer
    LANGUAGE plpgsql
    AS $$
declare
	v_no_id				int4;
	v_tab_row			record;
	v_tab_fqname		text;
	v_n					int4;
begin
	-- ----
	-- Grab the central configuration lock
	-- ----
	lock table "_slony_cursos".sl_config_lock;

	-- ----
	-- Get our local node ID
	-- ----
	v_no_id := "_slony_cursos".getLocalNodeId('_slony_cursos');

	-- ----
	-- Get the sl_table row and the current tables origin.
	-- ----
	select T.tab_reloid, T.tab_set,
			S.set_origin, PGX.indexrelid,
			"_slony_cursos".slon_quote_brute(PGN.nspname) || '.' ||
			"_slony_cursos".slon_quote_brute(PGC.relname) as tab_fqname
			into v_tab_row
			from "_slony_cursos".sl_table T, "_slony_cursos".sl_set S,
				"pg_catalog".pg_class PGC, "pg_catalog".pg_namespace PGN,
				"pg_catalog".pg_index PGX, "pg_catalog".pg_class PGXC
			where T.tab_id = p_tab_id
				and T.tab_set = S.set_id
				and T.tab_reloid = PGC.oid
				and PGC.relnamespace = PGN.oid
				and PGX.indrelid = T.tab_reloid
				and PGX.indexrelid = PGXC.oid
				and PGXC.relname = T.tab_idxname
				for update;
	if not found then
		raise exception 'Slony-I: alterTableDropTriggers(): Table with id % not found', p_tab_id;
	end if;
	v_tab_fqname = v_tab_row.tab_fqname;

	execute 'lock table ' || v_tab_fqname || ' in access exclusive mode';

	-- ----
	-- Drop both triggers
	-- ----
	execute 'drop trigger "_slony_cursos_logtrigger" on ' || 
			v_tab_fqname;

	execute 'drop trigger "_slony_cursos_denyaccess" on ' || 
			v_tab_fqname;
				
	perform "_slony_cursos".alterTableDropTruncateTrigger(v_tab_fqname, p_tab_id);

	return p_tab_id;
end;
$$;


ALTER FUNCTION _slony_cursos.altertabledroptriggers(p_tab_id integer) OWNER TO postgres;

--
-- TOC entry 2643 (class 0 OID 0)
-- Dependencies: 335
-- Name: FUNCTION altertabledroptriggers(p_tab_id integer); Type: COMMENT; Schema: _slony_cursos; Owner: postgres
--

COMMENT ON FUNCTION altertabledroptriggers(p_tab_id integer) IS 'alterTableDropTriggers (tab_id)

Remove the log and deny access triggers from a table.';


--
-- TOC entry 380 (class 1255 OID 27702)
-- Name: altertabledroptruncatetrigger(text, integer); Type: FUNCTION; Schema: _slony_cursos; Owner: postgres
--

CREATE FUNCTION altertabledroptruncatetrigger(i_fqtable text, i_tabid integer) RETURNS integer
    LANGUAGE plpgsql
    AS $$
begin
		execute 'drop trigger "_slony_cursos_truncatetrigger" ' ||
				' on ' || i_fqtable || ';';
		execute 'drop trigger "_slony_cursos_truncatedeny" ' ||
				' on ' || i_fqtable || ';';
		return 1;
end
$$;


ALTER FUNCTION _slony_cursos.altertabledroptruncatetrigger(i_fqtable text, i_tabid integer) OWNER TO postgres;

--
-- TOC entry 2644 (class 0 OID 0)
-- Dependencies: 380
-- Name: FUNCTION altertabledroptruncatetrigger(i_fqtable text, i_tabid integer); Type: COMMENT; Schema: _slony_cursos; Owner: postgres
--

COMMENT ON FUNCTION altertabledroptruncatetrigger(i_fqtable text, i_tabid integer) IS 'function to drop TRUNCATE TRIGGER';


--
-- TOC entry 355 (class 1255 OID 27670)
-- Name: check_table_field_exists(text, text, text); Type: FUNCTION; Schema: _slony_cursos; Owner: postgres
--

CREATE FUNCTION check_table_field_exists(p_namespace text, p_table text, p_field text) RETURNS boolean
    LANGUAGE plpgsql
    AS $$
BEGIN
	return exists (
			select 1 from "information_schema".columns
				where table_schema = p_namespace
				and table_name = p_table
				and column_name = p_field
		);
END;$$;


ALTER FUNCTION _slony_cursos.check_table_field_exists(p_namespace text, p_table text, p_field text) OWNER TO postgres;

--
-- TOC entry 2645 (class 0 OID 0)
-- Dependencies: 355
-- Name: FUNCTION check_table_field_exists(p_namespace text, p_table text, p_field text); Type: COMMENT; Schema: _slony_cursos; Owner: postgres
--

COMMENT ON FUNCTION check_table_field_exists(p_namespace text, p_table text, p_field text) IS 'Check if a table has a specific attribute';


--
-- TOC entry 358 (class 1255 OID 27674)
-- Name: check_unconfirmed_log(); Type: FUNCTION; Schema: _slony_cursos; Owner: postgres
--

CREATE FUNCTION check_unconfirmed_log() RETURNS boolean
    LANGUAGE plpgsql
    AS $$
declare
	v_rc		bool = false;
	v_error		bool = false;
	v_origin	integer;
	v_allconf	bigint;
	v_allsnap	txid_snapshot;
	v_count		bigint;
begin
	--
	-- Loop over all nodes that are the origin of at least one set
	--
	for v_origin in select distinct set_origin as no_id
			from "_slony_cursos".sl_set loop
		--
		-- Per origin determine which is the highest event seqno
		-- that is confirmed by all subscribers to any of the
		-- origins sets.
		--
		select into v_allconf min(max_seqno) from (
				select con_received, max(con_seqno) as max_seqno
					from "_slony_cursos".sl_confirm
					where con_origin = v_origin
					and con_received in (
						select distinct sub_receiver
							from "_slony_cursos".sl_set as SET,
								"_slony_cursos".sl_subscribe as SUB
							where SET.set_id = SUB.sub_set
							and SET.set_origin = v_origin
						)
					group by con_received
			) as maxconfirmed;
		if not found then
			raise NOTICE 'check_unconfirmed_log(): cannot determine highest ev_seqno for node % confirmed by all subscribers', v_origin;
			v_error = true;
			continue;
		end if;

		--
		-- Get the txid snapshot that corresponds with that event
		--
		select into v_allsnap ev_snapshot
			from "_slony_cursos".sl_event
			where ev_origin = v_origin
			and ev_seqno = v_allconf;
		if not found then
			raise NOTICE 'check_unconfirmed_log(): cannot find event %,% in sl_event', v_origin, v_allconf;
			v_error = true;
			continue;
		end if;

		--
		-- Count the number of log rows that appeard after that event.
		--
		select into v_count count(*) from (
			select 1 from "_slony_cursos".sl_log_1
				where log_origin = v_origin
				and log_txid >= "pg_catalog".txid_snapshot_xmax(v_allsnap)
			union all
			select 1 from "_slony_cursos".sl_log_1
				where log_origin = v_origin
				and log_txid in (
					select * from "pg_catalog".txid_snapshot_xip(v_allsnap)
				)
			union all
			select 1 from "_slony_cursos".sl_log_2
				where log_origin = v_origin
				and log_txid >= "pg_catalog".txid_snapshot_xmax(v_allsnap)
			union all
			select 1 from "_slony_cursos".sl_log_2
				where log_origin = v_origin
				and log_txid in (
					select * from "pg_catalog".txid_snapshot_xip(v_allsnap)
				)
		) as cnt;

		if v_count > 0 then
			raise NOTICE 'check_unconfirmed_log(): origin % has % log rows that have not propagated to all subscribers yet', v_origin, v_count;
			v_rc = true;
		end if;
	end loop;

	if v_error then
		raise EXCEPTION 'check_unconfirmed_log(): aborting due to previous inconsistency';
	end if;

	return v_rc;
end;
$$;


ALTER FUNCTION _slony_cursos.check_unconfirmed_log() OWNER TO postgres;

--
-- TOC entry 261 (class 1255 OID 27574)
-- Name: checkmoduleversion(); Type: FUNCTION; Schema: _slony_cursos; Owner: postgres
--

CREATE FUNCTION checkmoduleversion() RETURNS text
    LANGUAGE plpgsql
    AS $$
declare
  moduleversion	text;
begin
  select into moduleversion "_slony_cursos".getModuleVersion();
  if moduleversion <> '2.2.4' then
      raise exception 'Slonik version: 2.2.4 != Slony-I version in PG build %',
             moduleversion;
  end if;
  return null;
end;$$;


ALTER FUNCTION _slony_cursos.checkmoduleversion() OWNER TO postgres;

--
-- TOC entry 2646 (class 0 OID 0)
-- Dependencies: 261
-- Name: FUNCTION checkmoduleversion(); Type: COMMENT; Schema: _slony_cursos; Owner: postgres
--

COMMENT ON FUNCTION checkmoduleversion() IS 'Inline test function that verifies that slonik request for STORE
NODE/INIT CLUSTER is being run against a conformant set of
schema/functions.';


--
-- TOC entry 345 (class 1255 OID 27660)
-- Name: cleanupevent(interval); Type: FUNCTION; Schema: _slony_cursos; Owner: postgres
--

CREATE FUNCTION cleanupevent(p_interval interval) RETURNS integer
    LANGUAGE plpgsql
    AS $$
declare
	v_max_row	record;
	v_min_row	record;
	v_max_sync	int8;
	v_origin	int8;
	v_seqno		int8;
	v_xmin		bigint;
	v_rc            int8;
begin
	-- ----
	-- First remove all confirmations where origin/receiver no longer exist
	-- ----
	delete from "_slony_cursos".sl_confirm
				where con_origin not in (select no_id from "_slony_cursos".sl_node);
	delete from "_slony_cursos".sl_confirm
				where con_received not in (select no_id from "_slony_cursos".sl_node);
	-- ----
	-- Next remove all but the oldest confirm row per origin,receiver pair.
	-- Ignore confirmations that are younger than 10 minutes. We currently
	-- have an not confirmed suspicion that a possibly lost transaction due
	-- to a server crash might have been visible to another session, and
	-- that this led to log data that is needed again got removed.
	-- ----
	for v_max_row in select con_origin, con_received, max(con_seqno) as con_seqno
				from "_slony_cursos".sl_confirm
				where con_timestamp < (CURRENT_TIMESTAMP - p_interval)
				group by con_origin, con_received
	loop
		delete from "_slony_cursos".sl_confirm
				where con_origin = v_max_row.con_origin
				and con_received = v_max_row.con_received
				and con_seqno < v_max_row.con_seqno;
	end loop;

	-- ----
	-- Then remove all events that are confirmed by all nodes in the
	-- whole cluster up to the last SYNC
	-- ----
	for v_min_row in select con_origin, min(con_seqno) as con_seqno
				from "_slony_cursos".sl_confirm
				group by con_origin
	loop
		select coalesce(max(ev_seqno), 0) into v_max_sync
				from "_slony_cursos".sl_event
				where ev_origin = v_min_row.con_origin
				and ev_seqno <= v_min_row.con_seqno
				and ev_type = 'SYNC';
		if v_max_sync > 0 then
			delete from "_slony_cursos".sl_event
					where ev_origin = v_min_row.con_origin
					and ev_seqno < v_max_sync;
		end if;
	end loop;

	-- ----
	-- If cluster has only one node, then remove all events up to
	-- the last SYNC - Bug #1538
        -- http://gborg.postgresql.org/project/slony1/bugs/bugupdate.php?1538
	-- ----

	select * into v_min_row from "_slony_cursos".sl_node where
			no_id <> "_slony_cursos".getLocalNodeId('_slony_cursos') limit 1;
	if not found then
		select ev_origin, ev_seqno into v_min_row from "_slony_cursos".sl_event
		where ev_origin = "_slony_cursos".getLocalNodeId('_slony_cursos')
		order by ev_origin desc, ev_seqno desc limit 1;
		raise notice 'Slony-I: cleanupEvent(): Single node - deleting events < %', v_min_row.ev_seqno;
			delete from "_slony_cursos".sl_event
			where
				ev_origin = v_min_row.ev_origin and
				ev_seqno < v_min_row.ev_seqno;

        end if;

	if exists (select * from "pg_catalog".pg_class c, "pg_catalog".pg_namespace n, "pg_catalog".pg_attribute a where c.relname = 'sl_seqlog' and n.oid = c.relnamespace and a.attrelid = c.oid and a.attname = 'oid') then
                execute 'alter table "_slony_cursos".sl_seqlog set without oids;';
	end if;		
	-- ----
	-- Also remove stale entries from the nodelock table.
	-- ----
	perform "_slony_cursos".cleanupNodelock();

	-- ----
	-- Find the eldest event left, for each origin
	-- ----
    for v_origin, v_seqno, v_xmin in
	select ev_origin, ev_seqno, "pg_catalog".txid_snapshot_xmin(ev_snapshot) from "_slony_cursos".sl_event
          where (ev_origin, ev_seqno) in (select ev_origin, min(ev_seqno) from "_slony_cursos".sl_event where ev_type = 'SYNC' group by ev_origin)
	loop
		delete from "_slony_cursos".sl_seqlog where seql_origin = v_origin and seql_ev_seqno < v_seqno;
		delete from "_slony_cursos".sl_log_script where log_origin = v_origin and log_txid < v_xmin;
    end loop;
	
	v_rc := "_slony_cursos".logswitch_finish();
	if v_rc = 0 then   -- no switch in progress
		perform "_slony_cursos".logswitch_start();
	end if;

	return 0;
end;
$$;


ALTER FUNCTION _slony_cursos.cleanupevent(p_interval interval) OWNER TO postgres;

--
-- TOC entry 2647 (class 0 OID 0)
-- Dependencies: 345
-- Name: FUNCTION cleanupevent(p_interval interval); Type: COMMENT; Schema: _slony_cursos; Owner: postgres
--

COMMENT ON FUNCTION cleanupevent(p_interval interval) IS 'cleaning old data out of sl_confirm, sl_event.  Removes all but the
last sl_confirm row per (origin,receiver), and then removes all events
that are confirmed by all nodes in the whole cluster up to the last
SYNC.';


--
-- TOC entry 279 (class 1255 OID 27593)
-- Name: cleanupnodelock(); Type: FUNCTION; Schema: _slony_cursos; Owner: postgres
--

CREATE FUNCTION cleanupnodelock() RETURNS integer
    LANGUAGE plpgsql
    AS $$
declare
	v_row		record;
begin
	for v_row in select nl_nodeid, nl_conncnt, nl_backendpid
			from "_slony_cursos".sl_nodelock
			for update
	loop
		if "_slony_cursos".killBackend(v_row.nl_backendpid, 'NULL') < 0 then
			raise notice 'Slony-I: cleanup stale sl_nodelock entry for pid=%',
					v_row.nl_backendpid;
			delete from "_slony_cursos".sl_nodelock where
					nl_nodeid = v_row.nl_nodeid and
					nl_conncnt = v_row.nl_conncnt;
		end if;
	end loop;

	return 0;
end;
$$;


ALTER FUNCTION _slony_cursos.cleanupnodelock() OWNER TO postgres;

--
-- TOC entry 2648 (class 0 OID 0)
-- Dependencies: 279
-- Name: FUNCTION cleanupnodelock(); Type: COMMENT; Schema: _slony_cursos; Owner: postgres
--

COMMENT ON FUNCTION cleanupnodelock() IS 'Clean up stale entries when restarting slon';


--
-- TOC entry 297 (class 1255 OID 27612)
-- Name: clonenodefinish(integer, integer); Type: FUNCTION; Schema: _slony_cursos; Owner: postgres
--

CREATE FUNCTION clonenodefinish(p_no_id integer, p_no_provider integer) RETURNS integer
    LANGUAGE plpgsql
    AS $$
declare
	v_row			record;
begin
	-- ----
	-- Grab the central configuration lock
	-- ----
	lock table "_slony_cursos".sl_config_lock;

	perform "pg_catalog".setval('"_slony_cursos".sl_local_node_id', p_no_id);
	perform "_slony_cursos".resetSession();
	for v_row in select sub_set from "_slony_cursos".sl_subscribe
			where sub_receiver = p_no_id
	loop
		perform "_slony_cursos".updateReloid(v_row.sub_set, p_no_id);
	end loop;

	perform "_slony_cursos".RebuildListenEntries();

	delete from "_slony_cursos".sl_confirm
		where con_received = p_no_id;
	insert into "_slony_cursos".sl_confirm
		(con_origin, con_received, con_seqno, con_timestamp)
		select con_origin, p_no_id, con_seqno, con_timestamp
		from "_slony_cursos".sl_confirm
		where con_received = p_no_provider;
	insert into "_slony_cursos".sl_confirm
		(con_origin, con_received, con_seqno, con_timestamp)
		select p_no_provider, p_no_id, 
				(select max(ev_seqno) from "_slony_cursos".sl_event
					where ev_origin = p_no_provider), current_timestamp;

	return 0;
end;
$$;


ALTER FUNCTION _slony_cursos.clonenodefinish(p_no_id integer, p_no_provider integer) OWNER TO postgres;

--
-- TOC entry 2649 (class 0 OID 0)
-- Dependencies: 297
-- Name: FUNCTION clonenodefinish(p_no_id integer, p_no_provider integer); Type: COMMENT; Schema: _slony_cursos; Owner: postgres
--

COMMENT ON FUNCTION clonenodefinish(p_no_id integer, p_no_provider integer) IS 'Internal part of cloneNodePrepare().';


--
-- TOC entry 295 (class 1255 OID 27610)
-- Name: clonenodeprepare(integer, integer, text); Type: FUNCTION; Schema: _slony_cursos; Owner: postgres
--

CREATE FUNCTION clonenodeprepare(p_no_id integer, p_no_provider integer, p_no_comment text) RETURNS bigint
    LANGUAGE plpgsql
    AS $$
begin
	perform "_slony_cursos".cloneNodePrepare_int (p_no_id, p_no_provider, p_no_comment);
	return  "_slony_cursos".createEvent('_slony_cursos', 'CLONE_NODE',
									p_no_id::text, p_no_provider::text,
									p_no_comment::text);
end;
$$;


ALTER FUNCTION _slony_cursos.clonenodeprepare(p_no_id integer, p_no_provider integer, p_no_comment text) OWNER TO postgres;

--
-- TOC entry 2650 (class 0 OID 0)
-- Dependencies: 295
-- Name: FUNCTION clonenodeprepare(p_no_id integer, p_no_provider integer, p_no_comment text); Type: COMMENT; Schema: _slony_cursos; Owner: postgres
--

COMMENT ON FUNCTION clonenodeprepare(p_no_id integer, p_no_provider integer, p_no_comment text) IS 'Prepare for cloning a node.';


--
-- TOC entry 296 (class 1255 OID 27611)
-- Name: clonenodeprepare_int(integer, integer, text); Type: FUNCTION; Schema: _slony_cursos; Owner: postgres
--

CREATE FUNCTION clonenodeprepare_int(p_no_id integer, p_no_provider integer, p_no_comment text) RETURNS integer
    LANGUAGE plpgsql
    AS $$
declare
   v_dummy int4;
begin
	-- ----
	-- Grab the central configuration lock
	-- ----
	lock table "_slony_cursos".sl_config_lock;

	update "_slony_cursos".sl_node set
	       no_active = np.no_active,
	       no_comment = np.no_comment,
	       no_failed = np.no_failed
	       from "_slony_cursos".sl_node np
	       where np.no_id = p_no_provider
	       and sl_node.no_id = p_no_id;
	if not found then
	   insert into "_slony_cursos".sl_node
		(no_id, no_active, no_comment,no_failed)
		select p_no_id, no_active, p_no_comment, no_failed
		from "_slony_cursos".sl_node
		where no_id = p_no_provider;
	end if;

       insert into "_slony_cursos".sl_path
	    (pa_server, pa_client, pa_conninfo, pa_connretry)
	    select pa_server, p_no_id, '<event pending>', pa_connretry
	    from "_slony_cursos".sl_path
	    where pa_client = p_no_provider
	    and (pa_server, p_no_id) not in (select pa_server, pa_client
	    	    from "_slony_cursos".sl_path);

       insert into "_slony_cursos".sl_path
	    (pa_server, pa_client, pa_conninfo, pa_connretry)
	    select p_no_id, pa_client, '<event pending>', pa_connretry
	    from "_slony_cursos".sl_path
	    where pa_server = p_no_provider
	    and (p_no_id, pa_client) not in (select pa_server, pa_client
	    	    from "_slony_cursos".sl_path);

	insert into "_slony_cursos".sl_subscribe
		(sub_set, sub_provider, sub_receiver, sub_forward, sub_active)
		select sub_set, sub_provider, p_no_id, sub_forward, sub_active
		from "_slony_cursos".sl_subscribe
		where sub_receiver = p_no_provider;

	insert into "_slony_cursos".sl_confirm
		(con_origin, con_received, con_seqno, con_timestamp)
		select con_origin, p_no_id, con_seqno, con_timestamp
		from "_slony_cursos".sl_confirm
		where con_received = p_no_provider;

	perform "_slony_cursos".RebuildListenEntries();

	return 0;
end;
$$;


ALTER FUNCTION _slony_cursos.clonenodeprepare_int(p_no_id integer, p_no_provider integer, p_no_comment text) OWNER TO postgres;

--
-- TOC entry 2651 (class 0 OID 0)
-- Dependencies: 296
-- Name: FUNCTION clonenodeprepare_int(p_no_id integer, p_no_provider integer, p_no_comment text); Type: COMMENT; Schema: _slony_cursos; Owner: postgres
--

COMMENT ON FUNCTION clonenodeprepare_int(p_no_id integer, p_no_provider integer, p_no_comment text) IS 'Internal part of cloneNodePrepare().';


--
-- TOC entry 373 (class 1255 OID 27695)
-- Name: component_state(text, integer, integer, integer, text, timestamp with time zone, bigint, text); Type: FUNCTION; Schema: _slony_cursos; Owner: postgres
--

CREATE FUNCTION component_state(i_actor text, i_pid integer, i_node integer, i_conn_pid integer, i_activity text, i_starttime timestamp with time zone, i_event bigint, i_eventtype text) RETURNS integer
    LANGUAGE plpgsql
    AS $$
begin
	-- Trim out old state for this component
	if not exists (select 1 from "_slony_cursos".sl_components where co_actor = i_actor) then
	   insert into "_slony_cursos".sl_components 
             (co_actor, co_pid, co_node, co_connection_pid, co_activity, co_starttime, co_event, co_eventtype)
	   values 
              (i_actor, i_pid, i_node, i_conn_pid, i_activity, i_starttime, i_event, i_eventtype);
	else
	   update "_slony_cursos".sl_components 
              set
                 co_connection_pid = i_conn_pid, co_activity = i_activity, co_starttime = i_starttime, co_event = i_event,
                 co_eventtype = i_eventtype
              where co_actor = i_actor 
	      	    and co_starttime < i_starttime;
	end if;
	return 1;
end $$;


ALTER FUNCTION _slony_cursos.component_state(i_actor text, i_pid integer, i_node integer, i_conn_pid integer, i_activity text, i_starttime timestamp with time zone, i_event bigint, i_eventtype text) OWNER TO postgres;

--
-- TOC entry 2652 (class 0 OID 0)
-- Dependencies: 373
-- Name: FUNCTION component_state(i_actor text, i_pid integer, i_node integer, i_conn_pid integer, i_activity text, i_starttime timestamp with time zone, i_event bigint, i_eventtype text); Type: COMMENT; Schema: _slony_cursos; Owner: postgres
--

COMMENT ON FUNCTION component_state(i_actor text, i_pid integer, i_node integer, i_conn_pid integer, i_activity text, i_starttime timestamp with time zone, i_event bigint, i_eventtype text) IS 'Store state of a Slony component.  Useful for monitoring';


--
-- TOC entry 359 (class 1255 OID 27680)
-- Name: copyfields(integer); Type: FUNCTION; Schema: _slony_cursos; Owner: postgres
--

CREATE FUNCTION copyfields(p_tab_id integer) RETURNS text
    LANGUAGE plpgsql
    AS $$
declare
	result text;
	prefix text;
	prec record;
begin
	result := '';
	prefix := '(';   -- Initially, prefix is the opening paren

	for prec in select "_slony_cursos".slon_quote_input(a.attname) as column from "_slony_cursos".sl_table t, pg_catalog.pg_attribute a where t.tab_id = p_tab_id and t.tab_reloid = a.attrelid and a.attnum > 0 and a.attisdropped = false order by attnum
	loop
		result := result || prefix || prec.column;
		prefix := ',';   -- Subsequently, prepend columns with commas
	end loop;
	result := result || ')';
	return result;
end;
$$;


ALTER FUNCTION _slony_cursos.copyfields(p_tab_id integer) OWNER TO postgres;

--
-- TOC entry 2653 (class 0 OID 0)
-- Dependencies: 359
-- Name: FUNCTION copyfields(p_tab_id integer); Type: COMMENT; Schema: _slony_cursos; Owner: postgres
--

COMMENT ON FUNCTION copyfields(p_tab_id integer) IS 'Return a string consisting of what should be appended to a COPY statement
to specify fields for the passed-in tab_id.  

In PG versions > 7.3, this looks like (field1,field2,...fieldn)';


--
-- TOC entry 244 (class 1255 OID 27557)
-- Name: createevent(name, text); Type: FUNCTION; Schema: _slony_cursos; Owner: postgres
--

CREATE FUNCTION createevent(p_cluster_name name, p_event_type text) RETURNS bigint
    LANGUAGE c
    AS '$libdir/slony1_funcs.2.2.4', '_Slony_I_2_2_4_createEvent';


ALTER FUNCTION _slony_cursos.createevent(p_cluster_name name, p_event_type text) OWNER TO postgres;

--
-- TOC entry 2654 (class 0 OID 0)
-- Dependencies: 244
-- Name: FUNCTION createevent(p_cluster_name name, p_event_type text); Type: COMMENT; Schema: _slony_cursos; Owner: postgres
--

COMMENT ON FUNCTION createevent(p_cluster_name name, p_event_type text) IS 'FUNCTION createEvent (cluster_name, ev_type [, ev_data [...]])

Create an sl_event entry';


--
-- TOC entry 245 (class 1255 OID 27558)
-- Name: createevent(name, text, text); Type: FUNCTION; Schema: _slony_cursos; Owner: postgres
--

CREATE FUNCTION createevent(p_cluster_name name, p_event_type text, ev_data1 text) RETURNS bigint
    LANGUAGE c
    AS '$libdir/slony1_funcs.2.2.4', '_Slony_I_2_2_4_createEvent';


ALTER FUNCTION _slony_cursos.createevent(p_cluster_name name, p_event_type text, ev_data1 text) OWNER TO postgres;

--
-- TOC entry 2655 (class 0 OID 0)
-- Dependencies: 245
-- Name: FUNCTION createevent(p_cluster_name name, p_event_type text, ev_data1 text); Type: COMMENT; Schema: _slony_cursos; Owner: postgres
--

COMMENT ON FUNCTION createevent(p_cluster_name name, p_event_type text, ev_data1 text) IS 'FUNCTION createEvent (cluster_name, ev_type [, ev_data [...]])

Create an sl_event entry';


--
-- TOC entry 246 (class 1255 OID 27559)
-- Name: createevent(name, text, text, text); Type: FUNCTION; Schema: _slony_cursos; Owner: postgres
--

CREATE FUNCTION createevent(p_cluster_name name, p_event_type text, ev_data1 text, ev_data2 text) RETURNS bigint
    LANGUAGE c
    AS '$libdir/slony1_funcs.2.2.4', '_Slony_I_2_2_4_createEvent';


ALTER FUNCTION _slony_cursos.createevent(p_cluster_name name, p_event_type text, ev_data1 text, ev_data2 text) OWNER TO postgres;

--
-- TOC entry 2656 (class 0 OID 0)
-- Dependencies: 246
-- Name: FUNCTION createevent(p_cluster_name name, p_event_type text, ev_data1 text, ev_data2 text); Type: COMMENT; Schema: _slony_cursos; Owner: postgres
--

COMMENT ON FUNCTION createevent(p_cluster_name name, p_event_type text, ev_data1 text, ev_data2 text) IS 'FUNCTION createEvent (cluster_name, ev_type [, ev_data [...]])

Create an sl_event entry';


--
-- TOC entry 247 (class 1255 OID 27560)
-- Name: createevent(name, text, text, text, text); Type: FUNCTION; Schema: _slony_cursos; Owner: postgres
--

CREATE FUNCTION createevent(p_cluster_name name, p_event_type text, ev_data1 text, ev_data2 text, ev_data3 text) RETURNS bigint
    LANGUAGE c
    AS '$libdir/slony1_funcs.2.2.4', '_Slony_I_2_2_4_createEvent';


ALTER FUNCTION _slony_cursos.createevent(p_cluster_name name, p_event_type text, ev_data1 text, ev_data2 text, ev_data3 text) OWNER TO postgres;

--
-- TOC entry 2657 (class 0 OID 0)
-- Dependencies: 247
-- Name: FUNCTION createevent(p_cluster_name name, p_event_type text, ev_data1 text, ev_data2 text, ev_data3 text); Type: COMMENT; Schema: _slony_cursos; Owner: postgres
--

COMMENT ON FUNCTION createevent(p_cluster_name name, p_event_type text, ev_data1 text, ev_data2 text, ev_data3 text) IS 'FUNCTION createEvent (cluster_name, ev_type [, ev_data [...]])

Create an sl_event entry';


--
-- TOC entry 248 (class 1255 OID 27561)
-- Name: createevent(name, text, text, text, text, text); Type: FUNCTION; Schema: _slony_cursos; Owner: postgres
--

CREATE FUNCTION createevent(p_cluster_name name, p_event_type text, ev_data1 text, ev_data2 text, ev_data3 text, ev_data4 text) RETURNS bigint
    LANGUAGE c
    AS '$libdir/slony1_funcs.2.2.4', '_Slony_I_2_2_4_createEvent';


ALTER FUNCTION _slony_cursos.createevent(p_cluster_name name, p_event_type text, ev_data1 text, ev_data2 text, ev_data3 text, ev_data4 text) OWNER TO postgres;

--
-- TOC entry 2658 (class 0 OID 0)
-- Dependencies: 248
-- Name: FUNCTION createevent(p_cluster_name name, p_event_type text, ev_data1 text, ev_data2 text, ev_data3 text, ev_data4 text); Type: COMMENT; Schema: _slony_cursos; Owner: postgres
--

COMMENT ON FUNCTION createevent(p_cluster_name name, p_event_type text, ev_data1 text, ev_data2 text, ev_data3 text, ev_data4 text) IS 'FUNCTION createEvent (cluster_name, ev_type [, ev_data [...]])

Create an sl_event entry';


--
-- TOC entry 249 (class 1255 OID 27562)
-- Name: createevent(name, text, text, text, text, text, text); Type: FUNCTION; Schema: _slony_cursos; Owner: postgres
--

CREATE FUNCTION createevent(p_cluster_name name, p_event_type text, ev_data1 text, ev_data2 text, ev_data3 text, ev_data4 text, ev_data5 text) RETURNS bigint
    LANGUAGE c
    AS '$libdir/slony1_funcs.2.2.4', '_Slony_I_2_2_4_createEvent';


ALTER FUNCTION _slony_cursos.createevent(p_cluster_name name, p_event_type text, ev_data1 text, ev_data2 text, ev_data3 text, ev_data4 text, ev_data5 text) OWNER TO postgres;

--
-- TOC entry 2659 (class 0 OID 0)
-- Dependencies: 249
-- Name: FUNCTION createevent(p_cluster_name name, p_event_type text, ev_data1 text, ev_data2 text, ev_data3 text, ev_data4 text, ev_data5 text); Type: COMMENT; Schema: _slony_cursos; Owner: postgres
--

COMMENT ON FUNCTION createevent(p_cluster_name name, p_event_type text, ev_data1 text, ev_data2 text, ev_data3 text, ev_data4 text, ev_data5 text) IS 'FUNCTION createEvent (cluster_name, ev_type [, ev_data [...]])

Create an sl_event entry';


--
-- TOC entry 250 (class 1255 OID 27563)
-- Name: createevent(name, text, text, text, text, text, text, text); Type: FUNCTION; Schema: _slony_cursos; Owner: postgres
--

CREATE FUNCTION createevent(p_cluster_name name, p_event_type text, ev_data1 text, ev_data2 text, ev_data3 text, ev_data4 text, ev_data5 text, ev_data6 text) RETURNS bigint
    LANGUAGE c
    AS '$libdir/slony1_funcs.2.2.4', '_Slony_I_2_2_4_createEvent';


ALTER FUNCTION _slony_cursos.createevent(p_cluster_name name, p_event_type text, ev_data1 text, ev_data2 text, ev_data3 text, ev_data4 text, ev_data5 text, ev_data6 text) OWNER TO postgres;

--
-- TOC entry 2660 (class 0 OID 0)
-- Dependencies: 250
-- Name: FUNCTION createevent(p_cluster_name name, p_event_type text, ev_data1 text, ev_data2 text, ev_data3 text, ev_data4 text, ev_data5 text, ev_data6 text); Type: COMMENT; Schema: _slony_cursos; Owner: postgres
--

COMMENT ON FUNCTION createevent(p_cluster_name name, p_event_type text, ev_data1 text, ev_data2 text, ev_data3 text, ev_data4 text, ev_data5 text, ev_data6 text) IS 'FUNCTION createEvent (cluster_name, ev_type [, ev_data [...]])

Create an sl_event entry';


--
-- TOC entry 251 (class 1255 OID 27564)
-- Name: createevent(name, text, text, text, text, text, text, text, text); Type: FUNCTION; Schema: _slony_cursos; Owner: postgres
--

CREATE FUNCTION createevent(p_cluster_name name, p_event_type text, ev_data1 text, ev_data2 text, ev_data3 text, ev_data4 text, ev_data5 text, ev_data6 text, ev_data7 text) RETURNS bigint
    LANGUAGE c
    AS '$libdir/slony1_funcs.2.2.4', '_Slony_I_2_2_4_createEvent';


ALTER FUNCTION _slony_cursos.createevent(p_cluster_name name, p_event_type text, ev_data1 text, ev_data2 text, ev_data3 text, ev_data4 text, ev_data5 text, ev_data6 text, ev_data7 text) OWNER TO postgres;

--
-- TOC entry 2661 (class 0 OID 0)
-- Dependencies: 251
-- Name: FUNCTION createevent(p_cluster_name name, p_event_type text, ev_data1 text, ev_data2 text, ev_data3 text, ev_data4 text, ev_data5 text, ev_data6 text, ev_data7 text); Type: COMMENT; Schema: _slony_cursos; Owner: postgres
--

COMMENT ON FUNCTION createevent(p_cluster_name name, p_event_type text, ev_data1 text, ev_data2 text, ev_data3 text, ev_data4 text, ev_data5 text, ev_data6 text, ev_data7 text) IS 'FUNCTION createEvent (cluster_name, ev_type [, ev_data [...]])

Create an sl_event entry';


--
-- TOC entry 252 (class 1255 OID 27565)
-- Name: createevent(name, text, text, text, text, text, text, text, text, text); Type: FUNCTION; Schema: _slony_cursos; Owner: postgres
--

CREATE FUNCTION createevent(p_cluster_name name, p_event_type text, ev_data1 text, ev_data2 text, ev_data3 text, ev_data4 text, ev_data5 text, ev_data6 text, ev_data7 text, ev_data8 text) RETURNS bigint
    LANGUAGE c
    AS '$libdir/slony1_funcs.2.2.4', '_Slony_I_2_2_4_createEvent';


ALTER FUNCTION _slony_cursos.createevent(p_cluster_name name, p_event_type text, ev_data1 text, ev_data2 text, ev_data3 text, ev_data4 text, ev_data5 text, ev_data6 text, ev_data7 text, ev_data8 text) OWNER TO postgres;

--
-- TOC entry 2662 (class 0 OID 0)
-- Dependencies: 252
-- Name: FUNCTION createevent(p_cluster_name name, p_event_type text, ev_data1 text, ev_data2 text, ev_data3 text, ev_data4 text, ev_data5 text, ev_data6 text, ev_data7 text, ev_data8 text); Type: COMMENT; Schema: _slony_cursos; Owner: postgres
--

COMMENT ON FUNCTION createevent(p_cluster_name name, p_event_type text, ev_data1 text, ev_data2 text, ev_data3 text, ev_data4 text, ev_data5 text, ev_data6 text, ev_data7 text, ev_data8 text) IS 'FUNCTION createEvent (cluster_name, ev_type [, ev_data [...]])

Create an sl_event entry';


--
-- TOC entry 330 (class 1255 OID 27646)
-- Name: ddlcapture(text, text); Type: FUNCTION; Schema: _slony_cursos; Owner: postgres
--

CREATE FUNCTION ddlcapture(p_statement text, p_nodes text) RETURNS bigint
    LANGUAGE plpgsql
    AS $$
declare
	c_local_node	integer;
	c_found_origin	boolean;
	c_node			text;
	c_cmdargs		text[];
	c_nodeargs      text;
	c_delim         text;
begin
	c_local_node := "_slony_cursos".getLocalNodeId('_slony_cursos');

	c_cmdargs = array_append('{}'::text[], p_statement);
	c_nodeargs = '';
	if p_nodes is not null then
		c_found_origin := 'f';
		-- p_nodes list needs to consist of a list of nodes that exist
		-- and that include the current node ID
		for c_node in select trim(node) from
				pg_catalog.regexp_split_to_table(p_nodes, ',') as node loop
			if not exists 
					(select 1 from "_slony_cursos".sl_node 
					where no_id = (c_node::integer)) then
				raise exception 'ddlcapture(%,%) - node % does not exist!', 
					p_statement, p_nodes, c_node;
		   end if;

		   if c_local_node = (c_node::integer) then
		   	  c_found_origin := 't';
		   end if;
		   if length(c_nodeargs)>0 then
		   	  c_nodeargs = c_nodeargs ||','|| c_node;
		   else
				c_nodeargs=c_node;
			end if;
	   end loop;

		if not c_found_origin then
			raise exception 
				'ddlcapture(%,%) - origin node % not included in ONLY ON list!',
				p_statement, p_nodes, c_local_node;
       end if;
    end if;
	c_cmdargs = array_append(c_cmdargs,c_nodeargs);
	c_delim=',';
	c_cmdargs = array_append(c_cmdargs, 

           (select "_slony_cursos".string_agg( seq_id::text || c_delim
		   || c_local_node ||
		    c_delim || seq_last_value)
		    FROM (
		       select seq_id,
           	   seq_last_value from "_slony_cursos".sl_seqlastvalue
           	   where seq_origin = c_local_node) as FOO
			where NOT "_slony_cursos".seqtrack(seq_id,seq_last_value) is NULL));
	insert into "_slony_cursos".sl_log_script
			(log_origin, log_txid, log_actionseq, log_cmdtype, log_cmdargs)
		values 
			(c_local_node, pg_catalog.txid_current(), 
			nextval('"_slony_cursos".sl_action_seq'), 'S', c_cmdargs);
	execute p_statement;
	return currval('"_slony_cursos".sl_action_seq');
end;
$$;


ALTER FUNCTION _slony_cursos.ddlcapture(p_statement text, p_nodes text) OWNER TO postgres;

--
-- TOC entry 2663 (class 0 OID 0)
-- Dependencies: 330
-- Name: FUNCTION ddlcapture(p_statement text, p_nodes text); Type: COMMENT; Schema: _slony_cursos; Owner: postgres
--

COMMENT ON FUNCTION ddlcapture(p_statement text, p_nodes text) IS 'Capture an SQL statement (usually DDL) that is to be literally replayed on subscribers';


--
-- TOC entry 331 (class 1255 OID 27647)
-- Name: ddlscript_complete(text); Type: FUNCTION; Schema: _slony_cursos; Owner: postgres
--

CREATE FUNCTION ddlscript_complete(p_nodes text) RETURNS bigint
    LANGUAGE plpgsql
    AS $$
declare
	c_local_node	integer;
	c_found_origin	boolean;
	c_node			text;
	c_cmdargs		text[];
begin
	c_local_node := "_slony_cursos".getLocalNodeId('_slony_cursos');

	c_cmdargs = '{}'::text[];
	if p_nodes is not null then
		c_found_origin := 'f';
		-- p_nodes list needs to consist o a list of nodes that exist
		-- and that include the current node ID
		for c_node in select trim(node) from
				pg_catalog.regexp_split_to_table(p_nodes, ',') as node loop
			if not exists 
					(select 1 from "_slony_cursos".sl_node 
					where no_id = (c_node::integer)) then
				raise exception 'ddlcapture(%,%) - node % does not exist!', 
					p_statement, p_nodes, c_node;
		   end if;

		   if c_local_node = (c_node::integer) then
		   	  c_found_origin := 't';
		   end if;

		   c_cmdargs = array_append(c_cmdargs, c_node);
	   end loop;

		if not c_found_origin then
			raise exception 
				'ddlScript_complete(%) - origin node % not included in ONLY ON list!',
				p_nodes, c_local_node;
       end if;
    end if;

	perform "_slony_cursos".ddlScript_complete_int();

	insert into "_slony_cursos".sl_log_script
			(log_origin, log_txid, log_actionseq, log_cmdtype, log_cmdargs)
		values 
			(c_local_node, pg_catalog.txid_current(), 
			nextval('"_slony_cursos".sl_action_seq'), 's', c_cmdargs);

	return currval('"_slony_cursos".sl_action_seq');
end;
$$;


ALTER FUNCTION _slony_cursos.ddlscript_complete(p_nodes text) OWNER TO postgres;

--
-- TOC entry 2664 (class 0 OID 0)
-- Dependencies: 331
-- Name: FUNCTION ddlscript_complete(p_nodes text); Type: COMMENT; Schema: _slony_cursos; Owner: postgres
--

COMMENT ON FUNCTION ddlscript_complete(p_nodes text) IS 'ddlScript_complete(set_id, script, only_on_node)

After script has run on origin, this fixes up relnames and
log trigger arguments and inserts the "fire ddlScript_complete_int()
log row into sl_log_script.';


--
-- TOC entry 332 (class 1255 OID 27648)
-- Name: ddlscript_complete_int(); Type: FUNCTION; Schema: _slony_cursos; Owner: postgres
--

CREATE FUNCTION ddlscript_complete_int() RETURNS integer
    LANGUAGE plpgsql
    AS $$
begin
	perform "_slony_cursos".updateRelname();
	perform "_slony_cursos".repair_log_triggers(true);
	return 0;
end;
$$;


ALTER FUNCTION _slony_cursos.ddlscript_complete_int() OWNER TO postgres;

--
-- TOC entry 2665 (class 0 OID 0)
-- Dependencies: 332
-- Name: FUNCTION ddlscript_complete_int(); Type: COMMENT; Schema: _slony_cursos; Owner: postgres
--

COMMENT ON FUNCTION ddlscript_complete_int() IS 'ddlScript_complete_int()

Complete processing the DDL_SCRIPT event.';


--
-- TOC entry 262 (class 1255 OID 27575)
-- Name: decode_tgargs(bytea); Type: FUNCTION; Schema: _slony_cursos; Owner: postgres
--

CREATE FUNCTION decode_tgargs(bytea) RETURNS text[]
    LANGUAGE c SECURITY DEFINER
    AS '$libdir/slony1_funcs.2.2.4', '_Slony_I_2_2_4_slon_decode_tgargs';


ALTER FUNCTION _slony_cursos.decode_tgargs(bytea) OWNER TO postgres;

--
-- TOC entry 2666 (class 0 OID 0)
-- Dependencies: 262
-- Name: FUNCTION decode_tgargs(bytea); Type: COMMENT; Schema: _slony_cursos; Owner: postgres
--

COMMENT ON FUNCTION decode_tgargs(bytea) IS 'Translates the contents of pg_trigger.tgargs to an array of text arguments';


--
-- TOC entry 370 (class 1255 OID 27692)
-- Name: deny_truncate(); Type: FUNCTION; Schema: _slony_cursos; Owner: postgres
--

CREATE FUNCTION deny_truncate() RETURNS trigger
    LANGUAGE plpgsql
    AS $$
	begin
		raise exception 'truncation of replicated table forbidden on subscriber node';
    end
$$;


ALTER FUNCTION _slony_cursos.deny_truncate() OWNER TO postgres;

--
-- TOC entry 2668 (class 0 OID 0)
-- Dependencies: 370
-- Name: FUNCTION deny_truncate(); Type: COMMENT; Schema: _slony_cursos; Owner: postgres
--

COMMENT ON FUNCTION deny_truncate() IS 'trigger function run when a replicated table receives a TRUNCATE request';


--
-- TOC entry 253 (class 1255 OID 27566)
-- Name: denyaccess(); Type: FUNCTION; Schema: _slony_cursos; Owner: postgres
--

CREATE FUNCTION denyaccess() RETURNS trigger
    LANGUAGE c SECURITY DEFINER
    AS '$libdir/slony1_funcs.2.2.4', '_Slony_I_2_2_4_denyAccess';


ALTER FUNCTION _slony_cursos.denyaccess() OWNER TO postgres;

--
-- TOC entry 2669 (class 0 OID 0)
-- Dependencies: 253
-- Name: FUNCTION denyaccess(); Type: COMMENT; Schema: _slony_cursos; Owner: postgres
--

COMMENT ON FUNCTION denyaccess() IS 'Trigger function to prevent modifications to a table on a subscriber';


--
-- TOC entry 347 (class 1255 OID 27662)
-- Name: determineattkindunique(text, name); Type: FUNCTION; Schema: _slony_cursos; Owner: postgres
--

CREATE FUNCTION determineattkindunique(p_tab_fqname text, p_idx_name name) RETURNS text
    LANGUAGE plpgsql
    AS $$
declare
	v_tab_fqname_quoted	text default '';
	v_idx_name_quoted	text;
	v_idxrow		record;
	v_attrow		record;
	v_i				integer;
	v_attno			int2;
	v_attkind		text default '';
	v_attfound		bool;
begin
	v_tab_fqname_quoted := "_slony_cursos".slon_quote_input(p_tab_fqname);
	v_idx_name_quoted := "_slony_cursos".slon_quote_brute(p_idx_name);
	--
	-- Ensure that the table exists
	--
	if (select PGC.relname
				from "pg_catalog".pg_class PGC,
					"pg_catalog".pg_namespace PGN
				where "_slony_cursos".slon_quote_brute(PGN.nspname) || '.' ||
					"_slony_cursos".slon_quote_brute(PGC.relname) = v_tab_fqname_quoted
					and PGN.oid = PGC.relnamespace) is null then
		raise exception 'Slony-I: table % not found', v_tab_fqname_quoted;
	end if;

	--
	-- Lookup the tables primary key or the specified unique index
	--
	if p_idx_name isnull then
		raise exception 'Slony-I: index name must be specified';
	else
		select PGXC.relname, PGX.indexrelid, PGX.indkey
				into v_idxrow
				from "pg_catalog".pg_class PGC,
					"pg_catalog".pg_namespace PGN,
					"pg_catalog".pg_index PGX,
					"pg_catalog".pg_class PGXC
				where "_slony_cursos".slon_quote_brute(PGN.nspname) || '.' ||
					"_slony_cursos".slon_quote_brute(PGC.relname) = v_tab_fqname_quoted
					and PGN.oid = PGC.relnamespace
					and PGX.indrelid = PGC.oid
					and PGX.indexrelid = PGXC.oid
					and PGX.indisunique
					and "_slony_cursos".slon_quote_brute(PGXC.relname) = v_idx_name_quoted;
		if not found then
			raise exception 'Slony-I: table % has no unique index %',
					v_tab_fqname_quoted, v_idx_name_quoted;
		end if;
	end if;

	--
	-- Loop over the tables attributes and check if they are
	-- index attributes. If so, add a "k" to the return value,
	-- otherwise add a "v".
	--
	for v_attrow in select PGA.attnum, PGA.attname
			from "pg_catalog".pg_class PGC,
			    "pg_catalog".pg_namespace PGN,
				"pg_catalog".pg_attribute PGA
			where "_slony_cursos".slon_quote_brute(PGN.nspname) || '.' ||
			    "_slony_cursos".slon_quote_brute(PGC.relname) = v_tab_fqname_quoted
				and PGN.oid = PGC.relnamespace
				and PGA.attrelid = PGC.oid
				and not PGA.attisdropped
				and PGA.attnum > 0
			order by attnum
	loop
		v_attfound = 'f';

		v_i := 0;
		loop
			select indkey[v_i] into v_attno from "pg_catalog".pg_index
					where indexrelid = v_idxrow.indexrelid;
			if v_attno isnull or v_attno = 0 then
				exit;
			end if;
			if v_attrow.attnum = v_attno then
				v_attfound = 't';
				exit;
			end if;
			v_i := v_i + 1;
		end loop;

		if v_attfound then
			v_attkind := v_attkind || 'k';
		else
			v_attkind := v_attkind || 'v';
		end if;
	end loop;

	-- Strip off trailing v characters as they are not needed by the logtrigger
	v_attkind := pg_catalog.rtrim(v_attkind, 'v');

	--
	-- Return the resulting attkind
	--
	return v_attkind;
end;
$$;


ALTER FUNCTION _slony_cursos.determineattkindunique(p_tab_fqname text, p_idx_name name) OWNER TO postgres;

--
-- TOC entry 2671 (class 0 OID 0)
-- Dependencies: 347
-- Name: FUNCTION determineattkindunique(p_tab_fqname text, p_idx_name name); Type: COMMENT; Schema: _slony_cursos; Owner: postgres
--

COMMENT ON FUNCTION determineattkindunique(p_tab_fqname text, p_idx_name name) IS 'determineAttKindUnique (tab_fqname, indexname)

Given a tablename, return the Slony-I specific attkind (used for the
log trigger) of the table. Use the specified unique index or the
primary key (if indexname is NULL).';


--
-- TOC entry 346 (class 1255 OID 27661)
-- Name: determineidxnameunique(text, name); Type: FUNCTION; Schema: _slony_cursos; Owner: postgres
--

CREATE FUNCTION determineidxnameunique(p_tab_fqname text, p_idx_name name) RETURNS name
    LANGUAGE plpgsql
    AS $$
declare
	v_tab_fqname_quoted	text default '';
	v_idxrow		record;
begin
	v_tab_fqname_quoted := "_slony_cursos".slon_quote_input(p_tab_fqname);
	--
	-- Ensure that the table exists
	--
	if (select PGC.relname
				from "pg_catalog".pg_class PGC,
					"pg_catalog".pg_namespace PGN
				where "_slony_cursos".slon_quote_brute(PGN.nspname) || '.' ||
					"_slony_cursos".slon_quote_brute(PGC.relname) = v_tab_fqname_quoted
					and PGN.oid = PGC.relnamespace) is null then
		raise exception 'Slony-I: determineIdxnameUnique(): table % not found', v_tab_fqname_quoted;
	end if;

	--
	-- Lookup the tables primary key or the specified unique index
	--
	if p_idx_name isnull then
		select PGXC.relname
				into v_idxrow
				from "pg_catalog".pg_class PGC,
					"pg_catalog".pg_namespace PGN,
					"pg_catalog".pg_index PGX,
					"pg_catalog".pg_class PGXC
				where "_slony_cursos".slon_quote_brute(PGN.nspname) || '.' ||
					"_slony_cursos".slon_quote_brute(PGC.relname) = v_tab_fqname_quoted
					and PGN.oid = PGC.relnamespace
					and PGX.indrelid = PGC.oid
					and PGX.indexrelid = PGXC.oid
					and PGX.indisprimary;
		if not found then
			raise exception 'Slony-I: table % has no primary key',
					v_tab_fqname_quoted;
		end if;
	else
		select PGXC.relname
				into v_idxrow
				from "pg_catalog".pg_class PGC,
					"pg_catalog".pg_namespace PGN,
					"pg_catalog".pg_index PGX,
					"pg_catalog".pg_class PGXC
				where "_slony_cursos".slon_quote_brute(PGN.nspname) || '.' ||
					"_slony_cursos".slon_quote_brute(PGC.relname) = v_tab_fqname_quoted
					and PGN.oid = PGC.relnamespace
					and PGX.indrelid = PGC.oid
					and PGX.indexrelid = PGXC.oid
					and PGX.indisunique
					and "_slony_cursos".slon_quote_brute(PGXC.relname) = "_slony_cursos".slon_quote_input(p_idx_name);
		if not found then
			raise exception 'Slony-I: table % has no unique index %',
					v_tab_fqname_quoted, p_idx_name;
		end if;
	end if;

	--
	-- Return the found index name
	--
	return v_idxrow.relname;
end;
$$;


ALTER FUNCTION _slony_cursos.determineidxnameunique(p_tab_fqname text, p_idx_name name) OWNER TO postgres;

--
-- TOC entry 2672 (class 0 OID 0)
-- Dependencies: 346
-- Name: FUNCTION determineidxnameunique(p_tab_fqname text, p_idx_name name); Type: COMMENT; Schema: _slony_cursos; Owner: postgres
--

COMMENT ON FUNCTION determineidxnameunique(p_tab_fqname text, p_idx_name name) IS 'FUNCTION determineIdxnameUnique (tab_fqname, indexname)

Given a tablename, tab_fqname, check that the unique index, indexname,
exists or return the primary key index name for the table.  If there
is no unique index, it raises an exception.';


--
-- TOC entry 365 (class 1255 OID 27687)
-- Name: disable_indexes_on_table(oid); Type: FUNCTION; Schema: _slony_cursos; Owner: postgres
--

CREATE FUNCTION disable_indexes_on_table(i_oid oid) RETURNS integer
    LANGUAGE plpgsql
    AS $$
begin
	-- Setting pg_class.relhasindex to false will cause copy not to
	-- maintain any indexes. At the end of the copy we will reenable
	-- them and reindex the table. This bulk creating of indexes is
	-- faster.

	update pg_catalog.pg_class set relhasindex ='f' where oid = i_oid;
	return 1;
end $$;


ALTER FUNCTION _slony_cursos.disable_indexes_on_table(i_oid oid) OWNER TO postgres;

--
-- TOC entry 2673 (class 0 OID 0)
-- Dependencies: 365
-- Name: FUNCTION disable_indexes_on_table(i_oid oid); Type: COMMENT; Schema: _slony_cursos; Owner: postgres
--

COMMENT ON FUNCTION disable_indexes_on_table(i_oid oid) IS 'disable indexes on the specified table.
Used during subscription process to suppress indexes, which allows
COPY to go much faster.

This may be set as a SECURITY DEFINER in order to eliminate the need
for superuser access by Slony-I.
';


--
-- TOC entry 286 (class 1255 OID 27600)
-- Name: disablenode(integer); Type: FUNCTION; Schema: _slony_cursos; Owner: postgres
--

CREATE FUNCTION disablenode(p_no_id integer) RETURNS bigint
    LANGUAGE plpgsql
    AS $$
begin
	-- **** TODO ****
	raise exception 'Slony-I: disableNode() not implemented';
end;
$$;


ALTER FUNCTION _slony_cursos.disablenode(p_no_id integer) OWNER TO postgres;

--
-- TOC entry 2674 (class 0 OID 0)
-- Dependencies: 286
-- Name: FUNCTION disablenode(p_no_id integer); Type: COMMENT; Schema: _slony_cursos; Owner: postgres
--

COMMENT ON FUNCTION disablenode(p_no_id integer) IS 'process DISABLE_NODE event for node no_id

NOTE: This is not yet implemented!';


--
-- TOC entry 277 (class 1255 OID 27601)
-- Name: disablenode_int(integer); Type: FUNCTION; Schema: _slony_cursos; Owner: postgres
--

CREATE FUNCTION disablenode_int(p_no_id integer) RETURNS integer
    LANGUAGE plpgsql
    AS $$
begin
	-- **** TODO ****
	raise exception 'Slony-I: disableNode_int() not implemented';
end;
$$;


ALTER FUNCTION _slony_cursos.disablenode_int(p_no_id integer) OWNER TO postgres;

--
-- TOC entry 304 (class 1255 OID 27619)
-- Name: droplisten(integer, integer, integer); Type: FUNCTION; Schema: _slony_cursos; Owner: postgres
--

CREATE FUNCTION droplisten(p_li_origin integer, p_li_provider integer, p_li_receiver integer) RETURNS bigint
    LANGUAGE plpgsql
    AS $$
begin
	perform "_slony_cursos".dropListen_int(p_li_origin, 
			p_li_provider, p_li_receiver);
	
	return  "_slony_cursos".createEvent ('_slony_cursos', 'DROP_LISTEN',
			p_li_origin::text, p_li_provider::text, p_li_receiver::text);
end;
$$;


ALTER FUNCTION _slony_cursos.droplisten(p_li_origin integer, p_li_provider integer, p_li_receiver integer) OWNER TO postgres;

--
-- TOC entry 2675 (class 0 OID 0)
-- Dependencies: 304
-- Name: FUNCTION droplisten(p_li_origin integer, p_li_provider integer, p_li_receiver integer); Type: COMMENT; Schema: _slony_cursos; Owner: postgres
--

COMMENT ON FUNCTION droplisten(p_li_origin integer, p_li_provider integer, p_li_receiver integer) IS 'dropListen (li_origin, li_provider, li_receiver)

Generate the DROP_LISTEN event.';


--
-- TOC entry 305 (class 1255 OID 27620)
-- Name: droplisten_int(integer, integer, integer); Type: FUNCTION; Schema: _slony_cursos; Owner: postgres
--

CREATE FUNCTION droplisten_int(p_li_origin integer, p_li_provider integer, p_li_receiver integer) RETURNS integer
    LANGUAGE plpgsql
    AS $$
begin
	-- ----
	-- Grab the central configuration lock
	-- ----
	lock table "_slony_cursos".sl_config_lock;

	delete from "_slony_cursos".sl_listen
			where li_origin = p_li_origin
			and li_provider = p_li_provider
			and li_receiver = p_li_receiver;
	if found then
		return 1;
	else
		return 0;
	end if;
end;
$$;


ALTER FUNCTION _slony_cursos.droplisten_int(p_li_origin integer, p_li_provider integer, p_li_receiver integer) OWNER TO postgres;

--
-- TOC entry 2676 (class 0 OID 0)
-- Dependencies: 305
-- Name: FUNCTION droplisten_int(p_li_origin integer, p_li_provider integer, p_li_receiver integer); Type: COMMENT; Schema: _slony_cursos; Owner: postgres
--

COMMENT ON FUNCTION droplisten_int(p_li_origin integer, p_li_provider integer, p_li_receiver integer) IS 'dropListen (li_origin, li_provider, li_receiver)

Process the DROP_LISTEN event, deleting the sl_listen entry for
the indicated (origin,provider,receiver) combination.';


--
-- TOC entry 287 (class 1255 OID 27602)
-- Name: dropnode(integer[]); Type: FUNCTION; Schema: _slony_cursos; Owner: postgres
--

CREATE FUNCTION dropnode(p_no_ids integer[]) RETURNS bigint
    LANGUAGE plpgsql
    AS $$
declare
	v_node_row		record;
	v_idx         integer;
begin
	-- ----
	-- Grab the central configuration lock
	-- ----
	lock table "_slony_cursos".sl_config_lock;

	-- ----
	-- Check that this got called on a different node
	-- ----
	if  "_slony_cursos".getLocalNodeId('_slony_cursos') = ANY (p_no_ids) then
		raise exception 'Slony-I: DROP_NODE cannot initiate on the dropped node';
	end if;

	--
	-- if any of the deleted nodes are receivers we drop the sl_subscribe line
	--
	delete from "_slony_cursos".sl_subscribe where sub_receiver = ANY (p_no_ids);

	v_idx:=1;
	LOOP
	  EXIT WHEN v_idx>array_upper(p_no_ids,1) ;
	  select * into v_node_row from "_slony_cursos".sl_node
			where no_id = p_no_ids[v_idx]
			for update;
	  if not found then
		 raise exception 'Slony-I: unknown node ID % %', p_no_ids[v_idx],v_idx;
	  end if;
	  -- ----
	  -- Make sure we do not break other nodes subscriptions with this
	  -- ----
	  if exists (select true from "_slony_cursos".sl_subscribe
			where sub_provider = p_no_ids[v_idx])
	  then
		raise exception 'Slony-I: Node % is still configured as a data provider',
				p_no_ids[v_idx];
	  end if;

	  -- ----
	  -- Make sure no set originates there any more
	  -- ----
	  if exists (select true from "_slony_cursos".sl_set
			where set_origin = p_no_ids[v_idx])
	  then
	  	  raise exception 'Slony-I: Node % is still origin of one or more sets',
				p_no_ids[v_idx];
	  end if;

	  -- ----
	  -- Call the internal drop functionality and generate the event
	  -- ----
	  perform "_slony_cursos".dropNode_int(p_no_ids[v_idx]);
	  v_idx:=v_idx+1;
	END LOOP;
	return  "_slony_cursos".createEvent('_slony_cursos', 'DROP_NODE',
									array_to_string(p_no_ids,','));
end;
$$;


ALTER FUNCTION _slony_cursos.dropnode(p_no_ids integer[]) OWNER TO postgres;

--
-- TOC entry 2677 (class 0 OID 0)
-- Dependencies: 287
-- Name: FUNCTION dropnode(p_no_ids integer[]); Type: COMMENT; Schema: _slony_cursos; Owner: postgres
--

COMMENT ON FUNCTION dropnode(p_no_ids integer[]) IS 'generate DROP_NODE event to drop node node_id from replication';


--
-- TOC entry 288 (class 1255 OID 27603)
-- Name: dropnode_int(integer); Type: FUNCTION; Schema: _slony_cursos; Owner: postgres
--

CREATE FUNCTION dropnode_int(p_no_id integer) RETURNS integer
    LANGUAGE plpgsql
    AS $$
declare
	v_tab_row		record;
begin
	-- ----
	-- Grab the central configuration lock
	-- ----
	lock table "_slony_cursos".sl_config_lock;

	-- ----
	-- If the dropped node is a remote node, clean the configuration
	-- from all traces for it.
	-- ----
	if p_no_id <> "_slony_cursos".getLocalNodeId('_slony_cursos') then
		delete from "_slony_cursos".sl_subscribe
				where sub_receiver = p_no_id;
		delete from "_slony_cursos".sl_listen
				where li_origin = p_no_id
					or li_provider = p_no_id
					or li_receiver = p_no_id;
		delete from "_slony_cursos".sl_path
				where pa_server = p_no_id
					or pa_client = p_no_id;
		delete from "_slony_cursos".sl_confirm
				where con_origin = p_no_id
					or con_received = p_no_id;
		delete from "_slony_cursos".sl_event
				where ev_origin = p_no_id;
		delete from "_slony_cursos".sl_node
				where no_id = p_no_id;

		return p_no_id;
	end if;

	-- ----
	-- This is us ... deactivate the node for now, the daemon
	-- will call uninstallNode() in a separate transaction.
	-- ----
	update "_slony_cursos".sl_node
			set no_active = false
			where no_id = p_no_id;

	-- Rewrite sl_listen table
	perform "_slony_cursos".RebuildListenEntries();

	return p_no_id;
end;
$$;


ALTER FUNCTION _slony_cursos.dropnode_int(p_no_id integer) OWNER TO postgres;

--
-- TOC entry 2678 (class 0 OID 0)
-- Dependencies: 288
-- Name: FUNCTION dropnode_int(p_no_id integer); Type: COMMENT; Schema: _slony_cursos; Owner: postgres
--

COMMENT ON FUNCTION dropnode_int(p_no_id integer) IS 'internal function to process DROP_NODE event to drop node node_id from replication';


--
-- TOC entry 300 (class 1255 OID 27615)
-- Name: droppath(integer, integer); Type: FUNCTION; Schema: _slony_cursos; Owner: postgres
--

CREATE FUNCTION droppath(p_pa_server integer, p_pa_client integer) RETURNS bigint
    LANGUAGE plpgsql
    AS $$
declare
	v_row			record;
begin
	-- ----
	-- Grab the central configuration lock
	-- ----
	lock table "_slony_cursos".sl_config_lock;

	-- ----
	-- There should be no existing subscriptions. Auto unsubscribing
	-- is considered too dangerous. 
	-- ----
	for v_row in select sub_set, sub_provider, sub_receiver
			from "_slony_cursos".sl_subscribe
			where sub_provider = p_pa_server
			and sub_receiver = p_pa_client
	loop
		raise exception 
			'Slony-I: Path cannot be dropped, subscription of set % needs it',
			v_row.sub_set;
	end loop;

	-- ----
	-- Drop all sl_listen entries that depend on this path
	-- ----
	for v_row in select li_origin, li_provider, li_receiver
			from "_slony_cursos".sl_listen
			where li_provider = p_pa_server
			and li_receiver = p_pa_client
	loop
		perform "_slony_cursos".dropListen(
				v_row.li_origin, v_row.li_provider, v_row.li_receiver);
	end loop;

	-- ----
	-- Now drop the path and create the event
	-- ----
	perform "_slony_cursos".dropPath_int(p_pa_server, p_pa_client);

	-- Rewrite sl_listen table
	perform "_slony_cursos".RebuildListenEntries();

	return  "_slony_cursos".createEvent ('_slony_cursos', 'DROP_PATH',
			p_pa_server::text, p_pa_client::text);
end;
$$;


ALTER FUNCTION _slony_cursos.droppath(p_pa_server integer, p_pa_client integer) OWNER TO postgres;

--
-- TOC entry 2679 (class 0 OID 0)
-- Dependencies: 300
-- Name: FUNCTION droppath(p_pa_server integer, p_pa_client integer); Type: COMMENT; Schema: _slony_cursos; Owner: postgres
--

COMMENT ON FUNCTION droppath(p_pa_server integer, p_pa_client integer) IS 'Generate DROP_PATH event to drop path from pa_server to pa_client';


--
-- TOC entry 301 (class 1255 OID 27616)
-- Name: droppath_int(integer, integer); Type: FUNCTION; Schema: _slony_cursos; Owner: postgres
--

CREATE FUNCTION droppath_int(p_pa_server integer, p_pa_client integer) RETURNS integer
    LANGUAGE plpgsql
    AS $$
begin
	-- ----
	-- Grab the central configuration lock
	-- ----
	lock table "_slony_cursos".sl_config_lock;

	-- ----
	-- Remove any dangling sl_listen entries with the server
	-- as provider and the client as receiver. This must have
	-- been cleared out before, but obviously was not.
	-- ----
	delete from "_slony_cursos".sl_listen
			where li_provider = p_pa_server
			and li_receiver = p_pa_client;

	delete from "_slony_cursos".sl_path
			where pa_server = p_pa_server
			and pa_client = p_pa_client;

	if found then
		-- Rewrite sl_listen table
		perform "_slony_cursos".RebuildListenEntries();

		return 1;
	else
		-- Rewrite sl_listen table
		perform "_slony_cursos".RebuildListenEntries();

		return 0;
	end if;
end;
$$;


ALTER FUNCTION _slony_cursos.droppath_int(p_pa_server integer, p_pa_client integer) OWNER TO postgres;

--
-- TOC entry 2680 (class 0 OID 0)
-- Dependencies: 301
-- Name: FUNCTION droppath_int(p_pa_server integer, p_pa_client integer); Type: COMMENT; Schema: _slony_cursos; Owner: postgres
--

COMMENT ON FUNCTION droppath_int(p_pa_server integer, p_pa_client integer) IS 'Process DROP_PATH event to drop path from pa_server to pa_client';


--
-- TOC entry 312 (class 1255 OID 27628)
-- Name: dropset(integer); Type: FUNCTION; Schema: _slony_cursos; Owner: postgres
--

CREATE FUNCTION dropset(p_set_id integer) RETURNS bigint
    LANGUAGE plpgsql
    AS $$
declare
	v_origin			int4;
begin
	-- ----
	-- Grab the central configuration lock
	-- ----
	lock table "_slony_cursos".sl_config_lock;

	-- ----
	-- Check that the set exists and originates here
	-- ----
	select set_origin into v_origin from "_slony_cursos".sl_set
			where set_id = p_set_id;
	if not found then
		raise exception 'Slony-I: set % not found', p_set_id;
	end if;
	if v_origin != "_slony_cursos".getLocalNodeId('_slony_cursos') then
		raise exception 'Slony-I: set % does not originate on local node',
				p_set_id;
	end if;

	-- ----
	-- Call the internal drop set functionality and generate the event
	-- ----
	perform "_slony_cursos".dropSet_int(p_set_id);
	return  "_slony_cursos".createEvent('_slony_cursos', 'DROP_SET', 
			p_set_id::text);
end;
$$;


ALTER FUNCTION _slony_cursos.dropset(p_set_id integer) OWNER TO postgres;

--
-- TOC entry 2681 (class 0 OID 0)
-- Dependencies: 312
-- Name: FUNCTION dropset(p_set_id integer); Type: COMMENT; Schema: _slony_cursos; Owner: postgres
--

COMMENT ON FUNCTION dropset(p_set_id integer) IS 'Process DROP_SET event to drop replication of set set_id.  This involves:
- Removing log and deny access triggers
- Removing all traces of the set configuration, including sequences, tables, subscribers, syncs, and the set itself';


--
-- TOC entry 313 (class 1255 OID 27629)
-- Name: dropset_int(integer); Type: FUNCTION; Schema: _slony_cursos; Owner: postgres
--

CREATE FUNCTION dropset_int(p_set_id integer) RETURNS integer
    LANGUAGE plpgsql
    AS $$
declare
	v_tab_row			record;
begin
	-- ----
	-- Grab the central configuration lock
	-- ----
	lock table "_slony_cursos".sl_config_lock;

	-- ----
	-- Restore all tables original triggers and rules and remove
	-- our replication stuff.
	-- ----
	for v_tab_row in select tab_id from "_slony_cursos".sl_table
			where tab_set = p_set_id
			order by tab_id
	loop
		perform "_slony_cursos".alterTableDropTriggers(v_tab_row.tab_id);
	end loop;

	-- ----
	-- Remove all traces of the set configuration
	-- ----
	delete from "_slony_cursos".sl_sequence
			where seq_set = p_set_id;
	delete from "_slony_cursos".sl_table
			where tab_set = p_set_id;
	delete from "_slony_cursos".sl_subscribe
			where sub_set = p_set_id;
	delete from "_slony_cursos".sl_setsync
			where ssy_setid = p_set_id;
	delete from "_slony_cursos".sl_set
			where set_id = p_set_id;

	-- Regenerate sl_listen since we revised the subscriptions
	perform "_slony_cursos".RebuildListenEntries();

	-- Run addPartialLogIndices() to try to add indices to unused sl_log_? table
	perform "_slony_cursos".addPartialLogIndices();

	return p_set_id;
end;
$$;


ALTER FUNCTION _slony_cursos.dropset_int(p_set_id integer) OWNER TO postgres;

--
-- TOC entry 366 (class 1255 OID 27688)
-- Name: enable_indexes_on_table(oid); Type: FUNCTION; Schema: _slony_cursos; Owner: postgres
--

CREATE FUNCTION enable_indexes_on_table(i_oid oid) RETURNS integer
    LANGUAGE plpgsql SECURITY DEFINER
    AS $$
begin
	update pg_catalog.pg_class set relhasindex ='t' where oid = i_oid;
	return 1;
end $$;


ALTER FUNCTION _slony_cursos.enable_indexes_on_table(i_oid oid) OWNER TO postgres;

--
-- TOC entry 2682 (class 0 OID 0)
-- Dependencies: 366
-- Name: FUNCTION enable_indexes_on_table(i_oid oid); Type: COMMENT; Schema: _slony_cursos; Owner: postgres
--

COMMENT ON FUNCTION enable_indexes_on_table(i_oid oid) IS 're-enable indexes on the specified table.

This may be set as a SECURITY DEFINER in order to eliminate the need
for superuser access by Slony-I.
';


--
-- TOC entry 284 (class 1255 OID 27598)
-- Name: enablenode(integer); Type: FUNCTION; Schema: _slony_cursos; Owner: postgres
--

CREATE FUNCTION enablenode(p_no_id integer) RETURNS bigint
    LANGUAGE plpgsql
    AS $$
declare
	v_local_node_id	int4;
	v_node_row		record;
begin
	-- ----
	-- Grab the central configuration lock
	-- ----
	lock table "_slony_cursos".sl_config_lock;

	-- ----
	-- Check that we are the node to activate and that we are
	-- currently disabled.
	-- ----
	v_local_node_id := "_slony_cursos".getLocalNodeId('_slony_cursos');
	select * into v_node_row
			from "_slony_cursos".sl_node
			where no_id = p_no_id
			for update;
	if not found then 
		raise exception 'Slony-I: node % not found', p_no_id;
	end if;
	if v_node_row.no_active then
		raise exception 'Slony-I: node % is already active', p_no_id;
	end if;

	-- ----
	-- Activate this node and generate the ENABLE_NODE event
	-- ----
	perform "_slony_cursos".enableNode_int (p_no_id);
	return  "_slony_cursos".createEvent('_slony_cursos', 'ENABLE_NODE',
									p_no_id::text);
end;
$$;


ALTER FUNCTION _slony_cursos.enablenode(p_no_id integer) OWNER TO postgres;

--
-- TOC entry 2683 (class 0 OID 0)
-- Dependencies: 284
-- Name: FUNCTION enablenode(p_no_id integer); Type: COMMENT; Schema: _slony_cursos; Owner: postgres
--

COMMENT ON FUNCTION enablenode(p_no_id integer) IS 'no_id - Node ID #

Generate the ENABLE_NODE event for node no_id';


--
-- TOC entry 285 (class 1255 OID 27599)
-- Name: enablenode_int(integer); Type: FUNCTION; Schema: _slony_cursos; Owner: postgres
--

CREATE FUNCTION enablenode_int(p_no_id integer) RETURNS integer
    LANGUAGE plpgsql
    AS $$
declare
	v_local_node_id	int4;
	v_node_row		record;
	v_sub_row		record;
begin
	-- ----
	-- Grab the central configuration lock
	-- ----
	lock table "_slony_cursos".sl_config_lock;

	-- ----
	-- Check that the node is inactive
	-- ----
	select * into v_node_row
			from "_slony_cursos".sl_node
			where no_id = p_no_id
			for update;
	if not found then 
		raise exception 'Slony-I: node % not found', p_no_id;
	end if;
	if v_node_row.no_active then
		return p_no_id;
	end if;

	-- ----
	-- Activate the node and generate sl_confirm status rows for it.
	-- ----
	update "_slony_cursos".sl_node
			set no_active = 't'
			where no_id = p_no_id;
	insert into "_slony_cursos".sl_confirm
			(con_origin, con_received, con_seqno)
			select no_id, p_no_id, 0 from "_slony_cursos".sl_node
				where no_id != p_no_id
				and no_active;
	insert into "_slony_cursos".sl_confirm
			(con_origin, con_received, con_seqno)
			select p_no_id, no_id, 0 from "_slony_cursos".sl_node
				where no_id != p_no_id
				and no_active;

	-- ----
	-- Generate ENABLE_SUBSCRIPTION events for all sets that
	-- origin here and are subscribed by the just enabled node.
	-- ----
	v_local_node_id := "_slony_cursos".getLocalNodeId('_slony_cursos');
	for v_sub_row in select SUB.sub_set, SUB.sub_provider from
			"_slony_cursos".sl_set S,
			"_slony_cursos".sl_subscribe SUB
			where S.set_origin = v_local_node_id
			and S.set_id = SUB.sub_set
			and SUB.sub_receiver = p_no_id
			for update of S
	loop
		perform "_slony_cursos".enableSubscription (v_sub_row.sub_set,
				v_sub_row.sub_provider, p_no_id);
	end loop;

	return p_no_id;
end;
$$;


ALTER FUNCTION _slony_cursos.enablenode_int(p_no_id integer) OWNER TO postgres;

--
-- TOC entry 2684 (class 0 OID 0)
-- Dependencies: 285
-- Name: FUNCTION enablenode_int(p_no_id integer); Type: COMMENT; Schema: _slony_cursos; Owner: postgres
--

COMMENT ON FUNCTION enablenode_int(p_no_id integer) IS 'no_id - Node ID #

Internal function to process the ENABLE_NODE event for node no_id';


--
-- TOC entry 342 (class 1255 OID 27657)
-- Name: enablesubscription(integer, integer, integer); Type: FUNCTION; Schema: _slony_cursos; Owner: postgres
--

CREATE FUNCTION enablesubscription(p_sub_set integer, p_sub_provider integer, p_sub_receiver integer) RETURNS integer
    LANGUAGE plpgsql
    AS $$
begin
	return  "_slony_cursos".enableSubscription_int (p_sub_set, 
			p_sub_provider, p_sub_receiver);
end;
$$;


ALTER FUNCTION _slony_cursos.enablesubscription(p_sub_set integer, p_sub_provider integer, p_sub_receiver integer) OWNER TO postgres;

--
-- TOC entry 2685 (class 0 OID 0)
-- Dependencies: 342
-- Name: FUNCTION enablesubscription(p_sub_set integer, p_sub_provider integer, p_sub_receiver integer); Type: COMMENT; Schema: _slony_cursos; Owner: postgres
--

COMMENT ON FUNCTION enablesubscription(p_sub_set integer, p_sub_provider integer, p_sub_receiver integer) IS 'enableSubscription (sub_set, sub_provider, sub_receiver)

Indicates that sub_receiver intends subscribing to set sub_set from
sub_provider.  Work is all done by the internal function
enableSubscription_int (sub_set, sub_provider, sub_receiver).';


--
-- TOC entry 343 (class 1255 OID 27658)
-- Name: enablesubscription_int(integer, integer, integer); Type: FUNCTION; Schema: _slony_cursos; Owner: postgres
--

CREATE FUNCTION enablesubscription_int(p_sub_set integer, p_sub_provider integer, p_sub_receiver integer) RETURNS integer
    LANGUAGE plpgsql
    AS $$
declare
	v_n					int4;
begin
	-- ----
	-- Grab the central configuration lock
	-- ----
	lock table "_slony_cursos".sl_config_lock;

	-- ----
	-- The real work is done in the replication engine. All
	-- we have to do here is remembering that it happened.
	-- ----

	-- ----
	-- Well, not only ... we might be missing an important event here
	-- ----
	if not exists (select true from "_slony_cursos".sl_path
			where pa_server = p_sub_provider
			and pa_client = p_sub_receiver)
	then
		insert into "_slony_cursos".sl_path
				(pa_server, pa_client, pa_conninfo, pa_connretry)
				values 
				(p_sub_provider, p_sub_receiver, 
				'<event pending>', 10);
	end if;

	update "_slony_cursos".sl_subscribe
			set sub_active = 't'
			where sub_set = p_sub_set
			and sub_receiver = p_sub_receiver;
	get diagnostics v_n = row_count;
	if v_n = 0 then
		insert into "_slony_cursos".sl_subscribe
				(sub_set, sub_provider, sub_receiver,
				sub_forward, sub_active)
				values
				(p_sub_set, p_sub_provider, p_sub_receiver,
				false, true);
	end if;

	-- Rewrite sl_listen table
	perform "_slony_cursos".RebuildListenEntries();

	return p_sub_set;
end;
$$;


ALTER FUNCTION _slony_cursos.enablesubscription_int(p_sub_set integer, p_sub_provider integer, p_sub_receiver integer) OWNER TO postgres;

--
-- TOC entry 2686 (class 0 OID 0)
-- Dependencies: 343
-- Name: FUNCTION enablesubscription_int(p_sub_set integer, p_sub_provider integer, p_sub_receiver integer); Type: COMMENT; Schema: _slony_cursos; Owner: postgres
--

COMMENT ON FUNCTION enablesubscription_int(p_sub_set integer, p_sub_provider integer, p_sub_receiver integer) IS 'enableSubscription_int (sub_set, sub_provider, sub_receiver)

Internal function to enable subscription of node sub_receiver to set
sub_set via node sub_provider.

slon does most of the work; all we need do here is to remember that it
happened.  The function updates sl_subscribe, indicating that the
subscription has become active.';


--
-- TOC entry 290 (class 1255 OID 27605)
-- Name: failednode(integer, integer, integer[]); Type: FUNCTION; Schema: _slony_cursos; Owner: postgres
--

CREATE FUNCTION failednode(p_failed_node integer, p_backup_node integer, p_failed_nodes integer[]) RETURNS integer
    LANGUAGE plpgsql
    AS $$
declare
	v_row				record;
	v_row2				record;
	v_failed					boolean;
    v_restart_required          boolean;
begin
	
	-- ----
	-- Grab the central configuration lock
	-- ----
	lock table "_slony_cursos".sl_config_lock;

	v_restart_required:=false;
	--
	-- any nodes other than the backup receiving
	-- ANY subscription from a failed node
	-- will now get that data from the backup node.
	update "_slony_cursos".sl_subscribe set 
		   sub_provider=p_backup_node
		   where sub_provider=p_failed_node
		   and sub_receiver<>p_backup_node
		   and sub_receiver <> ALL (p_failed_nodes);
	if found then
	   v_restart_required:=true;
	end if;
	-- 
	-- if this node is receiving a subscription from the backup node
	-- with a failed node as the provider we need to fix this.
	update "_slony_cursos".sl_subscribe set 
	        sub_provider=p_backup_node
		from "_slony_cursos".sl_set
		where set_id = sub_set
		and set_origin=p_failed_node
		and sub_provider = ANY(p_failed_nodes)
		and sub_receiver="_slony_cursos".getLocalNodeId('_slony_cursos');

	-- ----
	-- Terminate all connections of the failed node the hard way
	-- ----
	perform "_slony_cursos".terminateNodeConnections(p_failed_node);

	-- Clear out the paths for the failed node.
	-- This ensures that *this* node won't be pulling data from
	-- the failed node even if it *does* become accessible

	update "_slony_cursos".sl_path set pa_conninfo='<event pending>' WHERE
	   		  pa_server=p_failed_node
			  and pa_conninfo<>'<event pending>';

	if found then
	   v_restart_required:=true;
	end if;

	v_failed := exists (select 1 from "_slony_cursos".sl_node 
		   where no_failed=true and no_id=p_failed_node);

    if not v_failed then
	   	
		update "_slony_cursos".sl_node set no_failed=true where no_id = ANY (p_failed_nodes)
			   and no_failed=false;
		if found then
	   	   v_restart_required:=true;
		end if;
	end if;	

	if v_restart_required then
	  -- Rewrite sl_listen table
	  perform "_slony_cursos".RebuildListenEntries();	   
	
	  -- ----
	  -- Make sure the node daemon will restart
 	  -- ----
	  notify "_slony_cursos_Restart";
    end if;


	-- ----
	-- That is it - so far.
	-- ----
	return p_failed_node;
end;
$$;


ALTER FUNCTION _slony_cursos.failednode(p_failed_node integer, p_backup_node integer, p_failed_nodes integer[]) OWNER TO postgres;

--
-- TOC entry 2687 (class 0 OID 0)
-- Dependencies: 290
-- Name: FUNCTION failednode(p_failed_node integer, p_backup_node integer, p_failed_nodes integer[]); Type: COMMENT; Schema: _slony_cursos; Owner: postgres
--

COMMENT ON FUNCTION failednode(p_failed_node integer, p_backup_node integer, p_failed_nodes integer[]) IS 'Initiate failover from failed_node to backup_node.  This function must be called on all nodes, 
and then waited for the restart of all node daemons.';


--
-- TOC entry 291 (class 1255 OID 27606)
-- Name: failednode2(integer, integer, bigint, integer[]); Type: FUNCTION; Schema: _slony_cursos; Owner: postgres
--

CREATE FUNCTION failednode2(p_failed_node integer, p_backup_node integer, p_ev_seqno bigint, p_failed_nodes integer[]) RETURNS bigint
    LANGUAGE plpgsql
    AS $$
declare
	v_row				record;
	v_new_event			bigint;
begin
	-- ----
	-- Grab the central configuration lock
	-- ----
	lock table "_slony_cursos".sl_config_lock;

	select * into v_row
			from "_slony_cursos".sl_event
			where ev_origin = p_failed_node
			and ev_seqno = p_ev_seqno;
	if not found then
		raise exception 'Slony-I: event %,% not found',
				p_failed_node, p_ev_seqno;
	end if;

	update "_slony_cursos".sl_node set no_failed=true  where no_id = ANY 
	(p_failed_nodes) and no_failed=false;
	-- Rewrite sl_listen table
	perform "_slony_cursos".RebuildListenEntries();
	-- ----
	-- Make sure the node daemon will restart
	-- ----
	raise notice 'calling restart node %',p_failed_node;

	notify "_slony_cursos_Restart";

	select "_slony_cursos".createEvent('_slony_cursos','FAILOVER_NODE',
								p_failed_node::text,p_ev_seqno::text,
								array_to_string(p_failed_nodes,','))
			into v_new_event;
		

	return v_new_event;
end;
$$;


ALTER FUNCTION _slony_cursos.failednode2(p_failed_node integer, p_backup_node integer, p_ev_seqno bigint, p_failed_nodes integer[]) OWNER TO postgres;

--
-- TOC entry 2688 (class 0 OID 0)
-- Dependencies: 291
-- Name: FUNCTION failednode2(p_failed_node integer, p_backup_node integer, p_ev_seqno bigint, p_failed_nodes integer[]); Type: COMMENT; Schema: _slony_cursos; Owner: postgres
--

COMMENT ON FUNCTION failednode2(p_failed_node integer, p_backup_node integer, p_ev_seqno bigint, p_failed_nodes integer[]) IS 'FUNCTION failedNode2 (failed_node, backup_node, set_id, ev_seqno, ev_seqfake,p_failed_nodes)

On the node that has the highest sequence number of the failed node,
fake the FAILOVER_SET event.';


--
-- TOC entry 292 (class 1255 OID 27607)
-- Name: failednode3(integer, integer, bigint); Type: FUNCTION; Schema: _slony_cursos; Owner: postgres
--

CREATE FUNCTION failednode3(p_failed_node integer, p_backup_node integer, p_seq_no bigint) RETURNS integer
    LANGUAGE plpgsql
    AS $$
declare

begin
	-- ----
	-- Grab the central configuration lock
	-- ----
	lock table "_slony_cursos".sl_config_lock;

	perform "_slony_cursos".failoverSet_int(p_failed_node,
		p_backup_node,p_seq_no);

	notify "_slony_cursos_Restart";
    return 0;
end;
$$;


ALTER FUNCTION _slony_cursos.failednode3(p_failed_node integer, p_backup_node integer, p_seq_no bigint) OWNER TO postgres;

--
-- TOC entry 293 (class 1255 OID 27608)
-- Name: failoverset_int(integer, integer, bigint); Type: FUNCTION; Schema: _slony_cursos; Owner: postgres
--

CREATE FUNCTION failoverset_int(p_failed_node integer, p_backup_node integer, p_last_seqno bigint) RETURNS integer
    LANGUAGE plpgsql
    AS $$
declare
	v_row				record;
	v_last_sync			int8;
	v_set				int4;
begin
	-- ----
	-- Grab the central configuration lock
	-- ----
	lock table "_slony_cursos".sl_config_lock;

	SELECT max(ev_seqno) into v_last_sync FROM "_slony_cursos".sl_event where
		   ev_origin=p_failed_node;
	if v_last_sync > p_last_seqno then
	   -- this node is ahead of the last sequence number from the
	   -- failed node that the backup node has.
	   -- this node must unsubscribe from all sets from the origin.
	   for v_set in select set_id from "_slony_cursos".sl_set where 
		set_origin=p_failed_node
		loop
			raise warning 'Slony is dropping the subscription of set % found sync %s bigger than %s '
			, v_set, v_last_sync::text, p_last_seqno::text;
			perform "_slony_cursos".unsubscribeSet(v_set,
				   "_slony_cursos".getLocalNodeId('_slony_cursos'),
				   true);
		end loop;
		delete from "_slony_cursos".sl_event where ev_origin=p_failed_node
			   and ev_seqno > p_last_seqno;
	end if;
	-- ----
	-- Change the origin of the set now to the backup node.
	-- On the backup node this includes changing all the
	-- trigger and protection stuff
	for v_set in select set_id from "_slony_cursos".sl_set where 
		set_origin=p_failed_node
	loop
	-- ----
	   if p_backup_node = "_slony_cursos".getLocalNodeId('_slony_cursos') then
	   	  	delete from "_slony_cursos".sl_setsync
				where ssy_setid = v_set;
			delete from "_slony_cursos".sl_subscribe
				where sub_set = v_set
					and sub_receiver = p_backup_node;
			update "_slony_cursos".sl_set
				set set_origin = p_backup_node
				where set_id = v_set;		
			 update "_slony_cursos".sl_subscribe
						set sub_provider=p_backup_node
					  	FROM "_slony_cursos".sl_node receive_node
					   where sub_set = v_set
					   and sub_provider=p_failed_node
					   and sub_receiver=receive_node.no_id
					   and receive_node.no_failed=false;			

			for v_row in select * from "_slony_cursos".sl_table
				where tab_set = v_set
				order by tab_id
			loop
				perform "_slony_cursos".alterTableConfigureTriggers(v_row.tab_id);
			end loop;
	else
		raise notice 'deleting from sl_subscribe all rows with receiver %',
		p_backup_node;
		
			delete from "_slony_cursos".sl_subscribe
					  where sub_set = v_set
					  and sub_receiver = p_backup_node;
		
			update "_slony_cursos".sl_subscribe
				 		set sub_provider=p_backup_node
						FROM "_slony_cursos".sl_node receive_node
					   where sub_set = v_set
					    and sub_provider=p_failed_node
						 and sub_provider=p_failed_node
					   and sub_receiver=receive_node.no_id
					   and receive_node.no_failed=false;
			update "_slony_cursos".sl_set
					   set set_origin = p_backup_node
				where set_id = v_set;
			-- ----
			-- If we are a subscriber of the set ourself, change our
			-- setsync status to reflect the new set origin.
			-- ----
			if exists (select true from "_slony_cursos".sl_subscribe
			   where sub_set = v_set
			   	and sub_receiver = "_slony_cursos".getLocalNodeId(
						'_slony_cursos'))
			then
				delete from "_slony_cursos".sl_setsync
					   where ssy_setid = v_set;

				select coalesce(max(ev_seqno), 0) into v_last_sync
					   from "_slony_cursos".sl_event
					   where ev_origin = p_backup_node
					   and ev_type = 'SYNC';
				if v_last_sync > 0 then
				   insert into "_slony_cursos".sl_setsync
					(ssy_setid, ssy_origin, ssy_seqno,
					ssy_snapshot, ssy_action_list)
					select v_set, p_backup_node, v_last_sync,
					ev_snapshot, NULL
					from "_slony_cursos".sl_event
					where ev_origin = p_backup_node
						and ev_seqno = v_last_sync;
				else
					insert into "_slony_cursos".sl_setsync
					(ssy_setid, ssy_origin, ssy_seqno,
					ssy_snapshot, ssy_action_list)
					values (v_set, p_backup_node, '0',
					'1:1:', NULL);
				end if;	
			end if;
		end if;
	end loop;
	
	--If there are any subscriptions with 
	--the failed_node being the provider then
	--we want to redirect those subscriptions
	--to come from the backup node.
	--
	-- The backup node should be a valid
	-- provider for all subscriptions served
	-- by the failed node. (otherwise it
	-- wouldn't be a allowable backup node).
	update "_slony_cursos".sl_subscribe	       
	       set sub_provider=p_backup_node
	       from "_slony_cursos".sl_node
	       where sub_provider=p_failed_node
	       and sl_node.no_id=sub_receiver
	       and sl_node.no_failed=false;	

	update "_slony_cursos".sl_node
		   set no_active=false WHERE 
		   no_id=p_failed_node;

	-- Rewrite sl_listen table
	perform "_slony_cursos".RebuildListenEntries();


	return p_failed_node;
end;
$$;


ALTER FUNCTION _slony_cursos.failoverset_int(p_failed_node integer, p_backup_node integer, p_last_seqno bigint) OWNER TO postgres;

--
-- TOC entry 2689 (class 0 OID 0)
-- Dependencies: 293
-- Name: FUNCTION failoverset_int(p_failed_node integer, p_backup_node integer, p_last_seqno bigint); Type: COMMENT; Schema: _slony_cursos; Owner: postgres
--

COMMENT ON FUNCTION failoverset_int(p_failed_node integer, p_backup_node integer, p_last_seqno bigint) IS 'FUNCTION failoverSet_int (failed_node, backup_node, set_id, wait_seqno)

Finish failover for one set.';


--
-- TOC entry 361 (class 1255 OID 27682)
-- Name: finishtableaftercopy(integer); Type: FUNCTION; Schema: _slony_cursos; Owner: postgres
--

CREATE FUNCTION finishtableaftercopy(p_tab_id integer) RETURNS integer
    LANGUAGE plpgsql
    AS $$
declare
	v_tab_oid		oid;
	v_tab_fqname	text;
begin
	-- ----
	-- Get the tables OID and fully qualified name
	-- ---
	select	PGC.oid,
			"_slony_cursos".slon_quote_brute(PGN.nspname) || '.' ||
			"_slony_cursos".slon_quote_brute(PGC.relname) as tab_fqname
		into v_tab_oid, v_tab_fqname
			from "_slony_cursos".sl_table T,   
				"pg_catalog".pg_class PGC, "pg_catalog".pg_namespace PGN
				where T.tab_id = p_tab_id
				and T.tab_reloid = PGC.oid
				and PGC.relnamespace = PGN.oid;
	if not found then
		raise exception 'Table with ID % not found in sl_table', p_tab_id;
	end if;

	-- ----
	-- Reenable indexes and reindex the table.
	-- ----
	perform "_slony_cursos".enable_indexes_on_table(v_tab_oid);
	execute 'reindex table ' || "_slony_cursos".slon_quote_input(v_tab_fqname);

	return 1;
end;
$$;


ALTER FUNCTION _slony_cursos.finishtableaftercopy(p_tab_id integer) OWNER TO postgres;

--
-- TOC entry 2690 (class 0 OID 0)
-- Dependencies: 361
-- Name: FUNCTION finishtableaftercopy(p_tab_id integer); Type: COMMENT; Schema: _slony_cursos; Owner: postgres
--

COMMENT ON FUNCTION finishtableaftercopy(p_tab_id integer) IS 'Reenable index maintenance and reindex the table';


--
-- TOC entry 344 (class 1255 OID 27659)
-- Name: forwardconfirm(integer, integer, bigint, timestamp without time zone); Type: FUNCTION; Schema: _slony_cursos; Owner: postgres
--

CREATE FUNCTION forwardconfirm(p_con_origin integer, p_con_received integer, p_con_seqno bigint, p_con_timestamp timestamp without time zone) RETURNS bigint
    LANGUAGE plpgsql
    AS $$
declare
	v_max_seqno		bigint;
begin
	select into v_max_seqno coalesce(max(con_seqno), 0)
			from "_slony_cursos".sl_confirm
			where con_origin = p_con_origin
			and con_received = p_con_received;
	if v_max_seqno < p_con_seqno then
		insert into "_slony_cursos".sl_confirm 
				(con_origin, con_received, con_seqno, con_timestamp)
				values (p_con_origin, p_con_received, p_con_seqno,
					p_con_timestamp);
		v_max_seqno = p_con_seqno;
	end if;

	return v_max_seqno;
end;
$$;


ALTER FUNCTION _slony_cursos.forwardconfirm(p_con_origin integer, p_con_received integer, p_con_seqno bigint, p_con_timestamp timestamp without time zone) OWNER TO postgres;

--
-- TOC entry 2691 (class 0 OID 0)
-- Dependencies: 344
-- Name: FUNCTION forwardconfirm(p_con_origin integer, p_con_received integer, p_con_seqno bigint, p_con_timestamp timestamp without time zone); Type: COMMENT; Schema: _slony_cursos; Owner: postgres
--

COMMENT ON FUNCTION forwardconfirm(p_con_origin integer, p_con_received integer, p_con_seqno bigint, p_con_timestamp timestamp without time zone) IS 'forwardConfirm (p_con_origin, p_con_received, p_con_seqno, p_con_timestamp)

Confirms (recorded in sl_confirm) that items from p_con_origin up to
p_con_seqno have been received by node p_con_received as of
p_con_timestamp, and raises an event to forward this confirmation.';


--
-- TOC entry 349 (class 1255 OID 27664)
-- Name: generate_sync_event(interval); Type: FUNCTION; Schema: _slony_cursos; Owner: postgres
--

CREATE FUNCTION generate_sync_event(p_interval interval) RETURNS integer
    LANGUAGE plpgsql
    AS $$
declare
	v_node_row     record;

BEGIN
	select 1 into v_node_row from "_slony_cursos".sl_event 
       	  where ev_type = 'SYNC' and ev_origin = "_slony_cursos".getLocalNodeId('_slony_cursos')
          and ev_timestamp > now() - p_interval limit 1;
	if not found then
		-- If there has been no SYNC in the last interval, then push one
		perform "_slony_cursos".createEvent('_slony_cursos', 'SYNC', NULL);
		return 1;
	else
		return 0;
	end if;
end;
$$;


ALTER FUNCTION _slony_cursos.generate_sync_event(p_interval interval) OWNER TO postgres;

--
-- TOC entry 2692 (class 0 OID 0)
-- Dependencies: 349
-- Name: FUNCTION generate_sync_event(p_interval interval); Type: COMMENT; Schema: _slony_cursos; Owner: postgres
--

COMMENT ON FUNCTION generate_sync_event(p_interval interval) IS 'Generate a sync event if there has not been one in the requested interval, and this is a provider node.';


--
-- TOC entry 255 (class 1255 OID 27568)
-- Name: getlocalnodeid(name); Type: FUNCTION; Schema: _slony_cursos; Owner: postgres
--

CREATE FUNCTION getlocalnodeid(p_cluster name) RETURNS integer
    LANGUAGE c SECURITY DEFINER
    AS '$libdir/slony1_funcs.2.2.4', '_Slony_I_2_2_4_getLocalNodeId';


ALTER FUNCTION _slony_cursos.getlocalnodeid(p_cluster name) OWNER TO postgres;

--
-- TOC entry 2693 (class 0 OID 0)
-- Dependencies: 255
-- Name: FUNCTION getlocalnodeid(p_cluster name); Type: COMMENT; Schema: _slony_cursos; Owner: postgres
--

COMMENT ON FUNCTION getlocalnodeid(p_cluster name) IS 'Returns the node ID of the node being serviced on the local database';


--
-- TOC entry 256 (class 1255 OID 27569)
-- Name: getmoduleversion(); Type: FUNCTION; Schema: _slony_cursos; Owner: postgres
--

CREATE FUNCTION getmoduleversion() RETURNS text
    LANGUAGE c SECURITY DEFINER
    AS '$libdir/slony1_funcs.2.2.4', '_Slony_I_2_2_4_getModuleVersion';


ALTER FUNCTION _slony_cursos.getmoduleversion() OWNER TO postgres;

--
-- TOC entry 2695 (class 0 OID 0)
-- Dependencies: 256
-- Name: FUNCTION getmoduleversion(); Type: COMMENT; Schema: _slony_cursos; Owner: postgres
--

COMMENT ON FUNCTION getmoduleversion() IS 'Returns the compiled-in version number of the Slony-I shared object';


--
-- TOC entry 281 (class 1255 OID 27595)
-- Name: initializelocalnode(integer, text); Type: FUNCTION; Schema: _slony_cursos; Owner: postgres
--

CREATE FUNCTION initializelocalnode(p_local_node_id integer, p_comment text) RETURNS integer
    LANGUAGE plpgsql
    AS $$
declare
	v_old_node_id		int4;
	v_first_log_no		int4;
	v_event_seq			int8;
begin
	-- ----
	-- Make sure this node is uninitialized or got reset
	-- ----
	select last_value::int4 into v_old_node_id from "_slony_cursos".sl_local_node_id;
	if v_old_node_id != -1 then
		raise exception 'Slony-I: This node is already initialized';
	end if;

	-- ----
	-- Set sl_local_node_id to the requested value and add our
	-- own system to sl_node.
	-- ----
	perform setval('"_slony_cursos".sl_local_node_id', p_local_node_id);
	perform "_slony_cursos".storeNode_int (p_local_node_id, p_comment);

	if (pg_catalog.current_setting('max_identifier_length')::integer - pg_catalog.length('"_slony_cursos"')) < 5 then
		raise notice 'Slony-I: Cluster name length [%] versus system max_identifier_length [%] ', pg_catalog.length('"_slony_cursos"'), pg_catalog.current_setting('max_identifier_length');
		raise notice 'leaves narrow/no room for some Slony-I-generated objects (such as indexes).';
		raise notice 'You may run into problems later!';
	end if;
	
	--
	-- Put the apply trigger onto sl_log_1 and sl_log_2
	--
	create trigger apply_trigger
		before INSERT on "_slony_cursos".sl_log_1
		for each row execute procedure "_slony_cursos".logApply('_slony_cursos');
	alter table "_slony_cursos".sl_log_1
	  enable replica trigger apply_trigger;
	create trigger apply_trigger
		before INSERT on "_slony_cursos".sl_log_2
		for each row execute procedure "_slony_cursos".logApply('_slony_cursos');
	alter table "_slony_cursos".sl_log_2
			enable replica trigger apply_trigger;

	return p_local_node_id;
end;
$$;


ALTER FUNCTION _slony_cursos.initializelocalnode(p_local_node_id integer, p_comment text) OWNER TO postgres;

--
-- TOC entry 2697 (class 0 OID 0)
-- Dependencies: 281
-- Name: FUNCTION initializelocalnode(p_local_node_id integer, p_comment text); Type: COMMENT; Schema: _slony_cursos; Owner: postgres
--

COMMENT ON FUNCTION initializelocalnode(p_local_node_id integer, p_comment text) IS 'no_id - Node ID #
no_comment - Human-oriented comment

Initializes the new node, no_id';


--
-- TOC entry 372 (class 1255 OID 27694)
-- Name: is_node_reachable(integer, integer); Type: FUNCTION; Schema: _slony_cursos; Owner: postgres
--

CREATE FUNCTION is_node_reachable(origin_node_id integer, receiver_node_id integer) RETURNS boolean
    LANGUAGE plpgsql
    AS $$
declare
		listen_row record;
		reachable boolean;
begin
	reachable:=false;
	select * into listen_row from "_slony_cursos".sl_listen where
		   li_origin=origin_node_id and li_receiver=receiver_node_id;
	if found then
	   reachable:=true;
	end if;
  return reachable;
end $$;


ALTER FUNCTION _slony_cursos.is_node_reachable(origin_node_id integer, receiver_node_id integer) OWNER TO postgres;

--
-- TOC entry 2698 (class 0 OID 0)
-- Dependencies: 372
-- Name: FUNCTION is_node_reachable(origin_node_id integer, receiver_node_id integer); Type: COMMENT; Schema: _slony_cursos; Owner: postgres
--

COMMENT ON FUNCTION is_node_reachable(origin_node_id integer, receiver_node_id integer) IS 'Is the receiver node reachable from the origin, via any of the listen paths?';


--
-- TOC entry 315 (class 1255 OID 27631)
-- Name: issubscriptioninprogress(integer); Type: FUNCTION; Schema: _slony_cursos; Owner: postgres
--

CREATE FUNCTION issubscriptioninprogress(p_add_id integer) RETURNS boolean
    LANGUAGE plpgsql
    AS $$
begin
	if exists (select true from "_slony_cursos".sl_event
			where ev_type = 'ENABLE_SUBSCRIPTION'
			and ev_data1 = p_add_id::text
			and ev_seqno > (select max(con_seqno) from "_slony_cursos".sl_confirm
					where con_origin = ev_origin
					and con_received::text = ev_data3))
	then
		return true;
	else
		return false;
	end if;
end;
$$;


ALTER FUNCTION _slony_cursos.issubscriptioninprogress(p_add_id integer) OWNER TO postgres;

--
-- TOC entry 2699 (class 0 OID 0)
-- Dependencies: 315
-- Name: FUNCTION issubscriptioninprogress(p_add_id integer); Type: COMMENT; Schema: _slony_cursos; Owner: postgres
--

COMMENT ON FUNCTION issubscriptioninprogress(p_add_id integer) IS 'Checks to see if a subscription for the indicated set is in progress.
Returns true if a subscription is in progress. Otherwise false';


--
-- TOC entry 264 (class 1255 OID 27579)
-- Name: killbackend(integer, text); Type: FUNCTION; Schema: _slony_cursos; Owner: postgres
--

CREATE FUNCTION killbackend(p_pid integer, p_signame text) RETURNS integer
    LANGUAGE c
    AS '$libdir/slony1_funcs.2.2.4', '_Slony_I_2_2_4_killBackend';


ALTER FUNCTION _slony_cursos.killbackend(p_pid integer, p_signame text) OWNER TO postgres;

--
-- TOC entry 2700 (class 0 OID 0)
-- Dependencies: 264
-- Name: FUNCTION killbackend(p_pid integer, p_signame text); Type: COMMENT; Schema: _slony_cursos; Owner: postgres
--

COMMENT ON FUNCTION killbackend(p_pid integer, p_signame text) IS 'Send a signal to a postgres process. Requires superuser rights';


--
-- TOC entry 254 (class 1255 OID 27567)
-- Name: lockedset(); Type: FUNCTION; Schema: _slony_cursos; Owner: postgres
--

CREATE FUNCTION lockedset() RETURNS trigger
    LANGUAGE c
    AS '$libdir/slony1_funcs.2.2.4', '_Slony_I_2_2_4_lockedSet';


ALTER FUNCTION _slony_cursos.lockedset() OWNER TO postgres;

--
-- TOC entry 2701 (class 0 OID 0)
-- Dependencies: 254
-- Name: FUNCTION lockedset(); Type: COMMENT; Schema: _slony_cursos; Owner: postgres
--

COMMENT ON FUNCTION lockedset() IS 'Trigger function to prevent modifications to a table before and after a moveSet()';


--
-- TOC entry 308 (class 1255 OID 27623)
-- Name: lockset(integer); Type: FUNCTION; Schema: _slony_cursos; Owner: postgres
--

CREATE FUNCTION lockset(p_set_id integer) RETURNS integer
    LANGUAGE plpgsql
    AS $$
declare
	v_local_node_id		int4;
	v_set_row			record;
	v_tab_row			record;
begin
	-- ----
	-- Grab the central configuration lock
	-- ----
	lock table "_slony_cursos".sl_config_lock;

	-- ----
	-- Check that the set exists and that we are the origin
	-- and that it is not already locked.
	-- ----
	v_local_node_id := "_slony_cursos".getLocalNodeId('_slony_cursos');
	select * into v_set_row from "_slony_cursos".sl_set
			where set_id = p_set_id
			for update;
	if not found then
		raise exception 'Slony-I: set % not found', p_set_id;
	end if;
	if v_set_row.set_origin <> v_local_node_id then
		raise exception 'Slony-I: set % does not originate on local node',
				p_set_id;
	end if;
	if v_set_row.set_locked notnull then
		raise exception 'Slony-I: set % is already locked', p_set_id;
	end if;

	-- ----
	-- Place the lockedSet trigger on all tables in the set.
	-- ----
	for v_tab_row in select T.tab_id,
			"_slony_cursos".slon_quote_brute(PGN.nspname) || '.' ||
			"_slony_cursos".slon_quote_brute(PGC.relname) as tab_fqname
			from "_slony_cursos".sl_table T,
				"pg_catalog".pg_class PGC, "pg_catalog".pg_namespace PGN
			where T.tab_set = p_set_id
				and T.tab_reloid = PGC.oid
				and PGC.relnamespace = PGN.oid
			order by tab_id
	loop
		execute 'create trigger "_slony_cursos_lockedset" ' || 
				'before insert or update or delete on ' ||
				v_tab_row.tab_fqname || ' for each row execute procedure
				"_slony_cursos".lockedSet (''_slony_cursos'');';
	end loop;

	-- ----
	-- Remember our snapshots xmax as for the set locking
	-- ----
	update "_slony_cursos".sl_set
			set set_locked = "pg_catalog".txid_snapshot_xmax("pg_catalog".txid_current_snapshot())
			where set_id = p_set_id;

	return p_set_id;
end;
$$;


ALTER FUNCTION _slony_cursos.lockset(p_set_id integer) OWNER TO postgres;

--
-- TOC entry 2702 (class 0 OID 0)
-- Dependencies: 308
-- Name: FUNCTION lockset(p_set_id integer); Type: COMMENT; Schema: _slony_cursos; Owner: postgres
--

COMMENT ON FUNCTION lockset(p_set_id integer) IS 'lockSet(set_id)

Add a special trigger to all tables of a set that disables access to
it.';


--
-- TOC entry 369 (class 1255 OID 27691)
-- Name: log_truncate(); Type: FUNCTION; Schema: _slony_cursos; Owner: postgres
--

CREATE FUNCTION log_truncate() RETURNS trigger
    LANGUAGE plpgsql SECURITY DEFINER
    AS $$
	declare
		c_nspname text;
		c_relname text;
		c_log integer;
		c_node integer;
		c_tabid integer;
	begin
        c_tabid := tg_argv[0];
	    c_node := "_slony_cursos".getLocalNodeId('_slony_cursos');
		select tab_nspname, tab_relname into c_nspname, c_relname
				  from "_slony_cursos".sl_table where tab_id = c_tabid;
		select last_value into c_log from "_slony_cursos".sl_log_status;
		if c_log in (0, 2) then
			insert into "_slony_cursos".sl_log_1 (
					log_origin, log_txid, log_tableid, 
					log_actionseq, log_tablenspname, 
					log_tablerelname, log_cmdtype, 
					log_cmdupdncols, log_cmdargs
				) values (
					c_node, pg_catalog.txid_current(), c_tabid,
					nextval('"_slony_cursos".sl_action_seq'), c_nspname,
					c_relname, 'T', 0, '{}'::text[]);
		else   -- (1, 3) 
			insert into "_slony_cursos".sl_log_2 (
					log_origin, log_txid, log_tableid, 
					log_actionseq, log_tablenspname, 
					log_tablerelname, log_cmdtype, 
					log_cmdupdncols, log_cmdargs
				) values (
					c_node, pg_catalog.txid_current(), c_tabid,
					nextval('"_slony_cursos".sl_action_seq'), c_nspname,
					c_relname, 'T', 0, '{}'::text[]);
		end if;
		return NULL;
    end
$$;


ALTER FUNCTION _slony_cursos.log_truncate() OWNER TO postgres;

--
-- TOC entry 2703 (class 0 OID 0)
-- Dependencies: 369
-- Name: FUNCTION log_truncate(); Type: COMMENT; Schema: _slony_cursos; Owner: postgres
--

COMMENT ON FUNCTION log_truncate() IS 'trigger function run when a replicated table receives a TRUNCATE request';


--
-- TOC entry 258 (class 1255 OID 27571)
-- Name: logapply(); Type: FUNCTION; Schema: _slony_cursos; Owner: postgres
--

CREATE FUNCTION logapply() RETURNS trigger
    LANGUAGE c SECURITY DEFINER
    AS '$libdir/slony1_funcs.2.2.4', '_Slony_I_2_2_4_logApply';


ALTER FUNCTION _slony_cursos.logapply() OWNER TO postgres;

--
-- TOC entry 260 (class 1255 OID 27573)
-- Name: logapplysavestats(name, integer, interval); Type: FUNCTION; Schema: _slony_cursos; Owner: postgres
--

CREATE FUNCTION logapplysavestats(p_cluster name, p_origin integer, p_duration interval) RETURNS integer
    LANGUAGE c
    AS '$libdir/slony1_funcs.2.2.4', '_Slony_I_2_2_4_logApplySaveStats';


ALTER FUNCTION _slony_cursos.logapplysavestats(p_cluster name, p_origin integer, p_duration interval) OWNER TO postgres;

--
-- TOC entry 259 (class 1255 OID 27572)
-- Name: logapplysetcachesize(integer); Type: FUNCTION; Schema: _slony_cursos; Owner: postgres
--

CREATE FUNCTION logapplysetcachesize(p_size integer) RETURNS integer
    LANGUAGE c
    AS '$libdir/slony1_funcs.2.2.4', '_Slony_I_2_2_4_logApplySetCacheSize';


ALTER FUNCTION _slony_cursos.logapplysetcachesize(p_size integer) OWNER TO postgres;

--
-- TOC entry 353 (class 1255 OID 27668)
-- Name: logswitch_finish(); Type: FUNCTION; Schema: _slony_cursos; Owner: postgres
--

CREATE FUNCTION logswitch_finish() RETURNS integer
    LANGUAGE plpgsql
    AS $$
DECLARE
	v_current_status	int4;
	v_dummy				record;
	v_origin	int8;
	v_seqno		int8;
	v_xmin		bigint;
	v_purgeable boolean;
BEGIN
	-- ----
	-- Get the current log status.
	-- ----
	select last_value into v_current_status from "_slony_cursos".sl_log_status;

	-- ----
	-- status value 0 or 1 means that there is no log switch in progress
	-- ----
	if v_current_status = 0 or v_current_status = 1 then
		return 0;
	end if;

	-- ----
	-- status = 2: sl_log_1 active, cleanup sl_log_2
	-- ----
	if v_current_status = 2 then
		v_purgeable := 'true';
		
		-- ----
		-- Attempt to lock sl_log_2 in order to make sure there are no other transactions 
		-- currently writing to it. Exit if it is still in use. This prevents TRUNCATE from 
		-- blocking writers to sl_log_2 while it is waiting for a lock. It also prevents it 
		-- immediately truncating log data generated inside the transaction which was active 
		-- when logswitch_finish() was called (and was blocking TRUNCATE) as soon as that 
		-- transaction is committed.
		-- ----
		begin
			lock table "_slony_cursos".sl_log_2 in access exclusive mode nowait;
		exception when lock_not_available then
			raise notice 'Slony-I: could not lock sl_log_2 - sl_log_2 not truncated';
			return -1;
		end;

		-- ----
		-- The cleanup thread calls us after it did the delete and
		-- vacuum of both log tables. If sl_log_2 is empty now, we
		-- can truncate it and the log switch is done.
		-- ----
	        for v_origin, v_seqno, v_xmin in
		  select ev_origin, ev_seqno, "pg_catalog".txid_snapshot_xmin(ev_snapshot) from "_slony_cursos".sl_event
	          where (ev_origin, ev_seqno) in (select ev_origin, min(ev_seqno) from "_slony_cursos".sl_event where ev_type = 'SYNC' group by ev_origin)
		loop
			if exists (select 1 from "_slony_cursos".sl_log_2 where log_origin = v_origin and log_txid >= v_xmin limit 1) then
				v_purgeable := 'false';
			end if;
	        end loop;
		if not v_purgeable then
			-- ----
			-- Found a row ... log switch is still in progress.
			-- ----
			raise notice 'Slony-I: log switch to sl_log_1 still in progress - sl_log_2 not truncated';
			return -1;
		end if;

		raise notice 'Slony-I: log switch to sl_log_1 complete - truncate sl_log_2';
		truncate "_slony_cursos".sl_log_2;
		if exists (select * from "pg_catalog".pg_class c, "pg_catalog".pg_namespace n, "pg_catalog".pg_attribute a where c.relname = 'sl_log_2' and n.oid = c.relnamespace and a.attrelid = c.oid and a.attname = 'oid') then
	                execute 'alter table "_slony_cursos".sl_log_2 set without oids;';
		end if;		
		perform "pg_catalog".setval('"_slony_cursos".sl_log_status', 0);
		-- Run addPartialLogIndices() to try to add indices to unused sl_log_? table
		perform "_slony_cursos".addPartialLogIndices();

		return 1;
	end if;

	-- ----
	-- status = 3: sl_log_2 active, cleanup sl_log_1
	-- ----
	if v_current_status = 3 then
		v_purgeable := 'true';

		-- ----
		-- Attempt to lock sl_log_1 in order to make sure there are no other transactions 
		-- currently writing to it. Exit if it is still in use. This prevents TRUNCATE from 
		-- blocking writes to sl_log_1 while it is waiting for a lock. It also prevents it 
		-- immediately truncating log data generated inside the transaction which was active 
		-- when logswitch_finish() was called (and was blocking TRUNCATE) as soon as that 
		-- transaction is committed.
		-- ----
		begin
			lock table "_slony_cursos".sl_log_1 in access exclusive mode nowait;
		exception when lock_not_available then
			raise notice 'Slony-I: could not lock sl_log_1 - sl_log_1 not truncated';
			return -1;
		end;

		-- ----
		-- The cleanup thread calls us after it did the delete and
		-- vacuum of both log tables. If sl_log_2 is empty now, we
		-- can truncate it and the log switch is done.
		-- ----
	        for v_origin, v_seqno, v_xmin in
		  select ev_origin, ev_seqno, "pg_catalog".txid_snapshot_xmin(ev_snapshot) from "_slony_cursos".sl_event
	          where (ev_origin, ev_seqno) in (select ev_origin, min(ev_seqno) from "_slony_cursos".sl_event where ev_type = 'SYNC' group by ev_origin)
		loop
			if (exists (select 1 from "_slony_cursos".sl_log_1 where log_origin = v_origin and log_txid >= v_xmin limit 1)) then
				v_purgeable := 'false';
			end if;
	        end loop;
		if not v_purgeable then
			-- ----
			-- Found a row ... log switch is still in progress.
			-- ----
			raise notice 'Slony-I: log switch to sl_log_2 still in progress - sl_log_1 not truncated';
			return -1;
		end if;

		raise notice 'Slony-I: log switch to sl_log_2 complete - truncate sl_log_1';
		truncate "_slony_cursos".sl_log_1;
		if exists (select * from "pg_catalog".pg_class c, "pg_catalog".pg_namespace n, "pg_catalog".pg_attribute a where c.relname = 'sl_log_1' and n.oid = c.relnamespace and a.attrelid = c.oid and a.attname = 'oid') then
	                execute 'alter table "_slony_cursos".sl_log_1 set without oids;';
		end if;		
		perform "pg_catalog".setval('"_slony_cursos".sl_log_status', 1);
		-- Run addPartialLogIndices() to try to add indices to unused sl_log_? table
		perform "_slony_cursos".addPartialLogIndices();
		return 2;
	end if;
END;
$$;


ALTER FUNCTION _slony_cursos.logswitch_finish() OWNER TO postgres;

--
-- TOC entry 2704 (class 0 OID 0)
-- Dependencies: 353
-- Name: FUNCTION logswitch_finish(); Type: COMMENT; Schema: _slony_cursos; Owner: postgres
--

COMMENT ON FUNCTION logswitch_finish() IS 'logswitch_finish()

Attempt to finalize a log table switch in progress
return values:
  -1 if switch in progress, but not complete
   0 if no switch in progress
   1 if performed truncate on sl_log_2
   2 if performed truncate on sl_log_1
';


--
-- TOC entry 352 (class 1255 OID 27667)
-- Name: logswitch_start(); Type: FUNCTION; Schema: _slony_cursos; Owner: postgres
--

CREATE FUNCTION logswitch_start() RETURNS integer
    LANGUAGE plpgsql
    AS $$
DECLARE
	v_current_status	int4;
BEGIN
	-- ----
	-- Get the current log status.
	-- ----
	select last_value into v_current_status from "_slony_cursos".sl_log_status;

	-- ----
	-- status = 0: sl_log_1 active, sl_log_2 clean
	-- Initiate a switch to sl_log_2.
	-- ----
	if v_current_status = 0 then
		perform "pg_catalog".setval('"_slony_cursos".sl_log_status', 3);
		perform "_slony_cursos".registry_set_timestamp(
				'logswitch.laststart', now());
		raise notice 'Slony-I: Logswitch to sl_log_2 initiated';
		return 2;
	end if;

	-- ----
	-- status = 1: sl_log_2 active, sl_log_1 clean
	-- Initiate a switch to sl_log_1.
	-- ----
	if v_current_status = 1 then
		perform "pg_catalog".setval('"_slony_cursos".sl_log_status', 2);
		perform "_slony_cursos".registry_set_timestamp(
				'logswitch.laststart', now());
		raise notice 'Slony-I: Logswitch to sl_log_1 initiated';
		return 1;
	end if;

	raise exception 'Previous logswitch still in progress';
END;
$$;


ALTER FUNCTION _slony_cursos.logswitch_start() OWNER TO postgres;

--
-- TOC entry 2705 (class 0 OID 0)
-- Dependencies: 352
-- Name: FUNCTION logswitch_start(); Type: COMMENT; Schema: _slony_cursos; Owner: postgres
--

COMMENT ON FUNCTION logswitch_start() IS 'logswitch_start()

Initiate a log table switch if none is in progress';


--
-- TOC entry 233 (class 1255 OID 27577)
-- Name: logtrigger(); Type: FUNCTION; Schema: _slony_cursos; Owner: postgres
--

CREATE FUNCTION logtrigger() RETURNS trigger
    LANGUAGE c SECURITY DEFINER
    AS '$libdir/slony1_funcs.2.2.4', '_Slony_I_2_2_4_logTrigger';


ALTER FUNCTION _slony_cursos.logtrigger() OWNER TO postgres;

--
-- TOC entry 2706 (class 0 OID 0)
-- Dependencies: 233
-- Name: FUNCTION logtrigger(); Type: COMMENT; Schema: _slony_cursos; Owner: postgres
--

COMMENT ON FUNCTION logtrigger() IS 'This is the trigger that is executed on the origin node that causes
updates to be recorded in sl_log_1/sl_log_2.';


--
-- TOC entry 314 (class 1255 OID 27630)
-- Name: mergeset(integer, integer); Type: FUNCTION; Schema: _slony_cursos; Owner: postgres
--

CREATE FUNCTION mergeset(p_set_id integer, p_add_id integer) RETURNS bigint
    LANGUAGE plpgsql
    AS $$
declare
	v_origin			int4;
	in_progress			boolean;
begin
	-- ----
	-- Grab the central configuration lock
	-- ----
	lock table "_slony_cursos".sl_config_lock;

	-- ----
	-- Check that both sets exist and originate here
	-- ----
	if p_set_id = p_add_id then
		raise exception 'Slony-I: merged set ids cannot be identical';
	end if;
	select set_origin into v_origin from "_slony_cursos".sl_set
			where set_id = p_set_id;
	if not found then
		raise exception 'Slony-I: set % not found', p_set_id;
	end if;
	if v_origin != "_slony_cursos".getLocalNodeId('_slony_cursos') then
		raise exception 'Slony-I: set % does not originate on local node',
				p_set_id;
	end if;

	select set_origin into v_origin from "_slony_cursos".sl_set
			where set_id = p_add_id;
	if not found then
		raise exception 'Slony-I: set % not found', p_add_id;
	end if;
	if v_origin != "_slony_cursos".getLocalNodeId('_slony_cursos') then
		raise exception 'Slony-I: set % does not originate on local node',
				p_add_id;
	end if;

	-- ----
	-- Check that both sets are subscribed by the same set of nodes
	-- ----
	if exists (select true from "_slony_cursos".sl_subscribe SUB1
				where SUB1.sub_set = p_set_id
				and SUB1.sub_receiver not in (select SUB2.sub_receiver
						from "_slony_cursos".sl_subscribe SUB2
						where SUB2.sub_set = p_add_id))
	then
		raise exception 'Slony-I: subscriber lists of set % and % are different',
				p_set_id, p_add_id;
	end if;

	if exists (select true from "_slony_cursos".sl_subscribe SUB1
				where SUB1.sub_set = p_add_id
				and SUB1.sub_receiver not in (select SUB2.sub_receiver
						from "_slony_cursos".sl_subscribe SUB2
						where SUB2.sub_set = p_set_id))
	then
		raise exception 'Slony-I: subscriber lists of set % and % are different',
				p_add_id, p_set_id;
	end if;

	-- ----
	-- Check that all ENABLE_SUBSCRIPTION events for the set are confirmed
	-- ----
	select "_slony_cursos".isSubscriptionInProgress(p_add_id) into in_progress ;
	
	if in_progress then
		raise exception 'Slony-I: set % has subscriptions in progress - cannot merge',
				p_add_id;
	end if;

	-- ----
	-- Create a SYNC event, merge the sets, create a MERGE_SET event
	-- ----
	perform "_slony_cursos".createEvent('_slony_cursos', 'SYNC', NULL);
	perform "_slony_cursos".mergeSet_int(p_set_id, p_add_id);
	return  "_slony_cursos".createEvent('_slony_cursos', 'MERGE_SET', 
			p_set_id::text, p_add_id::text);
end;
$$;


ALTER FUNCTION _slony_cursos.mergeset(p_set_id integer, p_add_id integer) OWNER TO postgres;

--
-- TOC entry 2708 (class 0 OID 0)
-- Dependencies: 314
-- Name: FUNCTION mergeset(p_set_id integer, p_add_id integer); Type: COMMENT; Schema: _slony_cursos; Owner: postgres
--

COMMENT ON FUNCTION mergeset(p_set_id integer, p_add_id integer) IS 'Generate MERGE_SET event to request that sets be merged together.

Both sets must exist, and originate on the same node.  They must be
subscribed by the same set of nodes.';


--
-- TOC entry 316 (class 1255 OID 27632)
-- Name: mergeset_int(integer, integer); Type: FUNCTION; Schema: _slony_cursos; Owner: postgres
--

CREATE FUNCTION mergeset_int(p_set_id integer, p_add_id integer) RETURNS integer
    LANGUAGE plpgsql
    AS $$
begin
	-- ----
	-- Grab the central configuration lock
	-- ----
	lock table "_slony_cursos".sl_config_lock;

	update "_slony_cursos".sl_sequence
			set seq_set = p_set_id
			where seq_set = p_add_id;
	update "_slony_cursos".sl_table
			set tab_set = p_set_id
			where tab_set = p_add_id;
	delete from "_slony_cursos".sl_subscribe
			where sub_set = p_add_id;
	delete from "_slony_cursos".sl_setsync
			where ssy_setid = p_add_id;
	delete from "_slony_cursos".sl_set
			where set_id = p_add_id;

	return p_set_id;
end;
$$;


ALTER FUNCTION _slony_cursos.mergeset_int(p_set_id integer, p_add_id integer) OWNER TO postgres;

--
-- TOC entry 2709 (class 0 OID 0)
-- Dependencies: 316
-- Name: FUNCTION mergeset_int(p_set_id integer, p_add_id integer); Type: COMMENT; Schema: _slony_cursos; Owner: postgres
--

COMMENT ON FUNCTION mergeset_int(p_set_id integer, p_add_id integer) IS 'mergeSet_int(set_id, add_id) - Perform MERGE_SET event, merging all objects from 
set add_id into set set_id.';


--
-- TOC entry 310 (class 1255 OID 27625)
-- Name: moveset(integer, integer); Type: FUNCTION; Schema: _slony_cursos; Owner: postgres
--

CREATE FUNCTION moveset(p_set_id integer, p_new_origin integer) RETURNS bigint
    LANGUAGE plpgsql
    AS $$
declare
	v_local_node_id		int4;
	v_set_row			record;
	v_sub_row			record;
	v_sync_seqno		int8;
	v_lv_row			record;
begin
	-- ----
	-- Grab the central configuration lock
	-- ----
	lock table "_slony_cursos".sl_config_lock;

	-- ----
	-- Check that the set is locked and that this locking
	-- happened long enough ago.
	-- ----
	v_local_node_id := "_slony_cursos".getLocalNodeId('_slony_cursos');
	select * into v_set_row from "_slony_cursos".sl_set
			where set_id = p_set_id
			for update;
	if not found then
		raise exception 'Slony-I: set % not found', p_set_id;
	end if;
	if v_set_row.set_origin <> v_local_node_id then
		raise exception 'Slony-I: set % does not originate on local node',
				p_set_id;
	end if;
	if v_set_row.set_locked isnull then
		raise exception 'Slony-I: set % is not locked', p_set_id;
	end if;
	if v_set_row.set_locked > "pg_catalog".txid_snapshot_xmin("pg_catalog".txid_current_snapshot()) then
		raise exception 'Slony-I: cannot move set % yet, transactions < % are still in progress',
				p_set_id, v_set_row.set_locked;
	end if;

	-- ----
	-- Unlock the set
	-- ----
	perform "_slony_cursos".unlockSet(p_set_id);

	-- ----
	-- Check that the new_origin is an active subscriber of the set
	-- ----
	select * into v_sub_row from "_slony_cursos".sl_subscribe
			where sub_set = p_set_id
			and sub_receiver = p_new_origin;
	if not found then
		raise exception 'Slony-I: set % is not subscribed by node %',
				p_set_id, p_new_origin;
	end if;
	if not v_sub_row.sub_active then
		raise exception 'Slony-I: subsctiption of node % for set % is inactive',
				p_new_origin, p_set_id;
	end if;

	-- ----
	-- Reconfigure everything
	-- ----
	perform "_slony_cursos".moveSet_int(p_set_id, v_local_node_id,
			p_new_origin, 0);

	perform "_slony_cursos".RebuildListenEntries();

	-- ----
	-- At this time we hold access exclusive locks for every table
	-- in the set. But we did move the set to the new origin, so the
	-- createEvent() we are doing now will not record the sequences.
	-- ----
	v_sync_seqno := "_slony_cursos".createEvent('_slony_cursos', 'SYNC');
	insert into "_slony_cursos".sl_seqlog 
			(seql_seqid, seql_origin, seql_ev_seqno, seql_last_value)
			select seq_id, v_local_node_id, v_sync_seqno, seq_last_value
			from "_slony_cursos".sl_seqlastvalue
			where seq_set = p_set_id;
					
	-- ----
	-- Finally we generate the real event
	-- ----
	return "_slony_cursos".createEvent('_slony_cursos', 'MOVE_SET', 
			p_set_id::text, v_local_node_id::text, p_new_origin::text);
end;
$$;


ALTER FUNCTION _slony_cursos.moveset(p_set_id integer, p_new_origin integer) OWNER TO postgres;

--
-- TOC entry 2710 (class 0 OID 0)
-- Dependencies: 310
-- Name: FUNCTION moveset(p_set_id integer, p_new_origin integer); Type: COMMENT; Schema: _slony_cursos; Owner: postgres
--

COMMENT ON FUNCTION moveset(p_set_id integer, p_new_origin integer) IS 'moveSet(set_id, new_origin)

Generate MOVE_SET event to request that the origin for set set_id be moved to node new_origin';


--
-- TOC entry 311 (class 1255 OID 27626)
-- Name: moveset_int(integer, integer, integer, bigint); Type: FUNCTION; Schema: _slony_cursos; Owner: postgres
--

CREATE FUNCTION moveset_int(p_set_id integer, p_old_origin integer, p_new_origin integer, p_wait_seqno bigint) RETURNS integer
    LANGUAGE plpgsql
    AS $$
declare
	v_local_node_id		int4;
	v_tab_row			record;
	v_sub_row			record;
	v_sub_node			int4;
	v_sub_last			int4;
	v_sub_next			int4;
	v_last_sync			int8;
begin
	-- ----
	-- Grab the central configuration lock
	-- ----
	lock table "_slony_cursos".sl_config_lock;

	-- ----
	-- Get our local node ID
	-- ----
	v_local_node_id := "_slony_cursos".getLocalNodeId('_slony_cursos');

	-- On the new origin, raise an event - ACCEPT_SET
	if v_local_node_id = p_new_origin then
		-- Create a SYNC event as well so that the ACCEPT_SET has
		-- the same snapshot as the last SYNC generated by the new
		-- origin. This snapshot will be used by other nodes to
		-- finalize the setsync status.
		perform "_slony_cursos".createEvent('_slony_cursos', 'SYNC', NULL);
		perform "_slony_cursos".createEvent('_slony_cursos', 'ACCEPT_SET', 
			p_set_id::text, p_old_origin::text, 
			p_new_origin::text, p_wait_seqno::text);
	end if;

	-- ----
	-- Next we have to reverse the subscription path
	-- ----
	v_sub_last = p_new_origin;
	select sub_provider into v_sub_node
			from "_slony_cursos".sl_subscribe
			where sub_set = p_set_id
			and sub_receiver = p_new_origin;
	if not found then
		raise exception 'Slony-I: subscription path broken in moveSet_int';
	end if;
	while v_sub_node <> p_old_origin loop
		-- ----
		-- Tracing node by node, the old receiver is now in
		-- v_sub_last and the old provider is in v_sub_node.
		-- ----

		-- ----
		-- Get the current provider of this node as next
		-- and change the provider to the previous one in
		-- the reverse chain.
		-- ----
		select sub_provider into v_sub_next
				from "_slony_cursos".sl_subscribe
				where sub_set = p_set_id
					and sub_receiver = v_sub_node
				for update;
		if not found then
			raise exception 'Slony-I: subscription path broken in moveSet_int';
		end if;
		update "_slony_cursos".sl_subscribe
				set sub_provider = v_sub_last
				where sub_set = p_set_id
					and sub_receiver = v_sub_node
					and sub_receiver <> v_sub_last;

		v_sub_last = v_sub_node;
		v_sub_node = v_sub_next;
	end loop;

	-- ----
	-- This includes creating a subscription for the old origin
	-- ----
	insert into "_slony_cursos".sl_subscribe
			(sub_set, sub_provider, sub_receiver,
			sub_forward, sub_active)
			values (p_set_id, v_sub_last, p_old_origin, true, true);
	if v_local_node_id = p_old_origin then
		select coalesce(max(ev_seqno), 0) into v_last_sync 
				from "_slony_cursos".sl_event
				where ev_origin = p_new_origin
					and ev_type = 'SYNC';
		if v_last_sync > 0 then
			insert into "_slony_cursos".sl_setsync
					(ssy_setid, ssy_origin, ssy_seqno,
					ssy_snapshot, ssy_action_list)
					select p_set_id, p_new_origin, v_last_sync,
					ev_snapshot, NULL
					from "_slony_cursos".sl_event
					where ev_origin = p_new_origin
						and ev_seqno = v_last_sync;
		else
			insert into "_slony_cursos".sl_setsync
					(ssy_setid, ssy_origin, ssy_seqno,
					ssy_snapshot, ssy_action_list)
					values (p_set_id, p_new_origin, '0',
					'1:1:', NULL);
		end if;
	end if;

	-- ----
	-- Now change the ownership of the set.
	-- ----
	update "_slony_cursos".sl_set
			set set_origin = p_new_origin
			where set_id = p_set_id;

	-- ----
	-- On the new origin, delete the obsolete setsync information
	-- and the subscription.
	-- ----
	if v_local_node_id = p_new_origin then
		delete from "_slony_cursos".sl_setsync
				where ssy_setid = p_set_id;
	else
		if v_local_node_id <> p_old_origin then
			--
			-- On every other node, change the setsync so that it will
			-- pick up from the new origins last known sync.
			--
			delete from "_slony_cursos".sl_setsync
					where ssy_setid = p_set_id;
			select coalesce(max(ev_seqno), 0) into v_last_sync
					from "_slony_cursos".sl_event
					where ev_origin = p_new_origin
						and ev_type = 'SYNC';
			if v_last_sync > 0 then
				insert into "_slony_cursos".sl_setsync
						(ssy_setid, ssy_origin, ssy_seqno,
						ssy_snapshot, ssy_action_list)
						select p_set_id, p_new_origin, v_last_sync,
						ev_snapshot, NULL
						from "_slony_cursos".sl_event
						where ev_origin = p_new_origin
							and ev_seqno = v_last_sync;
			else
				insert into "_slony_cursos".sl_setsync
						(ssy_setid, ssy_origin, ssy_seqno,
						ssy_snapshot, ssy_action_list)
						values (p_set_id, p_new_origin,
						'0', '1:1:', NULL);
			end if;
		end if;
	end if;
	delete from "_slony_cursos".sl_subscribe
			where sub_set = p_set_id
			and sub_receiver = p_new_origin;

	-- Regenerate sl_listen since we revised the subscriptions
	perform "_slony_cursos".RebuildListenEntries();

	-- Run addPartialLogIndices() to try to add indices to unused sl_log_? table
	perform "_slony_cursos".addPartialLogIndices();

	-- ----
	-- If we are the new or old origin, we have to
	-- adjust the log and deny access trigger configuration.
	-- ----
	if v_local_node_id = p_old_origin or v_local_node_id = p_new_origin then
		for v_tab_row in select tab_id from "_slony_cursos".sl_table
				where tab_set = p_set_id
				order by tab_id
		loop
			perform "_slony_cursos".alterTableConfigureTriggers(v_tab_row.tab_id);
		end loop;
	end if;

	return p_set_id;
end;
$$;


ALTER FUNCTION _slony_cursos.moveset_int(p_set_id integer, p_old_origin integer, p_new_origin integer, p_wait_seqno bigint) OWNER TO postgres;

--
-- TOC entry 2711 (class 0 OID 0)
-- Dependencies: 311
-- Name: FUNCTION moveset_int(p_set_id integer, p_old_origin integer, p_new_origin integer, p_wait_seqno bigint); Type: COMMENT; Schema: _slony_cursos; Owner: postgres
--

COMMENT ON FUNCTION moveset_int(p_set_id integer, p_old_origin integer, p_new_origin integer, p_wait_seqno bigint) IS 'moveSet(set_id, old_origin, new_origin, wait_seqno)

Process MOVE_SET event to request that the origin for set set_id be
moved from old_origin to node new_origin';


--
-- TOC entry 289 (class 1255 OID 27604)
-- Name: prefailover(integer, boolean); Type: FUNCTION; Schema: _slony_cursos; Owner: postgres
--

CREATE FUNCTION prefailover(p_failed_node integer, p_is_candidate boolean) RETURNS integer
    LANGUAGE plpgsql
    AS $$
declare
	v_row				record;
	v_row2				record;
	v_n					int4;
begin
	-- ----
	-- Grab the central configuration lock
	-- ----
	lock table "_slony_cursos".sl_config_lock;

	-- ----
	-- All consistency checks first

	if p_is_candidate then
	   -- ----
	   -- Check all sets originating on the failed node
	   -- ----
	   for v_row in select set_id
			from "_slony_cursos".sl_set
			where set_origin = p_failed_node
		loop
				-- ----
				-- Check that the backup node is subscribed to all sets
				-- that originate on the failed node
				-- ----
				select into v_row2 sub_forward, sub_active
					   from "_slony_cursos".sl_subscribe
					   where sub_set = v_row.set_id
					and sub_receiver = "_slony_cursos".getLocalNodeId('_slony_cursos');
				if not found then
				   raise exception 'Slony-I: cannot failover - node % is not subscribed to set %',
					"_slony_cursos".getLocalNodeId('_slony_cursos'), v_row.set_id;
				end if;

				-- ----
				-- Check that the subscription is active
				-- ----
				if not v_row2.sub_active then
				   raise exception 'Slony-I: cannot failover - subscription for set % is not active',
					v_row.set_id;
				end if;

				-- ----
				-- If there are other subscribers, the backup node needs to
				-- be a forwarder too.
				-- ----
				select into v_n count(*)
					   from "_slony_cursos".sl_subscribe
					   where sub_set = v_row.set_id
					   and sub_receiver <> "_slony_cursos".getLocalNodeId('_slony_cursos');
				if v_n > 0 and not v_row2.sub_forward then
				raise exception 'Slony-I: cannot failover - node % is not a forwarder of set %',
					 "_slony_cursos".getLocalNodeId('_slony_cursos'), v_row.set_id;
				end if;
			end loop;
	end if;

	-- ----
	-- Terminate all connections of the failed node the hard way
	-- ----
	perform "_slony_cursos".terminateNodeConnections(p_failed_node);

	update "_slony_cursos".sl_path set pa_conninfo='<event pending>' WHERE
	   		  pa_server=p_failed_node;	
	notify "_slony_cursos_Restart";
	-- ----
	-- That is it - so far.
	-- ----
	return p_failed_node;
end;
$$;


ALTER FUNCTION _slony_cursos.prefailover(p_failed_node integer, p_is_candidate boolean) OWNER TO postgres;

--
-- TOC entry 2712 (class 0 OID 0)
-- Dependencies: 289
-- Name: FUNCTION prefailover(p_failed_node integer, p_is_candidate boolean); Type: COMMENT; Schema: _slony_cursos; Owner: postgres
--

COMMENT ON FUNCTION prefailover(p_failed_node integer, p_is_candidate boolean) IS 'Prepare for a failover.  This function is called on all candidate nodes.
It blanks the paths to the failed node
and then restart of all node daemons.';


--
-- TOC entry 360 (class 1255 OID 27681)
-- Name: preparetableforcopy(integer); Type: FUNCTION; Schema: _slony_cursos; Owner: postgres
--

CREATE FUNCTION preparetableforcopy(p_tab_id integer) RETURNS integer
    LANGUAGE plpgsql
    AS $$
declare
	v_tab_oid		oid;
	v_tab_fqname	text;
begin
	-- ----
	-- Get the OID and fully qualified name for the table
	-- ---
	select	PGC.oid,
			"_slony_cursos".slon_quote_brute(PGN.nspname) || '.' ||
			"_slony_cursos".slon_quote_brute(PGC.relname) as tab_fqname
		into v_tab_oid, v_tab_fqname
			from "_slony_cursos".sl_table T,   
				"pg_catalog".pg_class PGC, "pg_catalog".pg_namespace PGN
				where T.tab_id = p_tab_id
				and T.tab_reloid = PGC.oid
				and PGC.relnamespace = PGN.oid;
	if not found then
		raise exception 'Table with ID % not found in sl_table', p_tab_id;
	end if;

	-- ----
	-- Try using truncate to empty the table and fallback to
	-- delete on error.
	-- ----
	perform "_slony_cursos".TruncateOnlyTable(v_tab_fqname);
	raise notice 'truncate of % succeeded', v_tab_fqname;

	-- suppress index activity
        perform "_slony_cursos".disable_indexes_on_table(v_tab_oid);

	return 1;
	exception when others then
		raise notice 'truncate of % failed - doing delete', v_tab_fqname;
		perform "_slony_cursos".disable_indexes_on_table(v_tab_oid);
		execute 'delete from only ' || "_slony_cursos".slon_quote_input(v_tab_fqname);
		return 0;
end;
$$;


ALTER FUNCTION _slony_cursos.preparetableforcopy(p_tab_id integer) OWNER TO postgres;

--
-- TOC entry 2713 (class 0 OID 0)
-- Dependencies: 360
-- Name: FUNCTION preparetableforcopy(p_tab_id integer); Type: COMMENT; Schema: _slony_cursos; Owner: postgres
--

COMMENT ON FUNCTION preparetableforcopy(p_tab_id integer) IS 'Delete all data and suppress index maintenance';


--
-- TOC entry 348 (class 1255 OID 27663)
-- Name: rebuildlistenentries(); Type: FUNCTION; Schema: _slony_cursos; Owner: postgres
--

CREATE FUNCTION rebuildlistenentries() RETURNS integer
    LANGUAGE plpgsql
    AS $$
declare
	v_row	record;
        v_cnt  integer;
begin
	-- ----
	-- Grab the central configuration lock
	-- ----
	lock table "_slony_cursos".sl_config_lock;

	-- First remove the entire configuration
	delete from "_slony_cursos".sl_listen;

	-- Second populate the sl_listen configuration with a full
	-- network of all possible paths.
	insert into "_slony_cursos".sl_listen
				(li_origin, li_provider, li_receiver)
			select pa_server, pa_server, pa_client from "_slony_cursos".sl_path;
	while true loop
		insert into "_slony_cursos".sl_listen
					(li_origin, li_provider, li_receiver)
			select distinct li_origin, pa_server, pa_client
				from "_slony_cursos".sl_listen, "_slony_cursos".sl_path
				where li_receiver = pa_server
				  and li_origin <> pa_client
				  and pa_conninfo<>'<event pending>'
			except
			select li_origin, li_provider, li_receiver
				from "_slony_cursos".sl_listen;

		if not found then
			exit;
		end if;
	end loop;

	-- We now replace specific event-origin,receiver combinations
	-- with a configuration that tries to avoid events arriving at
	-- a node before the data provider actually has the data ready.

	-- Loop over every possible pair of receiver and event origin
	for v_row in select N1.no_id as receiver, N2.no_id as origin,
			  N2.no_failed as failed
			from "_slony_cursos".sl_node as N1, "_slony_cursos".sl_node as N2
			where N1.no_id <> N2.no_id
	loop
		-- 1st choice:
		-- If we use the event origin as a data provider for any
		-- set that originates on that very node, we are a direct
		-- subscriber to that origin and listen there only.
		if exists (select true from "_slony_cursos".sl_set, "_slony_cursos".sl_subscribe				, "_slony_cursos".sl_node p		   		
				where set_origin = v_row.origin
				  and sub_set = set_id
				  and sub_provider = v_row.origin
				  and sub_receiver = v_row.receiver
				  and sub_active
				  and p.no_active
				  and p.no_id=sub_provider
				  )
		then
			delete from "_slony_cursos".sl_listen
				where li_origin = v_row.origin
				  and li_receiver = v_row.receiver;
			insert into "_slony_cursos".sl_listen (li_origin, li_provider, li_receiver)
				values (v_row.origin, v_row.origin, v_row.receiver);
		
		-- 2nd choice:
		-- If we are subscribed to any set originating on this
		-- event origin, we want to listen on all data providers
		-- we use for this origin. We are a cascaded subscriber
		-- for sets from this node.
		else
				if exists (select true from "_slony_cursos".sl_set, "_slony_cursos".sl_subscribe,
				   	  	       "_slony_cursos".sl_node provider
						where set_origin = v_row.origin
						  and sub_set = set_id
						  and sub_provider=provider.no_id
						  and provider.no_failed = false
						  and sub_receiver = v_row.receiver
						  and sub_active)
				then
						delete from "_slony_cursos".sl_listen
							   where li_origin = v_row.origin
					  		   and li_receiver = v_row.receiver;
						insert into "_slony_cursos".sl_listen (li_origin, li_provider, li_receiver)
						select distinct set_origin, sub_provider, v_row.receiver
							   from "_slony_cursos".sl_set, "_slony_cursos".sl_subscribe
						where set_origin = v_row.origin
						  and sub_set = set_id
						  and sub_receiver = v_row.receiver
						  and sub_active;
				end if;
		end if;

		if v_row.failed then		

		--for every failed node we delete all sl_listen entries
		--except via providers (listed in sl_subscribe)
		--or failover candidates (sl_failover_targets)
		--we do this to prevent a non-failover candidate
		--that is more ahead of the failover candidate from
		--sending events to the failover candidate that
		--are 'too far ahead'

		--if the failed node is not an origin for any
                --node then we don't delete all listen paths
		--for events from it.  Instead we leave
                --the listen network alone.
		
		select count(*) into v_cnt from "_slony_cursos".sl_subscribe sub,
		       "_slony_cursos".sl_set s
                       where s.set_origin=v_row.origin and s.set_id=sub.sub_set;
                if v_cnt > 0 then
		    delete from "_slony_cursos".sl_listen where
			   li_origin=v_row.origin and
			   li_receiver=v_row.receiver			
			   and li_provider not in 
			       (select sub_provider from
			       "_slony_cursos".sl_subscribe,
			       "_slony_cursos".sl_set where
			       sub_set=set_id
			       and set_origin=v_row.origin);
		    end if;
               end if;
--		   insert into "_slony_cursos".sl_listen
--		   		  (li_origin,li_provider,li_receiver)
--				  SELECT v_row.origin, pa_server
--				  ,v_row.receiver
--				  FROM "_slony_cursos".sl_path where
--				  	   pa_client=v_row.receiver
--				  and (v_row.origin,pa_server,v_row.receiver) not in
--				  	  		(select li_origin,li_provider,li_receiver
--					  		from "_slony_cursos".sl_listen);
--		end if;
	end loop ;

	return null ;
end ;
$$;


ALTER FUNCTION _slony_cursos.rebuildlistenentries() OWNER TO postgres;

--
-- TOC entry 2714 (class 0 OID 0)
-- Dependencies: 348
-- Name: FUNCTION rebuildlistenentries(); Type: COMMENT; Schema: _slony_cursos; Owner: postgres
--

COMMENT ON FUNCTION rebuildlistenentries() IS 'RebuildListenEntries()

Invoked by various subscription and path modifying functions, this
rewrites the sl_listen entries, adding in all the ones required to
allow communications between nodes in the Slony-I cluster.';


--
-- TOC entry 374 (class 1255 OID 27696)
-- Name: recreate_log_trigger(text, oid, text); Type: FUNCTION; Schema: _slony_cursos; Owner: postgres
--

CREATE FUNCTION recreate_log_trigger(p_fq_table_name text, p_tab_id oid, p_tab_attkind text) RETURNS integer
    LANGUAGE plpgsql
    AS $$
begin
	execute 'drop trigger "_slony_cursos_logtrigger" on ' ||
		p_fq_table_name	;
		-- ----
	execute 'create trigger "_slony_cursos_logtrigger"' || 
			' after insert or update or delete on ' ||
			p_fq_table_name 
			|| ' for each row execute procedure "_slony_cursos".logTrigger (' ||
                               pg_catalog.quote_literal('_slony_cursos') || ',' || 
				pg_catalog.quote_literal(p_tab_id::text) || ',' || 
				pg_catalog.quote_literal(p_tab_attkind) || ');';
	return 0;
end
$$;


ALTER FUNCTION _slony_cursos.recreate_log_trigger(p_fq_table_name text, p_tab_id oid, p_tab_attkind text) OWNER TO postgres;

--
-- TOC entry 2715 (class 0 OID 0)
-- Dependencies: 374
-- Name: FUNCTION recreate_log_trigger(p_fq_table_name text, p_tab_id oid, p_tab_attkind text); Type: COMMENT; Schema: _slony_cursos; Owner: postgres
--

COMMENT ON FUNCTION recreate_log_trigger(p_fq_table_name text, p_tab_id oid, p_tab_attkind text) IS 'A function that drops and recreates the log trigger on the specified table.
It is intended to be used after the primary_key/unique index has changed.';


--
-- TOC entry 280 (class 1255 OID 27594)
-- Name: registernodeconnection(integer); Type: FUNCTION; Schema: _slony_cursos; Owner: postgres
--

CREATE FUNCTION registernodeconnection(p_nodeid integer) RETURNS integer
    LANGUAGE plpgsql
    AS $$
begin
	insert into "_slony_cursos".sl_nodelock
		(nl_nodeid, nl_backendpid)
		values
		(p_nodeid, pg_backend_pid());

	return 0;
end;
$$;


ALTER FUNCTION _slony_cursos.registernodeconnection(p_nodeid integer) OWNER TO postgres;

--
-- TOC entry 2716 (class 0 OID 0)
-- Dependencies: 280
-- Name: FUNCTION registernodeconnection(p_nodeid integer); Type: COMMENT; Schema: _slony_cursos; Owner: postgres
--

COMMENT ON FUNCTION registernodeconnection(p_nodeid integer) IS 'Register (uniquely) the node connection so that only one slon can service the node';


--
-- TOC entry 273 (class 1255 OID 27588)
-- Name: registry_get_int4(text, integer); Type: FUNCTION; Schema: _slony_cursos; Owner: postgres
--

CREATE FUNCTION registry_get_int4(p_key text, p_default integer) RETURNS integer
    LANGUAGE plpgsql
    AS $$
DECLARE
	v_value		int4;
BEGIN
	select reg_int4 into v_value from "_slony_cursos".sl_registry
			where reg_key = p_key;
	if not found then 
		v_value = p_default;
		if p_default notnull then
			perform "_slony_cursos".registry_set_int4(p_key, p_default);
		end if;
	else
		if v_value is null then
			raise exception 'Slony-I: registry key % is not an int4 value',
					p_key;
		end if;
	end if;
	return v_value;
END;
$$;


ALTER FUNCTION _slony_cursos.registry_get_int4(p_key text, p_default integer) OWNER TO postgres;

--
-- TOC entry 2717 (class 0 OID 0)
-- Dependencies: 273
-- Name: FUNCTION registry_get_int4(p_key text, p_default integer); Type: COMMENT; Schema: _slony_cursos; Owner: postgres
--

COMMENT ON FUNCTION registry_get_int4(p_key text, p_default integer) IS 'registry_get_int4(key, value)

Get a registry value. If not present, set and return the default.';


--
-- TOC entry 275 (class 1255 OID 27590)
-- Name: registry_get_text(text, text); Type: FUNCTION; Schema: _slony_cursos; Owner: postgres
--

CREATE FUNCTION registry_get_text(p_key text, p_default text) RETURNS text
    LANGUAGE plpgsql
    AS $$
DECLARE
	v_value		text;
BEGIN
	select reg_text into v_value from "_slony_cursos".sl_registry
			where reg_key = p_key;
	if not found then 
		v_value = p_default;
		if p_default notnull then
			perform "_slony_cursos".registry_set_text(p_key, p_default);
		end if;
	else
		if v_value is null then
			raise exception 'Slony-I: registry key % is not a text value',
					p_key;
		end if;
	end if;
	return v_value;
END;
$$;


ALTER FUNCTION _slony_cursos.registry_get_text(p_key text, p_default text) OWNER TO postgres;

--
-- TOC entry 2718 (class 0 OID 0)
-- Dependencies: 275
-- Name: FUNCTION registry_get_text(p_key text, p_default text); Type: COMMENT; Schema: _slony_cursos; Owner: postgres
--

COMMENT ON FUNCTION registry_get_text(p_key text, p_default text) IS 'registry_get_text(key, value)

Get a registry value. If not present, set and return the default.';


--
-- TOC entry 278 (class 1255 OID 27592)
-- Name: registry_get_timestamp(text, timestamp with time zone); Type: FUNCTION; Schema: _slony_cursos; Owner: postgres
--

CREATE FUNCTION registry_get_timestamp(p_key text, p_default timestamp with time zone) RETURNS timestamp without time zone
    LANGUAGE plpgsql
    AS $$
DECLARE
	v_value		timestamp;
BEGIN
	select reg_timestamp into v_value from "_slony_cursos".sl_registry
			where reg_key = p_key;
	if not found then 
		v_value = p_default;
		if p_default notnull then
			perform "_slony_cursos".registry_set_timestamp(p_key, p_default);
		end if;
	else
		if v_value is null then
			raise exception 'Slony-I: registry key % is not an timestamp value',
					p_key;
		end if;
	end if;
	return v_value;
END;
$$;


ALTER FUNCTION _slony_cursos.registry_get_timestamp(p_key text, p_default timestamp with time zone) OWNER TO postgres;

--
-- TOC entry 2719 (class 0 OID 0)
-- Dependencies: 278
-- Name: FUNCTION registry_get_timestamp(p_key text, p_default timestamp with time zone); Type: COMMENT; Schema: _slony_cursos; Owner: postgres
--

COMMENT ON FUNCTION registry_get_timestamp(p_key text, p_default timestamp with time zone) IS 'registry_get_timestamp(key, value)

Get a registry value. If not present, set and return the default.';


--
-- TOC entry 272 (class 1255 OID 27587)
-- Name: registry_set_int4(text, integer); Type: FUNCTION; Schema: _slony_cursos; Owner: postgres
--

CREATE FUNCTION registry_set_int4(p_key text, p_value integer) RETURNS integer
    LANGUAGE plpgsql
    AS $$
BEGIN
	if p_value is null then
		delete from "_slony_cursos".sl_registry
				where reg_key = p_key;
	else
		lock table "_slony_cursos".sl_registry;
		update "_slony_cursos".sl_registry
				set reg_int4 = p_value
				where reg_key = p_key;
		if not found then
			insert into "_slony_cursos".sl_registry (reg_key, reg_int4)
					values (p_key, p_value);
		end if;
	end if;
	return p_value;
END;
$$;


ALTER FUNCTION _slony_cursos.registry_set_int4(p_key text, p_value integer) OWNER TO postgres;

--
-- TOC entry 2720 (class 0 OID 0)
-- Dependencies: 272
-- Name: FUNCTION registry_set_int4(p_key text, p_value integer); Type: COMMENT; Schema: _slony_cursos; Owner: postgres
--

COMMENT ON FUNCTION registry_set_int4(p_key text, p_value integer) IS 'registry_set_int4(key, value)

Set or delete a registry value';


--
-- TOC entry 274 (class 1255 OID 27589)
-- Name: registry_set_text(text, text); Type: FUNCTION; Schema: _slony_cursos; Owner: postgres
--

CREATE FUNCTION registry_set_text(p_key text, p_value text) RETURNS text
    LANGUAGE plpgsql
    AS $$
BEGIN
	if p_value is null then
		delete from "_slony_cursos".sl_registry
				where reg_key = p_key;
	else
		lock table "_slony_cursos".sl_registry;
		update "_slony_cursos".sl_registry
				set reg_text = p_value
				where reg_key = p_key;
		if not found then
			insert into "_slony_cursos".sl_registry (reg_key, reg_text)
					values (p_key, p_value);
		end if;
	end if;
	return p_value;
END;
$$;


ALTER FUNCTION _slony_cursos.registry_set_text(p_key text, p_value text) OWNER TO postgres;

--
-- TOC entry 2721 (class 0 OID 0)
-- Dependencies: 274
-- Name: FUNCTION registry_set_text(p_key text, p_value text); Type: COMMENT; Schema: _slony_cursos; Owner: postgres
--

COMMENT ON FUNCTION registry_set_text(p_key text, p_value text) IS 'registry_set_text(key, value)

Set or delete a registry value';


--
-- TOC entry 276 (class 1255 OID 27591)
-- Name: registry_set_timestamp(text, timestamp with time zone); Type: FUNCTION; Schema: _slony_cursos; Owner: postgres
--

CREATE FUNCTION registry_set_timestamp(p_key text, p_value timestamp with time zone) RETURNS timestamp without time zone
    LANGUAGE plpgsql
    AS $$
BEGIN
	if p_value is null then
		delete from "_slony_cursos".sl_registry
				where reg_key = p_key;
	else
		lock table "_slony_cursos".sl_registry;
		update "_slony_cursos".sl_registry
				set reg_timestamp = p_value
				where reg_key = p_key;
		if not found then
			insert into "_slony_cursos".sl_registry (reg_key, reg_timestamp)
					values (p_key, p_value);
		end if;
	end if;
	return p_value;
END;
$$;


ALTER FUNCTION _slony_cursos.registry_set_timestamp(p_key text, p_value timestamp with time zone) OWNER TO postgres;

--
-- TOC entry 2722 (class 0 OID 0)
-- Dependencies: 276
-- Name: FUNCTION registry_set_timestamp(p_key text, p_value timestamp with time zone); Type: COMMENT; Schema: _slony_cursos; Owner: postgres
--

COMMENT ON FUNCTION registry_set_timestamp(p_key text, p_value timestamp with time zone) IS 'registry_set_timestamp(key, value)

Set or delete a registry value';


--
-- TOC entry 375 (class 1255 OID 27697)
-- Name: repair_log_triggers(boolean); Type: FUNCTION; Schema: _slony_cursos; Owner: postgres
--

CREATE FUNCTION repair_log_triggers(only_locked boolean) RETURNS integer
    LANGUAGE plpgsql
    AS $$
declare
	retval integer;
	table_row record;
begin
	retval=0;
	for table_row in	
		select  tab_nspname,tab_relname,
				tab_idxname, tab_id, mode,
				"_slony_cursos".determineAttKindUnique(tab_nspname||
					'.'||tab_relname,tab_idxname) as attkind
		from
				"_slony_cursos".sl_table
				left join 
				pg_locks on (relation=tab_reloid and pid=pg_backend_pid()
				and mode='AccessExclusiveLock')				
				,pg_trigger
		where tab_reloid=tgrelid and 
		"_slony_cursos".determineAttKindUnique(tab_nspname||'.'
						||tab_relname,tab_idxname)
			!=("_slony_cursos".decode_tgargs(tgargs))[2]
			and tgname =  '_slony_cursos'
			|| '_logtrigger'
		LOOP
				if (only_locked=false) or table_row.mode='AccessExclusiveLock' then
					 perform "_slony_cursos".recreate_log_trigger
					 		 (table_row.tab_nspname||'.'||table_row.tab_relname,
							 table_row.tab_id,table_row.attkind);
					retval=retval+1;
				else 
					 raise notice '%.% has an invalid configuration on the log trigger. This was not corrected because only_lock is true and the table is not locked.',
					 table_row.tab_nspname,table_row.tab_relname;
			
				end if;
		end loop;
	return retval;
end
$$;


ALTER FUNCTION _slony_cursos.repair_log_triggers(only_locked boolean) OWNER TO postgres;

--
-- TOC entry 2723 (class 0 OID 0)
-- Dependencies: 375
-- Name: FUNCTION repair_log_triggers(only_locked boolean); Type: COMMENT; Schema: _slony_cursos; Owner: postgres
--

COMMENT ON FUNCTION repair_log_triggers(only_locked boolean) IS '
repair the log triggers as required.  If only_locked is true then only 
tables that are already exclusively locked by the current transaction are 
repaired. Otherwise all replicated tables with outdated trigger arguments
are recreated.';


--
-- TOC entry 364 (class 1255 OID 27686)
-- Name: replicate_partition(integer, text, text, text, text); Type: FUNCTION; Schema: _slony_cursos; Owner: postgres
--

CREATE FUNCTION replicate_partition(p_tab_id integer, p_nspname text, p_tabname text, p_idxname text, p_comment text) RETURNS bigint
    LANGUAGE plpgsql
    AS $$
declare
  prec record;
  prec2 record;
  v_set_id int4;

begin
-- Look up the parent table; fail if it does not exist
   select c1.oid into prec from pg_catalog.pg_class c1, pg_catalog.pg_class c2, pg_catalog.pg_inherits i, pg_catalog.pg_namespace n where c1.oid = i.inhparent  and c2.oid = i.inhrelid and n.oid = c2.relnamespace and n.nspname = p_nspname and c2.relname = p_tabname;
   if not found then
	raise exception 'replicate_partition: No parent table found for %.%!', p_nspname, p_tabname;
   end if;

-- The parent table tells us what replication set to use
   select tab_set into prec2 from "_slony_cursos".sl_table where tab_reloid = prec.oid;
   if not found then
	raise exception 'replicate_partition: Parent table % for new partition %.% is not replicated!', prec.oid, p_nspname, p_tabname;
   end if;

   v_set_id := prec2.tab_set;

-- Now, we have all the parameters necessary to run add_empty_table_to_replication...
   return "_slony_cursos".add_empty_table_to_replication(v_set_id, p_tab_id, p_nspname, p_tabname, p_idxname, p_comment);
end
$$;


ALTER FUNCTION _slony_cursos.replicate_partition(p_tab_id integer, p_nspname text, p_tabname text, p_idxname text, p_comment text) OWNER TO postgres;

--
-- TOC entry 2724 (class 0 OID 0)
-- Dependencies: 364
-- Name: FUNCTION replicate_partition(p_tab_id integer, p_nspname text, p_tabname text, p_idxname text, p_comment text); Type: COMMENT; Schema: _slony_cursos; Owner: postgres
--

COMMENT ON FUNCTION replicate_partition(p_tab_id integer, p_nspname text, p_tabname text, p_idxname text, p_comment text) IS 'Add a partition table to replication.
tab_idxname is optional - if NULL, then we use the primary key.
This function looks up replication configuration via the parent table.

Note that this function is to be run within an EXECUTE SCRIPT script,
so it runs at the right place in the transaction stream on all
nodes.';


--
-- TOC entry 257 (class 1255 OID 27570)
-- Name: resetsession(); Type: FUNCTION; Schema: _slony_cursos; Owner: postgres
--

CREATE FUNCTION resetsession() RETURNS text
    LANGUAGE c
    AS '$libdir/slony1_funcs.2.2.4', '_Slony_I_2_2_4_resetSession';


ALTER FUNCTION _slony_cursos.resetsession() OWNER TO postgres;

--
-- TOC entry 367 (class 1255 OID 27689)
-- Name: reshapesubscription(integer, integer, integer); Type: FUNCTION; Schema: _slony_cursos; Owner: postgres
--

CREATE FUNCTION reshapesubscription(p_sub_origin integer, p_sub_provider integer, p_sub_receiver integer) RETURNS integer
    LANGUAGE plpgsql
    AS $$
begin
	update "_slony_cursos".sl_subscribe
		   set sub_provider=p_sub_provider
		   from "_slony_cursos".sl_set
		   WHERE sub_set=sl_set.set_id
		   and sl_set.set_origin=p_sub_origin and sub_receiver=p_sub_receiver;
	if found then
	   perform "_slony_cursos".RebuildListenEntries();
	   notify "_slony_cursos_Restart";
	end if;
	return 0;
end
$$;


ALTER FUNCTION _slony_cursos.reshapesubscription(p_sub_origin integer, p_sub_provider integer, p_sub_receiver integer) OWNER TO postgres;

--
-- TOC entry 2725 (class 0 OID 0)
-- Dependencies: 367
-- Name: FUNCTION reshapesubscription(p_sub_origin integer, p_sub_provider integer, p_sub_receiver integer); Type: COMMENT; Schema: _slony_cursos; Owner: postgres
--

COMMENT ON FUNCTION reshapesubscription(p_sub_origin integer, p_sub_provider integer, p_sub_receiver integer) IS 'Run on a receiver/subscriber node when the provider for that
subscription is being changed.  Slonik will invoke this method
before the SUBSCRIBE_SET event propogates to the receiver
so listen paths can be updated.';


--
-- TOC entry 337 (class 1255 OID 27652)
-- Name: resubscribenode(integer, integer, integer); Type: FUNCTION; Schema: _slony_cursos; Owner: postgres
--

CREATE FUNCTION resubscribenode(p_origin integer, p_provider integer, p_receiver integer) RETURNS bigint
    LANGUAGE plpgsql
    AS $$
declare
	v_record record;
	v_missing_sets text;
	v_ev_seqno bigint;
begin
	-- ----
	-- Grab the central configuration lock
	-- ----
	lock table "_slony_cursos".sl_config_lock;

	--
	-- Check that the receiver exists
	--
	if not exists (select no_id from "_slony_cursos".sl_node where no_id=
	       	      p_receiver) then
		      raise exception 'Slony-I: subscribeSet() receiver % does not exist' , p_receiver;
	end if;

	--
	-- Check that the provider exists
	--
	if not exists (select no_id from "_slony_cursos".sl_node where no_id=
	       	      p_provider) then
		      raise exception 'Slony-I: subscribeSet() provider % does not exist' , p_provider;
	end if;

	
	-- ----
	-- Check that this is called on the origin node
	-- ----
	if p_origin != "_slony_cursos".getLocalNodeId('_slony_cursos') then
		raise exception 'Slony-I: subscribeSet() must be called on origin';
	end if;

	-- ---
	-- Verify that the provider is either the origin or an active subscriber
	-- Bug report #1362
	-- ---
	if p_origin <> p_provider then
	   for v_record in select sub1.sub_set from 
		    "_slony_cursos".sl_subscribe sub1			
		    left outer join  ("_slony_cursos".sl_subscribe sub2 
				 inner join
				 "_slony_cursos".sl_set  on (
								sl_set.set_id=sub2.sub_set
								and sub2.sub_set=p_origin)
								)
			ON (  sub1.sub_set = sub2.sub_set and 
                  sub1.sub_receiver = p_provider and
			      sub1.sub_forward and sub1.sub_active
				  and sub2.sub_receiver=p_receiver)
		
			where sub2.sub_set is null 
		loop
				v_missing_sets=v_missing_sets || ' ' || v_record.sub_set;
		end loop;
		if v_missing_sets is not null then
			raise exception 'Slony-I: subscribeSet(): provider % is not an active forwarding node for replication set %', p_sub_provider, v_missing_sets;
		end if;
	end if;

	for v_record in select *  from 
		"_slony_cursos".sl_subscribe, "_slony_cursos".sl_set where 
		sub_set=set_id and
		sub_receiver=p_receiver
		and set_origin=p_origin
	loop
	-- ----
	-- Create the SUBSCRIBE_SET event
	-- ----
	   v_ev_seqno :=  "_slony_cursos".createEvent('_slony_cursos', 'SUBSCRIBE_SET', 
				  v_record.sub_set::text, p_provider::text, p_receiver::text, 
				  case v_record.sub_forward when true then 't' else 'f' end,
				  	   'f' );

		-- ----
		-- Call the internal procedure to store the subscription
		-- ----
		perform "_slony_cursos".subscribeSet_int(v_record.sub_set, 
				p_provider,
				p_receiver, v_record.sub_forward, false);
	end loop;

	return v_ev_seqno;	
end;
$$;


ALTER FUNCTION _slony_cursos.resubscribenode(p_origin integer, p_provider integer, p_receiver integer) OWNER TO postgres;

--
-- TOC entry 265 (class 1255 OID 27580)
-- Name: seqtrack(integer, bigint); Type: FUNCTION; Schema: _slony_cursos; Owner: postgres
--

CREATE FUNCTION seqtrack(p_seqid integer, p_seqval bigint) RETURNS bigint
    LANGUAGE c STRICT
    AS '$libdir/slony1_funcs.2.2.4', '_Slony_I_2_2_4_seqtrack';


ALTER FUNCTION _slony_cursos.seqtrack(p_seqid integer, p_seqval bigint) OWNER TO postgres;

--
-- TOC entry 2726 (class 0 OID 0)
-- Dependencies: 265
-- Name: FUNCTION seqtrack(p_seqid integer, p_seqval bigint); Type: COMMENT; Schema: _slony_cursos; Owner: postgres
--

COMMENT ON FUNCTION seqtrack(p_seqid integer, p_seqval bigint) IS 'Returns NULL if seqval has not changed since the last call for seqid';


--
-- TOC entry 232 (class 1255 OID 27484)
-- Name: sequencelastvalue(text); Type: FUNCTION; Schema: _slony_cursos; Owner: postgres
--

CREATE FUNCTION sequencelastvalue(p_seqname text) RETURNS bigint
    LANGUAGE plpgsql
    AS $$
declare
	v_seq_row	record;
begin
	for v_seq_row in execute 'select last_value from ' || "_slony_cursos".slon_quote_input(p_seqname)
	loop
		return v_seq_row.last_value;
	end loop;

	-- not reached
end;
$$;


ALTER FUNCTION _slony_cursos.sequencelastvalue(p_seqname text) OWNER TO postgres;

--
-- TOC entry 2727 (class 0 OID 0)
-- Dependencies: 232
-- Name: FUNCTION sequencelastvalue(p_seqname text); Type: COMMENT; Schema: _slony_cursos; Owner: postgres
--

COMMENT ON FUNCTION sequencelastvalue(p_seqname text) IS 'sequenceLastValue(p_seqname)

Utility function used in sl_seqlastvalue view to compactly get the
last value from the requested sequence.';


--
-- TOC entry 329 (class 1255 OID 27645)
-- Name: sequencesetvalue(integer, integer, bigint, bigint, boolean); Type: FUNCTION; Schema: _slony_cursos; Owner: postgres
--

CREATE FUNCTION sequencesetvalue(p_seq_id integer, p_seq_origin integer, p_ev_seqno bigint, p_last_value bigint, p_ignore_missing boolean) RETURNS integer
    LANGUAGE plpgsql
    AS $$
declare
	v_fqname			text;
	v_found                         integer;
begin
	-- ----
	-- Get the sequences fully qualified name
	-- ----
	select "_slony_cursos".slon_quote_brute(PGN.nspname) || '.' ||
			"_slony_cursos".slon_quote_brute(PGC.relname) into v_fqname
		from "_slony_cursos".sl_sequence SQ,
			"pg_catalog".pg_class PGC, "pg_catalog".pg_namespace PGN
		where SQ.seq_id = p_seq_id
			and SQ.seq_reloid = PGC.oid
			and PGC.relnamespace = PGN.oid;
	if not found then
	        if p_ignore_missing then
                       return null;
                end if;
		raise exception 'Slony-I: sequenceSetValue(): sequence % not found', p_seq_id;
	end if;

	-- ----
	-- Update it to the new value
	-- ----
	execute 'select setval(''' || v_fqname ||
			''', ' || p_last_value::text || ')';

	if p_ev_seqno is not null then
	   insert into "_slony_cursos".sl_seqlog
			(seql_seqid, seql_origin, seql_ev_seqno, seql_last_value)
			values (p_seq_id, p_seq_origin, p_ev_seqno, p_last_value);
	end if;
	return p_seq_id;
end;
$$;


ALTER FUNCTION _slony_cursos.sequencesetvalue(p_seq_id integer, p_seq_origin integer, p_ev_seqno bigint, p_last_value bigint, p_ignore_missing boolean) OWNER TO postgres;

--
-- TOC entry 2728 (class 0 OID 0)
-- Dependencies: 329
-- Name: FUNCTION sequencesetvalue(p_seq_id integer, p_seq_origin integer, p_ev_seqno bigint, p_last_value bigint, p_ignore_missing boolean); Type: COMMENT; Schema: _slony_cursos; Owner: postgres
--

COMMENT ON FUNCTION sequencesetvalue(p_seq_id integer, p_seq_origin integer, p_ev_seqno bigint, p_last_value bigint, p_ignore_missing boolean) IS 'sequenceSetValue (seq_id, seq_origin, ev_seqno, last_value,ignore_missing)
Set sequence seq_id to have new value last_value.
';


--
-- TOC entry 322 (class 1255 OID 27637)
-- Name: setaddsequence(integer, integer, text, text); Type: FUNCTION; Schema: _slony_cursos; Owner: postgres
--

CREATE FUNCTION setaddsequence(p_set_id integer, p_seq_id integer, p_fqname text, p_seq_comment text) RETURNS bigint
    LANGUAGE plpgsql
    AS $$
declare
	v_set_origin		int4;
begin
	-- ----
	-- Grab the central configuration lock
	-- ----
	lock table "_slony_cursos".sl_config_lock;

	-- ----
	-- Check that we are the origin of the set
	-- ----
	select set_origin into v_set_origin
			from "_slony_cursos".sl_set
			where set_id = p_set_id;
	if not found then
		raise exception 'Slony-I: setAddSequence(): set % not found', p_set_id;
	end if;
	if v_set_origin != "_slony_cursos".getLocalNodeId('_slony_cursos') then
		raise exception 'Slony-I: setAddSequence(): set % has remote origin - submit to origin node', p_set_id;
	end if;

	if exists (select true from "_slony_cursos".sl_subscribe
			where sub_set = p_set_id)
	then
		raise exception 'Slony-I: cannot add sequence to currently subscribed set %',
				p_set_id;
	end if;

	-- ----
	-- Add the sequence to the set and generate the SET_ADD_SEQUENCE event
	-- ----
	perform "_slony_cursos".setAddSequence_int(p_set_id, p_seq_id, p_fqname,
			p_seq_comment);
	return  "_slony_cursos".createEvent('_slony_cursos', 'SET_ADD_SEQUENCE',
						p_set_id::text, p_seq_id::text, 
						p_fqname::text, p_seq_comment::text);
end;
$$;


ALTER FUNCTION _slony_cursos.setaddsequence(p_set_id integer, p_seq_id integer, p_fqname text, p_seq_comment text) OWNER TO postgres;

--
-- TOC entry 2729 (class 0 OID 0)
-- Dependencies: 322
-- Name: FUNCTION setaddsequence(p_set_id integer, p_seq_id integer, p_fqname text, p_seq_comment text); Type: COMMENT; Schema: _slony_cursos; Owner: postgres
--

COMMENT ON FUNCTION setaddsequence(p_set_id integer, p_seq_id integer, p_fqname text, p_seq_comment text) IS 'setAddSequence (set_id, seq_id, seq_fqname, seq_comment)

On the origin node for set set_id, add sequence seq_fqname to the
replication set, and raise SET_ADD_SEQUENCE to cause this to replicate
to subscriber nodes.';


--
-- TOC entry 323 (class 1255 OID 27638)
-- Name: setaddsequence_int(integer, integer, text, text); Type: FUNCTION; Schema: _slony_cursos; Owner: postgres
--

CREATE FUNCTION setaddsequence_int(p_set_id integer, p_seq_id integer, p_fqname text, p_seq_comment text) RETURNS integer
    LANGUAGE plpgsql
    AS $$
declare
	v_local_node_id		int4;
	v_set_origin		int4;
	v_sub_provider		int4;
	v_relkind			char;
	v_seq_reloid		oid;
	v_seq_relname		name;
	v_seq_nspname		name;
	v_sync_row			record;
begin
	-- ----
	-- Grab the central configuration lock
	-- ----
	lock table "_slony_cursos".sl_config_lock;

	-- ----
	-- For sets with a remote origin, check that we are subscribed 
	-- to that set. Otherwise we ignore the sequence because it might 
	-- not even exist in our database.
	-- ----
	v_local_node_id := "_slony_cursos".getLocalNodeId('_slony_cursos');
	select set_origin into v_set_origin
			from "_slony_cursos".sl_set
			where set_id = p_set_id;
	if not found then
		raise exception 'Slony-I: setAddSequence_int(): set % not found',
				p_set_id;
	end if;
	if v_set_origin != v_local_node_id then
		select sub_provider into v_sub_provider
				from "_slony_cursos".sl_subscribe
				where sub_set = p_set_id
				and sub_receiver = "_slony_cursos".getLocalNodeId('_slony_cursos');
		if not found then
			return 0;
		end if;
	end if;
	
	-- ----
	-- Get the sequences OID and check that it is a sequence
	-- ----
	select PGC.oid, PGC.relkind, PGC.relname, PGN.nspname 
		into v_seq_reloid, v_relkind, v_seq_relname, v_seq_nspname
			from "pg_catalog".pg_class PGC, "pg_catalog".pg_namespace PGN
			where PGC.relnamespace = PGN.oid
			and "_slony_cursos".slon_quote_input(p_fqname) = "_slony_cursos".slon_quote_brute(PGN.nspname) ||
					'.' || "_slony_cursos".slon_quote_brute(PGC.relname);
	if not found then
		raise exception 'Slony-I: setAddSequence_int(): sequence % not found', 
				p_fqname;
	end if;
	if v_relkind != 'S' then
		raise exception 'Slony-I: setAddSequence_int(): % is not a sequence',
				p_fqname;
	end if;

        select 1 into v_sync_row from "_slony_cursos".sl_sequence where seq_id = p_seq_id;
	if not found then
               v_relkind := 'o';   -- all is OK
        else
                raise exception 'Slony-I: setAddSequence_int(): sequence ID % has already been assigned', p_seq_id;
        end if;

	-- ----
	-- Add the sequence to sl_sequence
	-- ----
	insert into "_slony_cursos".sl_sequence
		(seq_id, seq_reloid, seq_relname, seq_nspname, seq_set, seq_comment) 
		values
		(p_seq_id, v_seq_reloid, v_seq_relname, v_seq_nspname,  p_set_id, p_seq_comment);

	-- ----
	-- On the set origin, fake a sl_seqlog row for the last sync event
	-- ----
	if v_set_origin = v_local_node_id then
		for v_sync_row in select coalesce (max(ev_seqno), 0) as ev_seqno
				from "_slony_cursos".sl_event
				where ev_origin = v_local_node_id
					and ev_type = 'SYNC'
		loop
			insert into "_slony_cursos".sl_seqlog
					(seql_seqid, seql_origin, seql_ev_seqno, 
					seql_last_value) values
					(p_seq_id, v_local_node_id, v_sync_row.ev_seqno,
					"_slony_cursos".sequenceLastValue(p_fqname));
		end loop;
	end if;

	return p_seq_id;
end;
$$;


ALTER FUNCTION _slony_cursos.setaddsequence_int(p_set_id integer, p_seq_id integer, p_fqname text, p_seq_comment text) OWNER TO postgres;

--
-- TOC entry 2730 (class 0 OID 0)
-- Dependencies: 323
-- Name: FUNCTION setaddsequence_int(p_set_id integer, p_seq_id integer, p_fqname text, p_seq_comment text); Type: COMMENT; Schema: _slony_cursos; Owner: postgres
--

COMMENT ON FUNCTION setaddsequence_int(p_set_id integer, p_seq_id integer, p_fqname text, p_seq_comment text) IS 'setAddSequence_int (set_id, seq_id, seq_fqname, seq_comment)

This processes the SET_ADD_SEQUENCE event.  On remote nodes that
subscribe to set_id, add the sequence to the replication set.';


--
-- TOC entry 317 (class 1255 OID 27633)
-- Name: setaddtable(integer, integer, text, name, text); Type: FUNCTION; Schema: _slony_cursos; Owner: postgres
--

CREATE FUNCTION setaddtable(p_set_id integer, p_tab_id integer, p_fqname text, p_tab_idxname name, p_tab_comment text) RETURNS bigint
    LANGUAGE plpgsql
    AS $$
declare
	v_set_origin		int4;
begin
	-- ----
	-- Grab the central configuration lock
	-- ----
	lock table "_slony_cursos".sl_config_lock;

	-- ----
	-- Check that we are the origin of the set
	-- ----
	select set_origin into v_set_origin
			from "_slony_cursos".sl_set
			where set_id = p_set_id;
	if not found then
		raise exception 'Slony-I: setAddTable(): set % not found', p_set_id;
	end if;
	if v_set_origin != "_slony_cursos".getLocalNodeId('_slony_cursos') then
		raise exception 'Slony-I: setAddTable(): set % has remote origin', p_set_id;
	end if;

	if exists (select true from "_slony_cursos".sl_subscribe
			where sub_set = p_set_id)
	then
		raise exception 'Slony-I: cannot add table to currently subscribed set % - must attach to an unsubscribed set',
				p_set_id;
	end if;

	-- ----
	-- Add the table to the set and generate the SET_ADD_TABLE event
	-- ----
	perform "_slony_cursos".setAddTable_int(p_set_id, p_tab_id, p_fqname,
			p_tab_idxname, p_tab_comment);
	return  "_slony_cursos".createEvent('_slony_cursos', 'SET_ADD_TABLE',
			p_set_id::text, p_tab_id::text, p_fqname::text,
			p_tab_idxname::text, p_tab_comment::text);
end;
$$;


ALTER FUNCTION _slony_cursos.setaddtable(p_set_id integer, p_tab_id integer, p_fqname text, p_tab_idxname name, p_tab_comment text) OWNER TO postgres;

--
-- TOC entry 2731 (class 0 OID 0)
-- Dependencies: 317
-- Name: FUNCTION setaddtable(p_set_id integer, p_tab_id integer, p_fqname text, p_tab_idxname name, p_tab_comment text); Type: COMMENT; Schema: _slony_cursos; Owner: postgres
--

COMMENT ON FUNCTION setaddtable(p_set_id integer, p_tab_id integer, p_fqname text, p_tab_idxname name, p_tab_comment text) IS 'setAddTable (set_id, tab_id, tab_fqname, tab_idxname, tab_comment)

Add table tab_fqname to replication set on origin node, and generate
SET_ADD_TABLE event to allow this to propagate to other nodes.

Note that the table id, tab_id, must be unique ACROSS ALL SETS.';


--
-- TOC entry 318 (class 1255 OID 27634)
-- Name: setaddtable_int(integer, integer, text, name, text); Type: FUNCTION; Schema: _slony_cursos; Owner: postgres
--

CREATE FUNCTION setaddtable_int(p_set_id integer, p_tab_id integer, p_fqname text, p_tab_idxname name, p_tab_comment text) RETURNS integer
    LANGUAGE plpgsql
    AS $$
declare
	v_tab_relname		name;
	v_tab_nspname		name;
	v_local_node_id		int4;
	v_set_origin		int4;
	v_sub_provider		int4;
	v_relkind		char;
	v_tab_reloid		oid;
	v_pkcand_nn		boolean;
	v_prec			record;
begin
	-- ----
	-- Grab the central configuration lock
	-- ----
	lock table "_slony_cursos".sl_config_lock;

	-- ----
	-- For sets with a remote origin, check that we are subscribed 
	-- to that set. Otherwise we ignore the table because it might 
	-- not even exist in our database.
	-- ----
	v_local_node_id := "_slony_cursos".getLocalNodeId('_slony_cursos');
	select set_origin into v_set_origin
			from "_slony_cursos".sl_set
			where set_id = p_set_id;
	if not found then
		raise exception 'Slony-I: setAddTable_int(): set % not found',
				p_set_id;
	end if;
	if v_set_origin != v_local_node_id then
		select sub_provider into v_sub_provider
				from "_slony_cursos".sl_subscribe
				where sub_set = p_set_id
				and sub_receiver = "_slony_cursos".getLocalNodeId('_slony_cursos');
		if not found then
			return 0;
		end if;
	end if;
	
	-- ----
	-- Get the tables OID and check that it is a real table
	-- ----
	select PGC.oid, PGC.relkind, PGC.relname, PGN.nspname into v_tab_reloid, v_relkind, v_tab_relname, v_tab_nspname
			from "pg_catalog".pg_class PGC, "pg_catalog".pg_namespace PGN
			where PGC.relnamespace = PGN.oid
			and "_slony_cursos".slon_quote_input(p_fqname) = "_slony_cursos".slon_quote_brute(PGN.nspname) ||
					'.' || "_slony_cursos".slon_quote_brute(PGC.relname);
	if not found then
		raise exception 'Slony-I: setAddTable_int(): table % not found', 
				p_fqname;
	end if;
	if v_relkind != 'r' then
		raise exception 'Slony-I: setAddTable_int(): % is not a regular table',
				p_fqname;
	end if;

	if not exists (select indexrelid
			from "pg_catalog".pg_index PGX, "pg_catalog".pg_class PGC
			where PGX.indrelid = v_tab_reloid
				and PGX.indexrelid = PGC.oid
				and PGC.relname = p_tab_idxname)
	then
		raise exception 'Slony-I: setAddTable_int(): table % has no index %',
				p_fqname, p_tab_idxname;
	end if;

	-- ----
	-- Verify that the columns in the PK (or candidate) are not NULLABLE
	-- ----

	v_pkcand_nn := 'f';
	for v_prec in select attname from "pg_catalog".pg_attribute where attrelid = 
                        (select oid from "pg_catalog".pg_class where oid = v_tab_reloid) 
                    and attname in (select attname from "pg_catalog".pg_attribute where 
                                    attrelid = (select oid from "pg_catalog".pg_class PGC, 
                                    "pg_catalog".pg_index PGX where 
                                    PGC.relname = p_tab_idxname and PGX.indexrelid=PGC.oid and
                                    PGX.indrelid = v_tab_reloid)) and attnotnull <> 't'
	loop
		raise notice 'Slony-I: setAddTable_int: table % PK column % nullable', p_fqname, v_prec.attname;
		v_pkcand_nn := 't';
	end loop;
	if v_pkcand_nn then
		raise exception 'Slony-I: setAddTable_int: table % not replicable!', p_fqname;
	end if;

	select * into v_prec from "_slony_cursos".sl_table where tab_id = p_tab_id;
	if not found then
		v_pkcand_nn := 't';  -- No-op -- All is well
	else
		raise exception 'Slony-I: setAddTable_int: table id % has already been assigned!', p_tab_id;
	end if;

	-- ----
	-- Add the table to sl_table and create the trigger on it.
	-- ----
	insert into "_slony_cursos".sl_table
			(tab_id, tab_reloid, tab_relname, tab_nspname, 
			tab_set, tab_idxname, tab_altered, tab_comment) 
			values
			(p_tab_id, v_tab_reloid, v_tab_relname, v_tab_nspname,
			p_set_id, p_tab_idxname, false, p_tab_comment);
	perform "_slony_cursos".alterTableAddTriggers(p_tab_id);

	return p_tab_id;
end;
$$;


ALTER FUNCTION _slony_cursos.setaddtable_int(p_set_id integer, p_tab_id integer, p_fqname text, p_tab_idxname name, p_tab_comment text) OWNER TO postgres;

--
-- TOC entry 2732 (class 0 OID 0)
-- Dependencies: 318
-- Name: FUNCTION setaddtable_int(p_set_id integer, p_tab_id integer, p_fqname text, p_tab_idxname name, p_tab_comment text); Type: COMMENT; Schema: _slony_cursos; Owner: postgres
--

COMMENT ON FUNCTION setaddtable_int(p_set_id integer, p_tab_id integer, p_fqname text, p_tab_idxname name, p_tab_comment text) IS 'setAddTable_int (set_id, tab_id, tab_fqname, tab_idxname, tab_comment)

This function processes the SET_ADD_TABLE event on remote nodes,
adding a table to replication if the remote node is subscribing to its
replication set.';


--
-- TOC entry 324 (class 1255 OID 27639)
-- Name: setdropsequence(integer); Type: FUNCTION; Schema: _slony_cursos; Owner: postgres
--

CREATE FUNCTION setdropsequence(p_seq_id integer) RETURNS bigint
    LANGUAGE plpgsql
    AS $$
declare
	v_set_id		int4;
	v_set_origin		int4;
begin
	-- ----
	-- Grab the central configuration lock
	-- ----
	lock table "_slony_cursos".sl_config_lock;

	-- ----
	-- Determine set id for this sequence
	-- ----
	select seq_set into v_set_id from "_slony_cursos".sl_sequence where seq_id = p_seq_id;

	-- ----
	-- Ensure sequence exists
	-- ----
	if not found then
		raise exception 'Slony-I: setDropSequence_int(): sequence % not found',
			p_seq_id;
	end if;

	-- ----
	-- Check that we are the origin of the set
	-- ----
	select set_origin into v_set_origin
			from "_slony_cursos".sl_set
			where set_id = v_set_id;
	if not found then
		raise exception 'Slony-I: setDropSequence(): set % not found', v_set_id;
	end if;
	if v_set_origin != "_slony_cursos".getLocalNodeId('_slony_cursos') then
		raise exception 'Slony-I: setDropSequence(): set % has origin at another node - submit this to that node', v_set_id;
	end if;

	-- ----
	-- Add the sequence to the set and generate the SET_ADD_SEQUENCE event
	-- ----
	perform "_slony_cursos".setDropSequence_int(p_seq_id);
	return  "_slony_cursos".createEvent('_slony_cursos', 'SET_DROP_SEQUENCE',
					p_seq_id::text);
end;
$$;


ALTER FUNCTION _slony_cursos.setdropsequence(p_seq_id integer) OWNER TO postgres;

--
-- TOC entry 2733 (class 0 OID 0)
-- Dependencies: 324
-- Name: FUNCTION setdropsequence(p_seq_id integer); Type: COMMENT; Schema: _slony_cursos; Owner: postgres
--

COMMENT ON FUNCTION setdropsequence(p_seq_id integer) IS 'setDropSequence (seq_id)

On the origin node for the set, drop sequence seq_id from replication
set, and raise SET_DROP_SEQUENCE to cause this to replicate to
subscriber nodes.';


--
-- TOC entry 325 (class 1255 OID 27640)
-- Name: setdropsequence_int(integer); Type: FUNCTION; Schema: _slony_cursos; Owner: postgres
--

CREATE FUNCTION setdropsequence_int(p_seq_id integer) RETURNS integer
    LANGUAGE plpgsql
    AS $$
declare
	v_set_id		int4;
	v_local_node_id		int4;
	v_set_origin		int4;
	v_sub_provider		int4;
	v_relkind			char;
	v_sync_row			record;
begin
	-- ----
	-- Grab the central configuration lock
	-- ----
	lock table "_slony_cursos".sl_config_lock;

	-- ----
	-- Determine set id for this sequence
	-- ----
	select seq_set into v_set_id from "_slony_cursos".sl_sequence where seq_id = p_seq_id;

	-- ----
	-- Ensure sequence exists
	-- ----
	if not found then
		return 0;
	end if;

	-- ----
	-- For sets with a remote origin, check that we are subscribed 
	-- to that set. Otherwise we ignore the sequence because it might 
	-- not even exist in our database.
	-- ----
	v_local_node_id := "_slony_cursos".getLocalNodeId('_slony_cursos');
	select set_origin into v_set_origin
			from "_slony_cursos".sl_set
			where set_id = v_set_id;
	if not found then
		raise exception 'Slony-I: setDropSequence_int(): set % not found',
				v_set_id;
	end if;
	if v_set_origin != v_local_node_id then
		select sub_provider into v_sub_provider
				from "_slony_cursos".sl_subscribe
				where sub_set = v_set_id
				and sub_receiver = "_slony_cursos".getLocalNodeId('_slony_cursos');
		if not found then
			return 0;
		end if;
	end if;

	-- ----
	-- drop the sequence from sl_sequence, sl_seqlog
	-- ----
	delete from "_slony_cursos".sl_seqlog where seql_seqid = p_seq_id;
	delete from "_slony_cursos".sl_sequence where seq_id = p_seq_id;

	return p_seq_id;
end;
$$;


ALTER FUNCTION _slony_cursos.setdropsequence_int(p_seq_id integer) OWNER TO postgres;

--
-- TOC entry 2734 (class 0 OID 0)
-- Dependencies: 325
-- Name: FUNCTION setdropsequence_int(p_seq_id integer); Type: COMMENT; Schema: _slony_cursos; Owner: postgres
--

COMMENT ON FUNCTION setdropsequence_int(p_seq_id integer) IS 'setDropSequence_int (seq_id)

This processes the SET_DROP_SEQUENCE event.  On remote nodes that
subscribe to the set containing sequence seq_id, drop the sequence
from the replication set.';


--
-- TOC entry 319 (class 1255 OID 27635)
-- Name: setdroptable(integer); Type: FUNCTION; Schema: _slony_cursos; Owner: postgres
--

CREATE FUNCTION setdroptable(p_tab_id integer) RETURNS bigint
    LANGUAGE plpgsql
    AS $$
declare
	v_set_id		int4;
	v_set_origin		int4;
begin
	-- ----
	-- Grab the central configuration lock
	-- ----
	lock table "_slony_cursos".sl_config_lock;

    -- ----
	-- Determine the set_id
    -- ----
	select tab_set into v_set_id from "_slony_cursos".sl_table where tab_id = p_tab_id;

	-- ----
	-- Ensure table exists
	-- ----
	if not found then
		raise exception 'Slony-I: setDropTable_int(): table % not found',
			p_tab_id;
	end if;

	-- ----
	-- Check that we are the origin of the set
	-- ----
	select set_origin into v_set_origin
			from "_slony_cursos".sl_set
			where set_id = v_set_id;
	if not found then
		raise exception 'Slony-I: setDropTable(): set % not found', v_set_id;
	end if;
	if v_set_origin != "_slony_cursos".getLocalNodeId('_slony_cursos') then
		raise exception 'Slony-I: setDropTable(): set % has remote origin', v_set_id;
	end if;

	-- ----
	-- Drop the table from the set and generate the SET_ADD_TABLE event
	-- ----
	perform "_slony_cursos".setDropTable_int(p_tab_id);
	return  "_slony_cursos".createEvent('_slony_cursos', 'SET_DROP_TABLE', 
				p_tab_id::text);
end;
$$;


ALTER FUNCTION _slony_cursos.setdroptable(p_tab_id integer) OWNER TO postgres;

--
-- TOC entry 2735 (class 0 OID 0)
-- Dependencies: 319
-- Name: FUNCTION setdroptable(p_tab_id integer); Type: COMMENT; Schema: _slony_cursos; Owner: postgres
--

COMMENT ON FUNCTION setdroptable(p_tab_id integer) IS 'setDropTable (tab_id)

Drop table tab_id from set on origin node, and generate SET_DROP_TABLE
event to allow this to propagate to other nodes.';


--
-- TOC entry 321 (class 1255 OID 27636)
-- Name: setdroptable_int(integer); Type: FUNCTION; Schema: _slony_cursos; Owner: postgres
--

CREATE FUNCTION setdroptable_int(p_tab_id integer) RETURNS integer
    LANGUAGE plpgsql
    AS $$
declare
	v_set_id		int4;
	v_local_node_id		int4;
	v_set_origin		int4;
	v_sub_provider		int4;
	v_tab_reloid		oid;
begin
	-- ----
	-- Grab the central configuration lock
	-- ----
	lock table "_slony_cursos".sl_config_lock;

    -- ----
	-- Determine the set_id
    -- ----
	select tab_set into v_set_id from "_slony_cursos".sl_table where tab_id = p_tab_id;

	-- ----
	-- Ensure table exists
	-- ----
	if not found then
		return 0;
	end if;

	-- ----
	-- For sets with a remote origin, check that we are subscribed 
	-- to that set. Otherwise we ignore the table because it might 
	-- not even exist in our database.
	-- ----
	v_local_node_id := "_slony_cursos".getLocalNodeId('_slony_cursos');
	select set_origin into v_set_origin
			from "_slony_cursos".sl_set
			where set_id = v_set_id;
	if not found then
		raise exception 'Slony-I: setDropTable_int(): set % not found',
				v_set_id;
	end if;
	if v_set_origin != v_local_node_id then
		select sub_provider into v_sub_provider
				from "_slony_cursos".sl_subscribe
				where sub_set = v_set_id
				and sub_receiver = "_slony_cursos".getLocalNodeId('_slony_cursos');
		if not found then
			return 0;
		end if;
	end if;
	
	-- ----
	-- Drop the table from sl_table and drop trigger from it.
	-- ----
	perform "_slony_cursos".alterTableDropTriggers(p_tab_id);
	delete from "_slony_cursos".sl_table where tab_id = p_tab_id;
	return p_tab_id;
end;
$$;


ALTER FUNCTION _slony_cursos.setdroptable_int(p_tab_id integer) OWNER TO postgres;

--
-- TOC entry 2736 (class 0 OID 0)
-- Dependencies: 321
-- Name: FUNCTION setdroptable_int(p_tab_id integer); Type: COMMENT; Schema: _slony_cursos; Owner: postgres
--

COMMENT ON FUNCTION setdroptable_int(p_tab_id integer) IS 'setDropTable_int (tab_id)

This function processes the SET_DROP_TABLE event on remote nodes,
dropping a table from replication if the remote node is subscribing to
its replication set.';


--
-- TOC entry 327 (class 1255 OID 27643)
-- Name: setmovesequence(integer, integer); Type: FUNCTION; Schema: _slony_cursos; Owner: postgres
--

CREATE FUNCTION setmovesequence(p_seq_id integer, p_new_set_id integer) RETURNS bigint
    LANGUAGE plpgsql
    AS $$
declare
	v_old_set_id		int4;
	v_origin			int4;
begin
	-- ----
	-- Grab the central configuration lock
	-- ----
	lock table "_slony_cursos".sl_config_lock;

	-- ----
	-- Get the sequences current set
	-- ----
	select seq_set into v_old_set_id from "_slony_cursos".sl_sequence
			where seq_id = p_seq_id;
	if not found then
		raise exception 'Slony-I: setMoveSequence(): sequence %d not found', p_seq_id;
	end if;
	
	-- ----
	-- Check that both sets exist and originate here
	-- ----
	if p_new_set_id = v_old_set_id then
		raise exception 'Slony-I: setMoveSequence(): set ids cannot be identical';
	end if;
	select set_origin into v_origin from "_slony_cursos".sl_set
			where set_id = p_new_set_id;
	if not found then
		raise exception 'Slony-I: setMoveSequence(): set % not found', p_new_set_id;
	end if;
	if v_origin != "_slony_cursos".getLocalNodeId('_slony_cursos') then
		raise exception 'Slony-I: setMoveSequence(): set % does not originate on local node',
				p_new_set_id;
	end if;

	select set_origin into v_origin from "_slony_cursos".sl_set
			where set_id = v_old_set_id;
	if not found then
		raise exception 'Slony-I: set % not found', v_old_set_id;
	end if;
	if v_origin != "_slony_cursos".getLocalNodeId('_slony_cursos') then
		raise exception 'Slony-I: set % does not originate on local node',
				v_old_set_id;
	end if;

	-- ----
	-- Check that both sets are subscribed by the same set of nodes
	-- ----
	if exists (select true from "_slony_cursos".sl_subscribe SUB1
				where SUB1.sub_set = p_new_set_id
				and SUB1.sub_receiver not in (select SUB2.sub_receiver
						from "_slony_cursos".sl_subscribe SUB2
						where SUB2.sub_set = v_old_set_id))
	then
		raise exception 'Slony-I: subscriber lists of set % and % are different',
				p_new_set_id, v_old_set_id;
	end if;

	if exists (select true from "_slony_cursos".sl_subscribe SUB1
				where SUB1.sub_set = v_old_set_id
				and SUB1.sub_receiver not in (select SUB2.sub_receiver
						from "_slony_cursos".sl_subscribe SUB2
						where SUB2.sub_set = p_new_set_id))
	then
		raise exception 'Slony-I: subscriber lists of set % and % are different',
				v_old_set_id, p_new_set_id;
	end if;

	-- ----
	-- Change the set the sequence belongs to
	-- ----
	perform "_slony_cursos".setMoveSequence_int(p_seq_id, p_new_set_id);
	return  "_slony_cursos".createEvent('_slony_cursos', 'SET_MOVE_SEQUENCE', 
			p_seq_id::text, p_new_set_id::text);
end;
$$;


ALTER FUNCTION _slony_cursos.setmovesequence(p_seq_id integer, p_new_set_id integer) OWNER TO postgres;

--
-- TOC entry 2737 (class 0 OID 0)
-- Dependencies: 327
-- Name: FUNCTION setmovesequence(p_seq_id integer, p_new_set_id integer); Type: COMMENT; Schema: _slony_cursos; Owner: postgres
--

COMMENT ON FUNCTION setmovesequence(p_seq_id integer, p_new_set_id integer) IS 'setMoveSequence(p_seq_id, p_new_set_id) - This generates the
SET_MOVE_SEQUENCE event, after validation, notably that both sets
exist, are distinct, and have exactly the same subscription lists';


--
-- TOC entry 328 (class 1255 OID 27644)
-- Name: setmovesequence_int(integer, integer); Type: FUNCTION; Schema: _slony_cursos; Owner: postgres
--

CREATE FUNCTION setmovesequence_int(p_seq_id integer, p_new_set_id integer) RETURNS integer
    LANGUAGE plpgsql
    AS $$
begin
	-- ----
	-- Grab the central configuration lock
	-- ----
	lock table "_slony_cursos".sl_config_lock;

	-- ----
	-- Move the sequence to the new set
	-- ----
	update "_slony_cursos".sl_sequence
			set seq_set = p_new_set_id
			where seq_id = p_seq_id;

	return p_seq_id;
end;
$$;


ALTER FUNCTION _slony_cursos.setmovesequence_int(p_seq_id integer, p_new_set_id integer) OWNER TO postgres;

--
-- TOC entry 2738 (class 0 OID 0)
-- Dependencies: 328
-- Name: FUNCTION setmovesequence_int(p_seq_id integer, p_new_set_id integer); Type: COMMENT; Schema: _slony_cursos; Owner: postgres
--

COMMENT ON FUNCTION setmovesequence_int(p_seq_id integer, p_new_set_id integer) IS 'setMoveSequence_int(p_seq_id, p_new_set_id) - processes the
SET_MOVE_SEQUENCE event, moving a sequence to another replication
set.';


--
-- TOC entry 320 (class 1255 OID 27641)
-- Name: setmovetable(integer, integer); Type: FUNCTION; Schema: _slony_cursos; Owner: postgres
--

CREATE FUNCTION setmovetable(p_tab_id integer, p_new_set_id integer) RETURNS bigint
    LANGUAGE plpgsql
    AS $$
declare
	v_old_set_id		int4;
	v_origin			int4;
begin
	-- ----
	-- Grab the central configuration lock
	-- ----
	lock table "_slony_cursos".sl_config_lock;

	-- ----
	-- Get the tables current set
	-- ----
	select tab_set into v_old_set_id from "_slony_cursos".sl_table
			where tab_id = p_tab_id;
	if not found then
		raise exception 'Slony-I: table %d not found', p_tab_id;
	end if;
	
	-- ----
	-- Check that both sets exist and originate here
	-- ----
	if p_new_set_id = v_old_set_id then
		raise exception 'Slony-I: set ids cannot be identical';
	end if;
	select set_origin into v_origin from "_slony_cursos".sl_set
			where set_id = p_new_set_id;
	if not found then
		raise exception 'Slony-I: set % not found', p_new_set_id;
	end if;
	if v_origin != "_slony_cursos".getLocalNodeId('_slony_cursos') then
		raise exception 'Slony-I: set % does not originate on local node',
				p_new_set_id;
	end if;

	select set_origin into v_origin from "_slony_cursos".sl_set
			where set_id = v_old_set_id;
	if not found then
		raise exception 'Slony-I: set % not found', v_old_set_id;
	end if;
	if v_origin != "_slony_cursos".getLocalNodeId('_slony_cursos') then
		raise exception 'Slony-I: set % does not originate on local node',
				v_old_set_id;
	end if;

	-- ----
	-- Check that both sets are subscribed by the same set of nodes
	-- ----
	if exists (select true from "_slony_cursos".sl_subscribe SUB1
				where SUB1.sub_set = p_new_set_id
				and SUB1.sub_receiver not in (select SUB2.sub_receiver
						from "_slony_cursos".sl_subscribe SUB2
						where SUB2.sub_set = v_old_set_id))
	then
		raise exception 'Slony-I: subscriber lists of set % and % are different',
				p_new_set_id, v_old_set_id;
	end if;

	if exists (select true from "_slony_cursos".sl_subscribe SUB1
				where SUB1.sub_set = v_old_set_id
				and SUB1.sub_receiver not in (select SUB2.sub_receiver
						from "_slony_cursos".sl_subscribe SUB2
						where SUB2.sub_set = p_new_set_id))
	then
		raise exception 'Slony-I: subscriber lists of set % and % are different',
				v_old_set_id, p_new_set_id;
	end if;

	-- ----
	-- Change the set the table belongs to
	-- ----
	perform "_slony_cursos".createEvent('_slony_cursos', 'SYNC', NULL);
	perform "_slony_cursos".setMoveTable_int(p_tab_id, p_new_set_id);
	return  "_slony_cursos".createEvent('_slony_cursos', 'SET_MOVE_TABLE', 
			p_tab_id::text, p_new_set_id::text);
end;
$$;


ALTER FUNCTION _slony_cursos.setmovetable(p_tab_id integer, p_new_set_id integer) OWNER TO postgres;

--
-- TOC entry 2739 (class 0 OID 0)
-- Dependencies: 320
-- Name: FUNCTION setmovetable(p_tab_id integer, p_new_set_id integer); Type: COMMENT; Schema: _slony_cursos; Owner: postgres
--

COMMENT ON FUNCTION setmovetable(p_tab_id integer, p_new_set_id integer) IS 'This processes the SET_MOVE_TABLE event.  The table is moved 
to the destination set.';


--
-- TOC entry 326 (class 1255 OID 27642)
-- Name: setmovetable_int(integer, integer); Type: FUNCTION; Schema: _slony_cursos; Owner: postgres
--

CREATE FUNCTION setmovetable_int(p_tab_id integer, p_new_set_id integer) RETURNS integer
    LANGUAGE plpgsql
    AS $$
begin
	-- ----
	-- Grab the central configuration lock
	-- ----
	lock table "_slony_cursos".sl_config_lock;

	-- ----
	-- Move the table to the new set
	-- ----
	update "_slony_cursos".sl_table
			set tab_set = p_new_set_id
			where tab_id = p_tab_id;

	return p_tab_id;
end;
$$;


ALTER FUNCTION _slony_cursos.setmovetable_int(p_tab_id integer, p_new_set_id integer) OWNER TO postgres;

--
-- TOC entry 377 (class 1255 OID 27699)
-- Name: shouldslonyvacuumtable(name, name); Type: FUNCTION; Schema: _slony_cursos; Owner: postgres
--

CREATE FUNCTION shouldslonyvacuumtable(i_nspname name, i_tblname name) RETURNS boolean
    LANGUAGE plpgsql
    AS $$
declare
	c_table oid;
	c_namespace oid;
	c_enabled boolean;
	v_dummy int4;
begin
	if not exists (select 1 from pg_catalog.pg_class c, pg_catalog.pg_namespace n 
                   where c.relname = i_tblname and n.nspname = i_nspname and c.relnamespace = n.oid) then
        return 'f'::boolean;   -- if table does not exist, then don't vacuum
	end if;				
	select 1 into v_dummy from "pg_catalog".pg_settings where name = 'autovacuum' and setting = 'on';
	if not found then
		return 't'::boolean;       -- If autovac is turned off, then we gotta vacuum
	end if;
	
	select into c_namespace oid from "pg_catalog".pg_namespace where nspname = i_nspname;
	if not found then
		raise exception 'Slony-I: namespace % does not exist', i_nspname;
	end if;
	select into c_table oid from "pg_catalog".pg_class where relname = i_tblname and relnamespace = c_namespace;
	if not found then
		raise warning 'Slony-I: table % does not exist in namespace %/%', i_tblname, c_namespace, i_nspname;
		return 'f'::boolean;
	end if;
	
	-- So, the table is legit; try to look it up for autovacuum policy
	if exists (select 1 from pg_class where 'autovacuum_enabled=off' = any (reloptions) and oid = c_table) then
		return 't'::boolean;   -- Autovac is turned on, but this table is disabled
	end if;

	return 'f'::boolean;

end;$$;


ALTER FUNCTION _slony_cursos.shouldslonyvacuumtable(i_nspname name, i_tblname name) OWNER TO postgres;

--
-- TOC entry 2740 (class 0 OID 0)
-- Dependencies: 377
-- Name: FUNCTION shouldslonyvacuumtable(i_nspname name, i_tblname name); Type: COMMENT; Schema: _slony_cursos; Owner: postgres
--

COMMENT ON FUNCTION shouldslonyvacuumtable(i_nspname name, i_tblname name) IS 'returns false if autovacuum handles vacuuming of the table, or if the table does not exist; returns true if Slony-I should manage it';


--
-- TOC entry 368 (class 1255 OID 27690)
-- Name: slon_node_health_check(); Type: FUNCTION; Schema: _slony_cursos; Owner: postgres
--

CREATE FUNCTION slon_node_health_check() RETURNS boolean
    LANGUAGE plpgsql
    AS $$
declare
		prec record;
		all_ok boolean;
begin
		all_ok := 't'::boolean;
		-- validate that all tables in sl_table have:
		--      sl_table agreeing with pg_class
		for prec in select tab_id, tab_relname, tab_nspname from
		"_slony_cursos".sl_table t where not exists (select 1 from pg_catalog.pg_class c, pg_catalog.pg_namespace n
				where c.oid = t.tab_reloid and c.relname = t.tab_relname and c.relnamespace = n.oid and n.nspname = t.tab_nspname) loop
				all_ok := 'f'::boolean;
				raise warning 'table [id,nsp,name]=[%,%,%] - sl_table does not match pg_class/pg_namespace', prec.tab_id, prec.tab_relname, prec.tab_nspname;
		end loop;
		if not all_ok then
		   raise warning 'Mismatch found between sl_table and pg_class.  Slonik command REPAIR CONFIG may be useful to rectify this.';
		end if;
		return all_ok;
end
$$;


ALTER FUNCTION _slony_cursos.slon_node_health_check() OWNER TO postgres;

--
-- TOC entry 2741 (class 0 OID 0)
-- Dependencies: 368
-- Name: FUNCTION slon_node_health_check(); Type: COMMENT; Schema: _slony_cursos; Owner: postgres
--

COMMENT ON FUNCTION slon_node_health_check() IS 'called when slon starts up to validate that there are not problems with node configuration.  Returns t if all is OK, f if there is a problem.';


--
-- TOC entry 266 (class 1255 OID 27581)
-- Name: slon_quote_brute(text); Type: FUNCTION; Schema: _slony_cursos; Owner: postgres
--

CREATE FUNCTION slon_quote_brute(p_tab_fqname text) RETURNS text
    LANGUAGE plpgsql IMMUTABLE
    AS $$
declare	
    v_fqname text default '';
begin
    v_fqname := '"' || replace(p_tab_fqname,'"','""') || '"';
    return v_fqname;
end;
$$;


ALTER FUNCTION _slony_cursos.slon_quote_brute(p_tab_fqname text) OWNER TO postgres;

--
-- TOC entry 2742 (class 0 OID 0)
-- Dependencies: 266
-- Name: FUNCTION slon_quote_brute(p_tab_fqname text); Type: COMMENT; Schema: _slony_cursos; Owner: postgres
--

COMMENT ON FUNCTION slon_quote_brute(p_tab_fqname text) IS 'Brutally quote the given text';


--
-- TOC entry 267 (class 1255 OID 27582)
-- Name: slon_quote_input(text); Type: FUNCTION; Schema: _slony_cursos; Owner: postgres
--

CREATE FUNCTION slon_quote_input(p_tab_fqname text) RETURNS text
    LANGUAGE plpgsql IMMUTABLE
    AS $$
  declare
     v_nsp_name text;
     v_tab_name text;
	 v_i integer;
	 v_l integer;
     v_pq2 integer;
begin
	v_l := length(p_tab_fqname);

	-- Let us search for the dot
	if p_tab_fqname like '"%' then
		-- if the first part of the ident starts with a double quote, search
		-- for the closing double quote, skipping over double double quotes.
		v_i := 2;
		while v_i <= v_l loop
			if substr(p_tab_fqname, v_i, 1) != '"' then
				v_i := v_i + 1;
			else
				v_i := v_i + 1;
				if substr(p_tab_fqname, v_i, 1) != '"' then
					exit;
				end if;
				v_i := v_i + 1;
			end if;
		end loop;
	else
		-- first part of ident is not quoted, search for the dot directly
		v_i := 1;
		while v_i <= v_l loop
			if substr(p_tab_fqname, v_i, 1) = '.' then
				exit;
			end if;
			v_i := v_i + 1;
		end loop;
	end if;

	-- v_i now points at the dot or behind the string.

	if substr(p_tab_fqname, v_i, 1) = '.' then
		-- There is a dot now, so split the ident into its namespace
		-- and objname parts and make sure each is quoted
		v_nsp_name := substr(p_tab_fqname, 1, v_i - 1);
		v_tab_name := substr(p_tab_fqname, v_i + 1);
		if v_nsp_name not like '"%' then
			v_nsp_name := '"' || replace(v_nsp_name, '"', '""') ||
						  '"';
		end if;
		if v_tab_name not like '"%' then
			v_tab_name := '"' || replace(v_tab_name, '"', '""') ||
						  '"';
		end if;

		return v_nsp_name || '.' || v_tab_name;
	else
		-- No dot ... must be just an ident without schema
		if p_tab_fqname like '"%' then
			return p_tab_fqname;
		else
			return '"' || replace(p_tab_fqname, '"', '""') || '"';
		end if;
	end if;

end;$$;


ALTER FUNCTION _slony_cursos.slon_quote_input(p_tab_fqname text) OWNER TO postgres;

--
-- TOC entry 2743 (class 0 OID 0)
-- Dependencies: 267
-- Name: FUNCTION slon_quote_input(p_tab_fqname text); Type: COMMENT; Schema: _slony_cursos; Owner: postgres
--

COMMENT ON FUNCTION slon_quote_input(p_tab_fqname text) IS 'quote all words that aren''t quoted yet';


--
-- TOC entry 271 (class 1255 OID 27586)
-- Name: slonyversion(); Type: FUNCTION; Schema: _slony_cursos; Owner: postgres
--

CREATE FUNCTION slonyversion() RETURNS text
    LANGUAGE plpgsql
    AS $$
begin
	return "_slony_cursos".slonyVersionMajor()::text || '.' || 
	       "_slony_cursos".slonyVersionMinor()::text || '.' || 
	       "_slony_cursos".slonyVersionPatchlevel()::text   ;
end;
$$;


ALTER FUNCTION _slony_cursos.slonyversion() OWNER TO postgres;

--
-- TOC entry 2744 (class 0 OID 0)
-- Dependencies: 271
-- Name: FUNCTION slonyversion(); Type: COMMENT; Schema: _slony_cursos; Owner: postgres
--

COMMENT ON FUNCTION slonyversion() IS 'Returns the version number of the slony schema';


--
-- TOC entry 268 (class 1255 OID 27583)
-- Name: slonyversionmajor(); Type: FUNCTION; Schema: _slony_cursos; Owner: postgres
--

CREATE FUNCTION slonyversionmajor() RETURNS integer
    LANGUAGE plpgsql
    AS $$
begin
	return 2;
end;
$$;


ALTER FUNCTION _slony_cursos.slonyversionmajor() OWNER TO postgres;

--
-- TOC entry 2745 (class 0 OID 0)
-- Dependencies: 268
-- Name: FUNCTION slonyversionmajor(); Type: COMMENT; Schema: _slony_cursos; Owner: postgres
--

COMMENT ON FUNCTION slonyversionmajor() IS 'Returns the major version number of the slony schema';


--
-- TOC entry 269 (class 1255 OID 27584)
-- Name: slonyversionminor(); Type: FUNCTION; Schema: _slony_cursos; Owner: postgres
--

CREATE FUNCTION slonyversionminor() RETURNS integer
    LANGUAGE plpgsql
    AS $$
begin
	return 2;
end;
$$;


ALTER FUNCTION _slony_cursos.slonyversionminor() OWNER TO postgres;

--
-- TOC entry 2746 (class 0 OID 0)
-- Dependencies: 269
-- Name: FUNCTION slonyversionminor(); Type: COMMENT; Schema: _slony_cursos; Owner: postgres
--

COMMENT ON FUNCTION slonyversionminor() IS 'Returns the minor version number of the slony schema';


--
-- TOC entry 270 (class 1255 OID 27585)
-- Name: slonyversionpatchlevel(); Type: FUNCTION; Schema: _slony_cursos; Owner: postgres
--

CREATE FUNCTION slonyversionpatchlevel() RETURNS integer
    LANGUAGE plpgsql
    AS $$
begin
	return 4;
end;
$$;


ALTER FUNCTION _slony_cursos.slonyversionpatchlevel() OWNER TO postgres;

--
-- TOC entry 2747 (class 0 OID 0)
-- Dependencies: 270
-- Name: FUNCTION slonyversionpatchlevel(); Type: COMMENT; Schema: _slony_cursos; Owner: postgres
--

COMMENT ON FUNCTION slonyversionpatchlevel() IS 'Returns the version patch level of the slony schema';


--
-- TOC entry 371 (class 1255 OID 27693)
-- Name: store_application_name(text); Type: FUNCTION; Schema: _slony_cursos; Owner: postgres
--

CREATE FUNCTION store_application_name(i_name text) RETURNS text
    LANGUAGE plpgsql
    AS $$
declare
		p_command text;
begin
		if exists (select 1 from pg_catalog.pg_settings where name = 'application_name') then
		   p_command := 'set application_name to '''|| i_name || ''';';
		   execute p_command;
		   return i_name;
		end if;
		return NULL::text;
end $$;


ALTER FUNCTION _slony_cursos.store_application_name(i_name text) OWNER TO postgres;

--
-- TOC entry 2748 (class 0 OID 0)
-- Dependencies: 371
-- Name: FUNCTION store_application_name(i_name text); Type: COMMENT; Schema: _slony_cursos; Owner: postgres
--

COMMENT ON FUNCTION store_application_name(i_name text) IS 'Set application_name GUC, if possible.  Returns NULL if it fails to work.';


--
-- TOC entry 302 (class 1255 OID 27617)
-- Name: storelisten(integer, integer, integer); Type: FUNCTION; Schema: _slony_cursos; Owner: postgres
--

CREATE FUNCTION storelisten(p_origin integer, p_provider integer, p_receiver integer) RETURNS bigint
    LANGUAGE plpgsql
    AS $$
begin
	perform "_slony_cursos".storeListen_int (p_origin, p_provider, p_receiver);
	return  "_slony_cursos".createEvent ('_slony_cursos', 'STORE_LISTEN',
			p_origin::text, p_provider::text, p_receiver::text);
end;
$$;


ALTER FUNCTION _slony_cursos.storelisten(p_origin integer, p_provider integer, p_receiver integer) OWNER TO postgres;

--
-- TOC entry 2749 (class 0 OID 0)
-- Dependencies: 302
-- Name: FUNCTION storelisten(p_origin integer, p_provider integer, p_receiver integer); Type: COMMENT; Schema: _slony_cursos; Owner: postgres
--

COMMENT ON FUNCTION storelisten(p_origin integer, p_provider integer, p_receiver integer) IS 'FUNCTION storeListen (li_origin, li_provider, li_receiver)

generate STORE_LISTEN event, indicating that receiver node li_receiver
listens to node li_provider in order to get messages coming from node
li_origin.';


--
-- TOC entry 303 (class 1255 OID 27618)
-- Name: storelisten_int(integer, integer, integer); Type: FUNCTION; Schema: _slony_cursos; Owner: postgres
--

CREATE FUNCTION storelisten_int(p_li_origin integer, p_li_provider integer, p_li_receiver integer) RETURNS integer
    LANGUAGE plpgsql
    AS $$
declare
	v_exists		int4;
begin
	-- ----
	-- Grab the central configuration lock
	-- ----
	lock table "_slony_cursos".sl_config_lock;

	select 1 into v_exists
			from "_slony_cursos".sl_listen
			where li_origin = p_li_origin
			and li_provider = p_li_provider
			and li_receiver = p_li_receiver;
	if not found then
		-- ----
		-- In case we receive STORE_LISTEN events before we know
		-- about the nodes involved in this, we generate those nodes
		-- as pending.
		-- ----
		if not exists (select 1 from "_slony_cursos".sl_node
						where no_id = p_li_origin) then
			perform "_slony_cursos".storeNode_int (p_li_origin, '<event pending>');
		end if;
		if not exists (select 1 from "_slony_cursos".sl_node
						where no_id = p_li_provider) then
			perform "_slony_cursos".storeNode_int (p_li_provider, '<event pending>');
		end if;
		if not exists (select 1 from "_slony_cursos".sl_node
						where no_id = p_li_receiver) then
			perform "_slony_cursos".storeNode_int (p_li_receiver, '<event pending>');
		end if;

		insert into "_slony_cursos".sl_listen
				(li_origin, li_provider, li_receiver) values
				(p_li_origin, p_li_provider, p_li_receiver);
	end if;

	return 0;
end;
$$;


ALTER FUNCTION _slony_cursos.storelisten_int(p_li_origin integer, p_li_provider integer, p_li_receiver integer) OWNER TO postgres;

--
-- TOC entry 2750 (class 0 OID 0)
-- Dependencies: 303
-- Name: FUNCTION storelisten_int(p_li_origin integer, p_li_provider integer, p_li_receiver integer); Type: COMMENT; Schema: _slony_cursos; Owner: postgres
--

COMMENT ON FUNCTION storelisten_int(p_li_origin integer, p_li_provider integer, p_li_receiver integer) IS 'FUNCTION storeListen_int (li_origin, li_provider, li_receiver)

Process STORE_LISTEN event, indicating that receiver node li_receiver
listens to node li_provider in order to get messages coming from node
li_origin.';


--
-- TOC entry 282 (class 1255 OID 27596)
-- Name: storenode(integer, text); Type: FUNCTION; Schema: _slony_cursos; Owner: postgres
--

CREATE FUNCTION storenode(p_no_id integer, p_no_comment text) RETURNS bigint
    LANGUAGE plpgsql
    AS $$
begin
	perform "_slony_cursos".storeNode_int (p_no_id, p_no_comment);
	return  "_slony_cursos".createEvent('_slony_cursos', 'STORE_NODE',
									p_no_id::text, p_no_comment::text);
end;
$$;


ALTER FUNCTION _slony_cursos.storenode(p_no_id integer, p_no_comment text) OWNER TO postgres;

--
-- TOC entry 2751 (class 0 OID 0)
-- Dependencies: 282
-- Name: FUNCTION storenode(p_no_id integer, p_no_comment text); Type: COMMENT; Schema: _slony_cursos; Owner: postgres
--

COMMENT ON FUNCTION storenode(p_no_id integer, p_no_comment text) IS 'no_id - Node ID #
no_comment - Human-oriented comment

Generate the STORE_NODE event for node no_id';


--
-- TOC entry 283 (class 1255 OID 27597)
-- Name: storenode_int(integer, text); Type: FUNCTION; Schema: _slony_cursos; Owner: postgres
--

CREATE FUNCTION storenode_int(p_no_id integer, p_no_comment text) RETURNS integer
    LANGUAGE plpgsql
    AS $$
declare
	v_old_row		record;
begin
	-- ----
	-- Grab the central configuration lock
	-- ----
	lock table "_slony_cursos".sl_config_lock;

	-- ----
	-- Check if the node exists
	-- ----
	select * into v_old_row
			from "_slony_cursos".sl_node
			where no_id = p_no_id
			for update;
	if found then 
		-- ----
		-- Node exists, update the existing row.
		-- ----
		update "_slony_cursos".sl_node
				set no_comment = p_no_comment
				where no_id = p_no_id;
	else
		-- ----
		-- New node, insert the sl_node row
		-- ----
		insert into "_slony_cursos".sl_node
				(no_id, no_active, no_comment,no_failed) values
				(p_no_id, 'f', p_no_comment,false);
	end if;

	return p_no_id;
end;
$$;


ALTER FUNCTION _slony_cursos.storenode_int(p_no_id integer, p_no_comment text) OWNER TO postgres;

--
-- TOC entry 2752 (class 0 OID 0)
-- Dependencies: 283
-- Name: FUNCTION storenode_int(p_no_id integer, p_no_comment text); Type: COMMENT; Schema: _slony_cursos; Owner: postgres
--

COMMENT ON FUNCTION storenode_int(p_no_id integer, p_no_comment text) IS 'no_id - Node ID #
no_comment - Human-oriented comment

Internal function to process the STORE_NODE event for node no_id';


--
-- TOC entry 298 (class 1255 OID 27613)
-- Name: storepath(integer, integer, text, integer); Type: FUNCTION; Schema: _slony_cursos; Owner: postgres
--

CREATE FUNCTION storepath(p_pa_server integer, p_pa_client integer, p_pa_conninfo text, p_pa_connretry integer) RETURNS bigint
    LANGUAGE plpgsql
    AS $$
begin
	perform "_slony_cursos".storePath_int(p_pa_server, p_pa_client,
			p_pa_conninfo, p_pa_connretry);
	return  "_slony_cursos".createEvent('_slony_cursos', 'STORE_PATH', 
			p_pa_server::text, p_pa_client::text, 
			p_pa_conninfo::text, p_pa_connretry::text);
end;
$$;


ALTER FUNCTION _slony_cursos.storepath(p_pa_server integer, p_pa_client integer, p_pa_conninfo text, p_pa_connretry integer) OWNER TO postgres;

--
-- TOC entry 2753 (class 0 OID 0)
-- Dependencies: 298
-- Name: FUNCTION storepath(p_pa_server integer, p_pa_client integer, p_pa_conninfo text, p_pa_connretry integer); Type: COMMENT; Schema: _slony_cursos; Owner: postgres
--

COMMENT ON FUNCTION storepath(p_pa_server integer, p_pa_client integer, p_pa_conninfo text, p_pa_connretry integer) IS 'FUNCTION storePath (pa_server, pa_client, pa_conninfo, pa_connretry)

Generate the STORE_PATH event indicating that node pa_client can
access node pa_server using DSN pa_conninfo';


--
-- TOC entry 299 (class 1255 OID 27614)
-- Name: storepath_int(integer, integer, text, integer); Type: FUNCTION; Schema: _slony_cursos; Owner: postgres
--

CREATE FUNCTION storepath_int(p_pa_server integer, p_pa_client integer, p_pa_conninfo text, p_pa_connretry integer) RETURNS integer
    LANGUAGE plpgsql
    AS $$
declare
	v_dummy			int4;
begin
	-- ----
	-- Grab the central configuration lock
	-- ----
	lock table "_slony_cursos".sl_config_lock;

	-- ----
	-- Check if the path already exists
	-- ----
	select 1 into v_dummy
			from "_slony_cursos".sl_path
			where pa_server = p_pa_server
			and pa_client = p_pa_client
			for update;
	if found then
		-- ----
		-- Path exists, update pa_conninfo
		-- ----
		update "_slony_cursos".sl_path
				set pa_conninfo = p_pa_conninfo,
					pa_connretry = p_pa_connretry
				where pa_server = p_pa_server
				and pa_client = p_pa_client;
	else
		-- ----
		-- New path
		--
		-- In case we receive STORE_PATH events before we know
		-- about the nodes involved in this, we generate those nodes
		-- as pending.
		-- ----
		if not exists (select 1 from "_slony_cursos".sl_node
						where no_id = p_pa_server) then
			perform "_slony_cursos".storeNode_int (p_pa_server, '<event pending>');
		end if;
		if not exists (select 1 from "_slony_cursos".sl_node
						where no_id = p_pa_client) then
			perform "_slony_cursos".storeNode_int (p_pa_client, '<event pending>');
		end if;
		insert into "_slony_cursos".sl_path
				(pa_server, pa_client, pa_conninfo, pa_connretry) values
				(p_pa_server, p_pa_client, p_pa_conninfo, p_pa_connretry);
	end if;

	-- Rewrite sl_listen table
	perform "_slony_cursos".RebuildListenEntries();

	return 0;
end;
$$;


ALTER FUNCTION _slony_cursos.storepath_int(p_pa_server integer, p_pa_client integer, p_pa_conninfo text, p_pa_connretry integer) OWNER TO postgres;

--
-- TOC entry 2754 (class 0 OID 0)
-- Dependencies: 299
-- Name: FUNCTION storepath_int(p_pa_server integer, p_pa_client integer, p_pa_conninfo text, p_pa_connretry integer); Type: COMMENT; Schema: _slony_cursos; Owner: postgres
--

COMMENT ON FUNCTION storepath_int(p_pa_server integer, p_pa_client integer, p_pa_conninfo text, p_pa_connretry integer) IS 'FUNCTION storePath (pa_server, pa_client, pa_conninfo, pa_connretry)

Process the STORE_PATH event indicating that node pa_client can
access node pa_server using DSN pa_conninfo';


--
-- TOC entry 306 (class 1255 OID 27621)
-- Name: storeset(integer, text); Type: FUNCTION; Schema: _slony_cursos; Owner: postgres
--

CREATE FUNCTION storeset(p_set_id integer, p_set_comment text) RETURNS bigint
    LANGUAGE plpgsql
    AS $$
declare
	v_local_node_id		int4;
begin
	-- ----
	-- Grab the central configuration lock
	-- ----
	lock table "_slony_cursos".sl_config_lock;

	v_local_node_id := "_slony_cursos".getLocalNodeId('_slony_cursos');

	insert into "_slony_cursos".sl_set
			(set_id, set_origin, set_comment) values
			(p_set_id, v_local_node_id, p_set_comment);

	return "_slony_cursos".createEvent('_slony_cursos', 'STORE_SET', 
			p_set_id::text, v_local_node_id::text, p_set_comment::text);
end;
$$;


ALTER FUNCTION _slony_cursos.storeset(p_set_id integer, p_set_comment text) OWNER TO postgres;

--
-- TOC entry 2755 (class 0 OID 0)
-- Dependencies: 306
-- Name: FUNCTION storeset(p_set_id integer, p_set_comment text); Type: COMMENT; Schema: _slony_cursos; Owner: postgres
--

COMMENT ON FUNCTION storeset(p_set_id integer, p_set_comment text) IS 'Generate STORE_SET event for set set_id with human readable comment set_comment';


--
-- TOC entry 307 (class 1255 OID 27622)
-- Name: storeset_int(integer, integer, text); Type: FUNCTION; Schema: _slony_cursos; Owner: postgres
--

CREATE FUNCTION storeset_int(p_set_id integer, p_set_origin integer, p_set_comment text) RETURNS integer
    LANGUAGE plpgsql
    AS $$
declare
	v_dummy				int4;
begin
	-- ----
	-- Grab the central configuration lock
	-- ----
	lock table "_slony_cursos".sl_config_lock;

	select 1 into v_dummy
			from "_slony_cursos".sl_set
			where set_id = p_set_id
			for update;
	if found then 
		update "_slony_cursos".sl_set
				set set_comment = p_set_comment
				where set_id = p_set_id;
	else
		if not exists (select 1 from "_slony_cursos".sl_node
						where no_id = p_set_origin) then
			perform "_slony_cursos".storeNode_int (p_set_origin, '<event pending>');
		end if;
		insert into "_slony_cursos".sl_set
				(set_id, set_origin, set_comment) values
				(p_set_id, p_set_origin, p_set_comment);
	end if;

	-- Run addPartialLogIndices() to try to add indices to unused sl_log_? table
	perform "_slony_cursos".addPartialLogIndices();

	return p_set_id;
end;
$$;


ALTER FUNCTION _slony_cursos.storeset_int(p_set_id integer, p_set_origin integer, p_set_comment text) OWNER TO postgres;

--
-- TOC entry 2756 (class 0 OID 0)
-- Dependencies: 307
-- Name: FUNCTION storeset_int(p_set_id integer, p_set_origin integer, p_set_comment text); Type: COMMENT; Schema: _slony_cursos; Owner: postgres
--

COMMENT ON FUNCTION storeset_int(p_set_id integer, p_set_origin integer, p_set_comment text) IS 'storeSet_int (set_id, set_origin, set_comment)

Process the STORE_SET event, indicating the new set with given ID,
origin node, and human readable comment.';


--
-- TOC entry 338 (class 1255 OID 27653)
-- Name: subscribeset(integer, integer, integer, boolean, boolean); Type: FUNCTION; Schema: _slony_cursos; Owner: postgres
--

CREATE FUNCTION subscribeset(p_sub_set integer, p_sub_provider integer, p_sub_receiver integer, p_sub_forward boolean, p_omit_copy boolean) RETURNS bigint
    LANGUAGE plpgsql
    AS $$
declare
	v_set_origin		int4;
	v_ev_seqno			int8;
	v_ev_seqno2			int8;
	v_rec			record;
begin
	-- ----
	-- Grab the central configuration lock
	-- ----
	lock table "_slony_cursos".sl_config_lock;

	--
	-- Check that the receiver exists
	--
	if not exists (select no_id from "_slony_cursos".sl_node where no_id=
	       	      p_sub_receiver) then
		      raise exception 'Slony-I: subscribeSet() receiver % does not exist' , p_sub_receiver;
	end if;

	--
	-- Check that the provider exists
	--
	if not exists (select no_id from "_slony_cursos".sl_node where no_id=
	       	      p_sub_provider) then
		      raise exception 'Slony-I: subscribeSet() provider % does not exist' , p_sub_provider;
	end if;

	-- ----
	-- Check that the origin and provider of the set are remote
	-- ----
	select set_origin into v_set_origin
			from "_slony_cursos".sl_set
			where set_id = p_sub_set;
	if not found then
		raise exception 'Slony-I: subscribeSet(): set % not found', p_sub_set;
	end if;
	if v_set_origin = p_sub_receiver then
		raise exception 
				'Slony-I: subscribeSet(): set origin and receiver cannot be identical';
	end if;
	if p_sub_receiver = p_sub_provider then
		raise exception 
				'Slony-I: subscribeSet(): set provider and receiver cannot be identical';
	end if;
	-- ----
	-- Check that this is called on the origin node
	-- ----
	if v_set_origin != "_slony_cursos".getLocalNodeId('_slony_cursos') then
		raise exception 'Slony-I: subscribeSet() must be called on origin';
	end if;

	-- ---
	-- Verify that the provider is either the origin or an active subscriber
	-- Bug report #1362
	-- ---
	if v_set_origin <> p_sub_provider then
		if not exists (select 1 from "_slony_cursos".sl_subscribe
			where sub_set = p_sub_set and 
                              sub_receiver = p_sub_provider and
			      sub_forward and sub_active) then
			raise exception 'Slony-I: subscribeSet(): provider % is not an active forwarding node for replication set %', p_sub_provider, p_sub_set;
		end if;
	end if;

	-- ---
	-- Enforce that all sets from one origin are subscribed
	-- using the same data provider per receiver.
	-- ----
	if not exists (select 1 from "_slony_cursos".sl_subscribe
			where sub_set = p_sub_set and sub_receiver = p_sub_receiver) then
		--
		-- New subscription - error out if we have any other subscription
		-- from that origin with a different data provider.
		--
		for v_rec in select sub_provider from "_slony_cursos".sl_subscribe
				join "_slony_cursos".sl_set on set_id = sub_set
				where set_origin = v_set_origin and sub_receiver = p_sub_receiver
		loop
			if v_rec.sub_provider <> p_sub_provider then
				raise exception 'Slony-I: subscribeSet(): wrong provider % - existing subscription from origin % users provider %',
					p_sub_provider, v_set_origin, v_rec.sub_provider;
			end if;
		end loop;
	else
		--
		-- Existing subscription - in case the data provider changes and
		-- there are other subscriptions, warn here. subscribeSet_int()
		-- will currently change the data provider for those sets as well.
		--
		for v_rec in select set_id, sub_provider from "_slony_cursos".sl_subscribe
				join "_slony_cursos".sl_set on set_id = sub_set
				where set_origin = v_set_origin and sub_receiver = p_sub_receiver
				and set_id <> p_sub_set
		loop
			if v_rec.sub_provider <> p_sub_provider then
				raise exception 'Slony-I: subscribeSet(): also data provider for set % use resubscribe instead',
					v_rec.set_id;
			end if;
		end loop;
	end if;

	-- ----
	-- Create the SUBSCRIBE_SET event
	-- ----
	v_ev_seqno :=  "_slony_cursos".createEvent('_slony_cursos', 'SUBSCRIBE_SET', 
			p_sub_set::text, p_sub_provider::text, p_sub_receiver::text, 
			case p_sub_forward when true then 't' else 'f' end,
			case p_omit_copy when true then 't' else 'f' end
                        );

	-- ----
	-- Call the internal procedure to store the subscription
	-- ----
	v_ev_seqno2:="_slony_cursos".subscribeSet_int(p_sub_set, p_sub_provider,
			p_sub_receiver, p_sub_forward, p_omit_copy);
	
	if v_ev_seqno2 is not null then
	   v_ev_seqno:=v_ev_seqno2;
	 end if;

	return v_ev_seqno;
end;
$$;


ALTER FUNCTION _slony_cursos.subscribeset(p_sub_set integer, p_sub_provider integer, p_sub_receiver integer, p_sub_forward boolean, p_omit_copy boolean) OWNER TO postgres;

--
-- TOC entry 2757 (class 0 OID 0)
-- Dependencies: 338
-- Name: FUNCTION subscribeset(p_sub_set integer, p_sub_provider integer, p_sub_receiver integer, p_sub_forward boolean, p_omit_copy boolean); Type: COMMENT; Schema: _slony_cursos; Owner: postgres
--

COMMENT ON FUNCTION subscribeset(p_sub_set integer, p_sub_provider integer, p_sub_receiver integer, p_sub_forward boolean, p_omit_copy boolean) IS 'subscribeSet (sub_set, sub_provider, sub_receiver, sub_forward, omit_copy)

Makes sure that the receiver is not the provider, then stores the
subscription, and publishes the SUBSCRIBE_SET event to other nodes.

If omit_copy is true, then no data copy will be done.
';


--
-- TOC entry 339 (class 1255 OID 27654)
-- Name: subscribeset_int(integer, integer, integer, boolean, boolean); Type: FUNCTION; Schema: _slony_cursos; Owner: postgres
--

CREATE FUNCTION subscribeset_int(p_sub_set integer, p_sub_provider integer, p_sub_receiver integer, p_sub_forward boolean, p_omit_copy boolean) RETURNS integer
    LANGUAGE plpgsql
    AS $$
declare
	v_set_origin		int4;
	v_sub_row			record;
	v_seq_id			bigint;
begin
	-- ----
	-- Grab the central configuration lock
	-- ----
	lock table "_slony_cursos".sl_config_lock;

	-- ----
	-- Lookup the set origin
	-- ----
	select set_origin into v_set_origin
			from "_slony_cursos".sl_set
			where set_id = p_sub_set;
	if not found then
		raise exception 'Slony-I: subscribeSet_int(): set % not found', p_sub_set;
	end if;

	-- ----
	-- Provider change is only allowed for active sets
	-- ----
	if p_sub_receiver = "_slony_cursos".getLocalNodeId('_slony_cursos') then
		select sub_active into v_sub_row from "_slony_cursos".sl_subscribe
				where sub_set = p_sub_set
				and sub_receiver = p_sub_receiver;
		if found then
			if not v_sub_row.sub_active then
				raise exception 'Slony-I: subscribeSet_int(): set % is not active, cannot change provider',
						p_sub_set;
			end if;
		end if;
	end if;

	-- ----
	-- Try to change provider and/or forward for an existing subscription
	-- ----
	update "_slony_cursos".sl_subscribe
			set sub_provider = p_sub_provider,
				sub_forward = p_sub_forward
			where sub_set = p_sub_set
			and sub_receiver = p_sub_receiver;
	if found then
	  
		-- ----
		-- This is changing a subscriptoin. Make sure all sets from
		-- this origin are subscribed using the same data provider.
		-- For this we first check that the requested data provider
		-- is subscribed to all the sets, the receiver is subscribed to.
		-- ----
		for v_sub_row in select set_id from "_slony_cursos".sl_set
				join "_slony_cursos".sl_subscribe on set_id = sub_set
				where set_origin = v_set_origin
				and sub_receiver = p_sub_receiver
				and sub_set <> p_sub_set
		loop
			if not exists (select 1 from "_slony_cursos".sl_subscribe
					where sub_set = v_sub_row.set_id
					and sub_receiver = p_sub_provider
					and sub_active and sub_forward)
				and not exists (select 1 from "_slony_cursos".sl_set
					where set_id = v_sub_row.set_id
					and set_origin = p_sub_provider)
			then
				raise exception 'Slony-I: subscribeSet_int(): node % is not a forwarding subscriber for set %',
						p_sub_provider, v_sub_row.set_id;
			end if;

			-- ----
			-- New data provider offers this set as well, change that
			-- subscription too.
			-- ----
			update "_slony_cursos".sl_subscribe
					set sub_provider = p_sub_provider
					where sub_set = v_sub_row.set_id
					and sub_receiver = p_sub_receiver;
		end loop;

		-- ----
		-- Rewrite sl_listen table
		-- ----
		perform "_slony_cursos".RebuildListenEntries();

		return p_sub_set;
	end if;

	-- ----
	-- Not found, insert a new one
	-- ----
	if not exists (select true from "_slony_cursos".sl_path
			where pa_server = p_sub_provider
			and pa_client = p_sub_receiver)
	then
		insert into "_slony_cursos".sl_path
				(pa_server, pa_client, pa_conninfo, pa_connretry)
				values 
				(p_sub_provider, p_sub_receiver, 
				'<event pending>', 10);
	end if;
	insert into "_slony_cursos".sl_subscribe
			(sub_set, sub_provider, sub_receiver, sub_forward, sub_active)
			values (p_sub_set, p_sub_provider, p_sub_receiver,
				p_sub_forward, false);

	-- ----
	-- If the set origin is here, then enable the subscription
	-- ----
	if v_set_origin = "_slony_cursos".getLocalNodeId('_slony_cursos') then
		select "_slony_cursos".createEvent('_slony_cursos', 'ENABLE_SUBSCRIPTION', 
				p_sub_set::text, p_sub_provider::text, p_sub_receiver::text, 
				case p_sub_forward when true then 't' else 'f' end,
				case p_omit_copy when true then 't' else 'f' end
				) into v_seq_id;
		perform "_slony_cursos".enableSubscription(p_sub_set, 
				p_sub_provider, p_sub_receiver);
	end if;
	
	-- ----
	-- Rewrite sl_listen table
	-- ----
	perform "_slony_cursos".RebuildListenEntries();

	return p_sub_set;
end;
$$;


ALTER FUNCTION _slony_cursos.subscribeset_int(p_sub_set integer, p_sub_provider integer, p_sub_receiver integer, p_sub_forward boolean, p_omit_copy boolean) OWNER TO postgres;

--
-- TOC entry 2758 (class 0 OID 0)
-- Dependencies: 339
-- Name: FUNCTION subscribeset_int(p_sub_set integer, p_sub_provider integer, p_sub_receiver integer, p_sub_forward boolean, p_omit_copy boolean); Type: COMMENT; Schema: _slony_cursos; Owner: postgres
--

COMMENT ON FUNCTION subscribeset_int(p_sub_set integer, p_sub_provider integer, p_sub_receiver integer, p_sub_forward boolean, p_omit_copy boolean) IS 'subscribeSet_int (sub_set, sub_provider, sub_receiver, sub_forward, omit_copy)

Internal actions for subscribing receiver sub_receiver to subscription
set sub_set.';


--
-- TOC entry 362 (class 1255 OID 27684)
-- Name: tablestovacuum(); Type: FUNCTION; Schema: _slony_cursos; Owner: postgres
--

CREATE FUNCTION tablestovacuum() RETURNS SETOF vactables
    LANGUAGE plpgsql
    AS $$
declare
	prec "_slony_cursos".vactables%rowtype;
begin
	prec.nspname := '_slony_cursos';
	prec.relname := 'sl_event';
	if "_slony_cursos".ShouldSlonyVacuumTable(prec.nspname, prec.relname) then
		return next prec;
	end if;
	prec.nspname := '_slony_cursos';
	prec.relname := 'sl_confirm';
	if "_slony_cursos".ShouldSlonyVacuumTable(prec.nspname, prec.relname) then
		return next prec;
	end if;
	prec.nspname := '_slony_cursos';
	prec.relname := 'sl_setsync';
	if "_slony_cursos".ShouldSlonyVacuumTable(prec.nspname, prec.relname) then
		return next prec;
	end if;
	prec.nspname := '_slony_cursos';
	prec.relname := 'sl_seqlog';
	if "_slony_cursos".ShouldSlonyVacuumTable(prec.nspname, prec.relname) then
		return next prec;
	end if;
	prec.nspname := '_slony_cursos';
	prec.relname := 'sl_archive_counter';
	if "_slony_cursos".ShouldSlonyVacuumTable(prec.nspname, prec.relname) then
		return next prec;
	end if;
	prec.nspname := '_slony_cursos';
	prec.relname := 'sl_components';
	if "_slony_cursos".ShouldSlonyVacuumTable(prec.nspname, prec.relname) then
		return next prec;
	end if;
	prec.nspname := '_slony_cursos';
	prec.relname := 'sl_log_script';
	if "_slony_cursos".ShouldSlonyVacuumTable(prec.nspname, prec.relname) then
		return next prec;
	end if;
	prec.nspname := 'pg_catalog';
	prec.relname := 'pg_listener';
	if "_slony_cursos".ShouldSlonyVacuumTable(prec.nspname, prec.relname) then
		return next prec;
	end if;
	prec.nspname := 'pg_catalog';
	prec.relname := 'pg_statistic';
	if "_slony_cursos".ShouldSlonyVacuumTable(prec.nspname, prec.relname) then
		return next prec;
	end if;

   return;
end
$$;


ALTER FUNCTION _slony_cursos.tablestovacuum() OWNER TO postgres;

--
-- TOC entry 2759 (class 0 OID 0)
-- Dependencies: 362
-- Name: FUNCTION tablestovacuum(); Type: COMMENT; Schema: _slony_cursos; Owner: postgres
--

COMMENT ON FUNCTION tablestovacuum() IS 'Return a list of tables that require frequent vacuuming.  The
function is used so that the list is not hardcoded into C code.';


--
-- TOC entry 263 (class 1255 OID 27578)
-- Name: terminatenodeconnections(integer); Type: FUNCTION; Schema: _slony_cursos; Owner: postgres
--

CREATE FUNCTION terminatenodeconnections(p_failed_node integer) RETURNS integer
    LANGUAGE plpgsql
    AS $$
declare
	v_row			record;
begin
	for v_row in select nl_nodeid, nl_conncnt,
			nl_backendpid from "_slony_cursos".sl_nodelock
			where nl_nodeid = p_failed_node for update
	loop
		perform "_slony_cursos".killBackend(v_row.nl_backendpid, 'TERM');
		delete from "_slony_cursos".sl_nodelock
			where nl_nodeid = v_row.nl_nodeid
			and nl_conncnt = v_row.nl_conncnt;
	end loop;

	return 0;
end;
$$;


ALTER FUNCTION _slony_cursos.terminatenodeconnections(p_failed_node integer) OWNER TO postgres;

--
-- TOC entry 2760 (class 0 OID 0)
-- Dependencies: 263
-- Name: FUNCTION terminatenodeconnections(p_failed_node integer); Type: COMMENT; Schema: _slony_cursos; Owner: postgres
--

COMMENT ON FUNCTION terminatenodeconnections(p_failed_node integer) IS 'terminates all backends that have registered to be from the given node';


--
-- TOC entry 378 (class 1255 OID 27700)
-- Name: truncateonlytable(name); Type: FUNCTION; Schema: _slony_cursos; Owner: postgres
--

CREATE FUNCTION truncateonlytable(name) RETURNS void
    LANGUAGE plpgsql
    AS $_$
begin
	execute 'truncate only '|| "_slony_cursos".slon_quote_input($1);
end;
$_$;


ALTER FUNCTION _slony_cursos.truncateonlytable(name) OWNER TO postgres;

--
-- TOC entry 2761 (class 0 OID 0)
-- Dependencies: 378
-- Name: FUNCTION truncateonlytable(name); Type: COMMENT; Schema: _slony_cursos; Owner: postgres
--

COMMENT ON FUNCTION truncateonlytable(name) IS 'Calls TRUNCATE ONLY, syntax supported in version >= 8.4';


--
-- TOC entry 294 (class 1255 OID 27609)
-- Name: uninstallnode(); Type: FUNCTION; Schema: _slony_cursos; Owner: postgres
--

CREATE FUNCTION uninstallnode() RETURNS integer
    LANGUAGE plpgsql
    AS $$
declare
	v_tab_row		record;
begin
	raise notice 'Slony-I: Please drop schema "_slony_cursos"';
	return 0;
end;
$$;


ALTER FUNCTION _slony_cursos.uninstallnode() OWNER TO postgres;

--
-- TOC entry 2762 (class 0 OID 0)
-- Dependencies: 294
-- Name: FUNCTION uninstallnode(); Type: COMMENT; Schema: _slony_cursos; Owner: postgres
--

COMMENT ON FUNCTION uninstallnode() IS 'Reset the whole database to standalone by removing the whole
replication system.';


--
-- TOC entry 309 (class 1255 OID 27624)
-- Name: unlockset(integer); Type: FUNCTION; Schema: _slony_cursos; Owner: postgres
--

CREATE FUNCTION unlockset(p_set_id integer) RETURNS integer
    LANGUAGE plpgsql
    AS $$
declare
	v_local_node_id		int4;
	v_set_row			record;
	v_tab_row			record;
begin
	-- ----
	-- Grab the central configuration lock
	-- ----
	lock table "_slony_cursos".sl_config_lock;

	-- ----
	-- Check that the set exists and that we are the origin
	-- and that it is not already locked.
	-- ----
	v_local_node_id := "_slony_cursos".getLocalNodeId('_slony_cursos');
	select * into v_set_row from "_slony_cursos".sl_set
			where set_id = p_set_id
			for update;
	if not found then
		raise exception 'Slony-I: set % not found', p_set_id;
	end if;
	if v_set_row.set_origin <> v_local_node_id then
		raise exception 'Slony-I: set % does not originate on local node',
				p_set_id;
	end if;
	if v_set_row.set_locked isnull then
		raise exception 'Slony-I: set % is not locked', p_set_id;
	end if;

	-- ----
	-- Drop the lockedSet trigger from all tables in the set.
	-- ----
	for v_tab_row in select T.tab_id,
			"_slony_cursos".slon_quote_brute(PGN.nspname) || '.' ||
			"_slony_cursos".slon_quote_brute(PGC.relname) as tab_fqname
			from "_slony_cursos".sl_table T,
				"pg_catalog".pg_class PGC, "pg_catalog".pg_namespace PGN
			where T.tab_set = p_set_id
				and T.tab_reloid = PGC.oid
				and PGC.relnamespace = PGN.oid
			order by tab_id
	loop
		execute 'drop trigger "_slony_cursos_lockedset" ' || 
				'on ' || v_tab_row.tab_fqname;
	end loop;

	-- ----
	-- Clear out the set_locked field
	-- ----
	update "_slony_cursos".sl_set
			set set_locked = NULL
			where set_id = p_set_id;

	return p_set_id;
end;
$$;


ALTER FUNCTION _slony_cursos.unlockset(p_set_id integer) OWNER TO postgres;

--
-- TOC entry 2763 (class 0 OID 0)
-- Dependencies: 309
-- Name: FUNCTION unlockset(p_set_id integer); Type: COMMENT; Schema: _slony_cursos; Owner: postgres
--

COMMENT ON FUNCTION unlockset(p_set_id integer) IS 'Remove the special trigger from all tables of a set that disables access to it.';


--
-- TOC entry 376 (class 1255 OID 27698)
-- Name: unsubscribe_abandoned_sets(integer); Type: FUNCTION; Schema: _slony_cursos; Owner: postgres
--

CREATE FUNCTION unsubscribe_abandoned_sets(p_failed_node integer) RETURNS bigint
    LANGUAGE plpgsql
    AS $$
declare
v_row record;
v_seq_id bigint;
v_local_node int4;
begin

	select "_slony_cursos".getLocalNodeId('_slony_cursos') into
			   v_local_node;

	if found then
		   --abandon all subscriptions from this origin.
		for v_row in select sub_set,sub_receiver from
			"_slony_cursos".sl_subscribe, "_slony_cursos".sl_set
			where sub_set=set_id and set_origin=p_failed_node
			and sub_receiver=v_local_node
		loop
				raise notice 'Slony-I: failover_abandon_set() is abandoning subscription to set % on node % because it is too far ahead', v_row.sub_set,
				v_local_node;
				--If this node is a provider for the set
				--then the receiver needs to be unsubscribed.
				--
			select "_slony_cursos".unsubscribeSet(v_row.sub_set,
												v_local_node,true)
				   into v_seq_id;
		end loop;
	end if;

	return v_seq_id;
end
$$;


ALTER FUNCTION _slony_cursos.unsubscribe_abandoned_sets(p_failed_node integer) OWNER TO postgres;

--
-- TOC entry 340 (class 1255 OID 27655)
-- Name: unsubscribeset(integer, integer, boolean); Type: FUNCTION; Schema: _slony_cursos; Owner: postgres
--

CREATE FUNCTION unsubscribeset(p_sub_set integer, p_sub_receiver integer, p_force boolean) RETURNS bigint
    LANGUAGE plpgsql
    AS $$
declare
	v_tab_row			record;
begin
	-- ----
	-- Grab the central configuration lock
	-- ----
	lock table "_slony_cursos".sl_config_lock;

	-- ----
	-- Check that this is called on the receiver node
	-- ----
	if p_sub_receiver != "_slony_cursos".getLocalNodeId('_slony_cursos') then
		raise exception 'Slony-I: unsubscribeSet() must be called on receiver';
	end if;



	-- ----
	-- Check that this does not break any chains
	-- ----
	if p_force=false and exists (select true from "_slony_cursos".sl_subscribe
			 where sub_set = p_sub_set
				and sub_provider = p_sub_receiver)
	then
		raise exception 'Slony-I: Cannot unsubscribe set % while being provider',
				p_sub_set;
	end if;

	if exists (select true from "_slony_cursos".sl_subscribe
			where sub_set = p_sub_set
				and sub_provider = p_sub_receiver)
	then
		--delete the receivers of this provider.
		--unsubscribeSet_int() will generate the event
		--when it runs on the receiver.
		delete from "_slony_cursos".sl_subscribe 
			   where sub_set=p_sub_set
			   and sub_provider=p_sub_receiver;
	end if;

	-- ----
	-- Remove the replication triggers.
	-- ----
	for v_tab_row in select tab_id from "_slony_cursos".sl_table
			where tab_set = p_sub_set
			order by tab_id
	loop
		perform "_slony_cursos".alterTableDropTriggers(v_tab_row.tab_id);
	end loop;

	-- ----
	-- Remove the setsync status. This will also cause the
	-- worker thread to ignore the set and stop replicating
	-- right now.
	-- ----
	delete from "_slony_cursos".sl_setsync
			where ssy_setid = p_sub_set;

	-- ----
	-- Remove all sl_table and sl_sequence entries for this set.
	-- Should we ever subscribe again, the initial data
	-- copy process will create new ones.
	-- ----
	delete from "_slony_cursos".sl_table
			where tab_set = p_sub_set;
	delete from "_slony_cursos".sl_sequence
			where seq_set = p_sub_set;

	-- ----
	-- Call the internal procedure to drop the subscription
	-- ----
	perform "_slony_cursos".unsubscribeSet_int(p_sub_set, p_sub_receiver);

	-- Rewrite sl_listen table
	perform "_slony_cursos".RebuildListenEntries();

	-- ----
	-- Create the UNSUBSCRIBE_SET event
	-- ----
	return  "_slony_cursos".createEvent('_slony_cursos', 'UNSUBSCRIBE_SET', 
			p_sub_set::text, p_sub_receiver::text);
end;
$$;


ALTER FUNCTION _slony_cursos.unsubscribeset(p_sub_set integer, p_sub_receiver integer, p_force boolean) OWNER TO postgres;

--
-- TOC entry 2764 (class 0 OID 0)
-- Dependencies: 340
-- Name: FUNCTION unsubscribeset(p_sub_set integer, p_sub_receiver integer, p_force boolean); Type: COMMENT; Schema: _slony_cursos; Owner: postgres
--

COMMENT ON FUNCTION unsubscribeset(p_sub_set integer, p_sub_receiver integer, p_force boolean) IS 'unsubscribeSet (sub_set, sub_receiver,force) 

Unsubscribe node sub_receiver from subscription set sub_set.  This is
invoked on the receiver node.  It verifies that this does not break
any chains (e.g. - where sub_receiver is a provider for another node),
then restores tables, drops Slony-specific keys, drops table entries
for the set, drops the subscription, and generates an UNSUBSCRIBE_SET
node to publish that the node is being dropped.';


--
-- TOC entry 341 (class 1255 OID 27656)
-- Name: unsubscribeset_int(integer, integer); Type: FUNCTION; Schema: _slony_cursos; Owner: postgres
--

CREATE FUNCTION unsubscribeset_int(p_sub_set integer, p_sub_receiver integer) RETURNS integer
    LANGUAGE plpgsql
    AS $$
declare
begin
	-- ----
	-- Grab the central configuration lock
	-- ----
	lock table "_slony_cursos".sl_config_lock;

	-- ----
	-- All the real work is done before event generation on the
	-- subscriber.
	-- ----

	--if this event unsubscribes the provider of this node
	--then this node should unsubscribe itself from the set as well.
	
	if exists (select true from 
		   "_slony_cursos".sl_subscribe where 
		   sub_set=p_sub_set and sub_provider=p_sub_receiver
		   and sub_receiver="_slony_cursos".getLocalNodeId('_slony_cursos'))
	then
	   perform "_slony_cursos".unsubscribeSet(p_sub_set,"_slony_cursos".getLocalNodeId('_slony_cursos'),true);
	end if;
	

	delete from "_slony_cursos".sl_subscribe
			where sub_set = p_sub_set
				and sub_receiver = p_sub_receiver;

	-- Rewrite sl_listen table
	perform "_slony_cursos".RebuildListenEntries();

	return p_sub_set;
end;
$$;


ALTER FUNCTION _slony_cursos.unsubscribeset_int(p_sub_set integer, p_sub_receiver integer) OWNER TO postgres;

--
-- TOC entry 2765 (class 0 OID 0)
-- Dependencies: 341
-- Name: FUNCTION unsubscribeset_int(p_sub_set integer, p_sub_receiver integer); Type: COMMENT; Schema: _slony_cursos; Owner: postgres
--

COMMENT ON FUNCTION unsubscribeset_int(p_sub_set integer, p_sub_receiver integer) IS 'unsubscribeSet_int (sub_set, sub_receiver)

All the REAL work of removing the subscriber is done before the event
is generated, so this function just has to drop the references to the
subscription in sl_subscribe.';


--
-- TOC entry 350 (class 1255 OID 27665)
-- Name: updaterelname(); Type: FUNCTION; Schema: _slony_cursos; Owner: postgres
--

CREATE FUNCTION updaterelname() RETURNS integer
    LANGUAGE plpgsql
    AS $$
declare
        v_no_id                 int4;
        v_set_origin            int4;
begin
        -- ----
        -- Grab the central configuration lock
        -- ----
        lock table "_slony_cursos".sl_config_lock;

        update "_slony_cursos".sl_table set 
                tab_relname = PGC.relname, tab_nspname = PGN.nspname
                from pg_catalog.pg_class PGC, pg_catalog.pg_namespace PGN 
                where "_slony_cursos".sl_table.tab_reloid = PGC.oid
                        and PGC.relnamespace = PGN.oid and
                    (tab_relname <> PGC.relname or tab_nspname <> PGN.nspname);
        update "_slony_cursos".sl_sequence set
                seq_relname = PGC.relname, seq_nspname = PGN.nspname
                from pg_catalog.pg_class PGC, pg_catalog.pg_namespace PGN
                where "_slony_cursos".sl_sequence.seq_reloid = PGC.oid
                and PGC.relnamespace = PGN.oid and
 		    (seq_relname <> PGC.relname or seq_nspname <> PGN.nspname);
        return 0;
end;
$$;


ALTER FUNCTION _slony_cursos.updaterelname() OWNER TO postgres;

--
-- TOC entry 2766 (class 0 OID 0)
-- Dependencies: 350
-- Name: FUNCTION updaterelname(); Type: COMMENT; Schema: _slony_cursos; Owner: postgres
--

COMMENT ON FUNCTION updaterelname() IS 'updateRelname()';


--
-- TOC entry 351 (class 1255 OID 27666)
-- Name: updatereloid(integer, integer); Type: FUNCTION; Schema: _slony_cursos; Owner: postgres
--

CREATE FUNCTION updatereloid(p_set_id integer, p_only_on_node integer) RETURNS bigint
    LANGUAGE plpgsql
    AS $$
declare
        v_no_id                 int4;
        v_set_origin            int4;
	prec			record;
begin
        -- ----
        -- Check that we either are the set origin or a current
        -- subscriber of the set.
        -- ----
        v_no_id := "_slony_cursos".getLocalNodeId('_slony_cursos');
        select set_origin into v_set_origin
                        from "_slony_cursos".sl_set
                        where set_id = p_set_id
                        for update;
        if not found then
                raise exception 'Slony-I: set % not found', p_set_id;
        end if;
        if v_set_origin <> v_no_id
                and not exists (select 1 from "_slony_cursos".sl_subscribe
                        where sub_set = p_set_id
                        and sub_receiver = v_no_id)
        then
                return 0;
        end if;

        -- ----
        -- If execution on only one node is requested, check that
        -- we are that node.
        -- ----
        if p_only_on_node > 0 and p_only_on_node <> v_no_id then
                return 0;
        end if;

	-- Update OIDs for tables to values pulled from non-table objects in pg_class
	-- This ensures that we won't have collisions when repairing the oids
	for prec in select tab_id from "_slony_cursos".sl_table loop
		update "_slony_cursos".sl_table set tab_reloid = (select oid from pg_class pc where relkind <> 'r' and not exists (select 1 from "_slony_cursos".sl_table t2 where t2.tab_reloid = pc.oid) limit 1)
		where tab_id = prec.tab_id;
	end loop;

	for prec in select tab_id, tab_relname, tab_nspname from "_slony_cursos".sl_table loop
	        update "_slony_cursos".sl_table set
        	        tab_reloid = (select PGC.oid
	                from pg_catalog.pg_class PGC, pg_catalog.pg_namespace PGN
	                where "_slony_cursos".slon_quote_brute(PGC.relname) = "_slony_cursos".slon_quote_brute(prec.tab_relname)
	                        and PGC.relnamespace = PGN.oid
				and "_slony_cursos".slon_quote_brute(PGN.nspname) = "_slony_cursos".slon_quote_brute(prec.tab_nspname))
		where tab_id = prec.tab_id;
	end loop;

	for prec in select seq_id from "_slony_cursos".sl_sequence loop
		update "_slony_cursos".sl_sequence set seq_reloid = (select oid from pg_class pc where relkind <> 'S' and not exists (select 1 from "_slony_cursos".sl_sequence t2 where t2.seq_reloid = pc.oid) limit 1)
		where seq_id = prec.seq_id;
	end loop;

	for prec in select seq_id, seq_relname, seq_nspname from "_slony_cursos".sl_sequence loop
	        update "_slony_cursos".sl_sequence set
	                seq_reloid = (select PGC.oid
	                from pg_catalog.pg_class PGC, pg_catalog.pg_namespace PGN
	                where "_slony_cursos".slon_quote_brute(PGC.relname) = "_slony_cursos".slon_quote_brute(prec.seq_relname)
                	and PGC.relnamespace = PGN.oid
			and "_slony_cursos".slon_quote_brute(PGN.nspname) = "_slony_cursos".slon_quote_brute(prec.seq_nspname))
		where seq_id = prec.seq_id;
	end loop;

	return 1;
end;
$$;


ALTER FUNCTION _slony_cursos.updatereloid(p_set_id integer, p_only_on_node integer) OWNER TO postgres;

--
-- TOC entry 2767 (class 0 OID 0)
-- Dependencies: 351
-- Name: FUNCTION updatereloid(p_set_id integer, p_only_on_node integer); Type: COMMENT; Schema: _slony_cursos; Owner: postgres
--

COMMENT ON FUNCTION updatereloid(p_set_id integer, p_only_on_node integer) IS 'updateReloid(set_id, only_on_node)

Updates the respective reloids in sl_table and sl_seqeunce based on
their respective FQN';


--
-- TOC entry 357 (class 1255 OID 27672)
-- Name: upgradeschema(text); Type: FUNCTION; Schema: _slony_cursos; Owner: postgres
--

CREATE FUNCTION upgradeschema(p_old text) RETURNS text
    LANGUAGE plpgsql
    AS $$
declare
	v_tab_row	record;
	v_query text;
	v_keepstatus text;
begin
	-- If old version is pre-2.0, then we require a special upgrade process
	if p_old like '1.%' then
		raise exception 'Upgrading to Slony-I 2.x requires running slony_upgrade_20';
	end if;

	perform "_slony_cursos".upgradeSchemaAddTruncateTriggers();

	-- Change all Slony-I-defined columns that are "timestamp without time zone" to "timestamp *WITH* time zone"
	if exists (select 1 from information_schema.columns c
            where table_schema = '_slony_cursos' and data_type = 'timestamp without time zone'
	    and exists (select 1 from information_schema.tables t where t.table_schema = c.table_schema and t.table_name = c.table_name and t.table_type = 'BASE TABLE')
		and (c.table_name, c.column_name) in (('sl_confirm', 'con_timestamp'), ('sl_event', 'ev_timestamp'), ('sl_registry', 'reg_timestamp'),('sl_archive_counter', 'ac_timestamp')))
	then

	  -- Preserve sl_status
	  select pg_get_viewdef('"_slony_cursos".sl_status') into v_keepstatus;
	  execute 'drop view sl_status';
	  for v_tab_row in select table_schema, table_name, column_name from information_schema.columns c
            where table_schema = '_slony_cursos' and data_type = 'timestamp without time zone'
	    and exists (select 1 from information_schema.tables t where t.table_schema = c.table_schema and t.table_name = c.table_name and t.table_type = 'BASE TABLE')
		and (table_name, column_name) in (('sl_confirm', 'con_timestamp'), ('sl_event', 'ev_timestamp'), ('sl_registry', 'reg_timestamp'),('sl_archive_counter', 'ac_timestamp'))
	  loop
		raise notice 'Changing Slony-I column [%.%] to timestamp WITH time zone', v_tab_row.table_name, v_tab_row.column_name;
		v_query := 'alter table ' || "_slony_cursos".slon_quote_brute(v_tab_row.table_schema) ||
                   '.' || v_tab_row.table_name || ' alter column ' || v_tab_row.column_name ||
                   ' type timestamp with time zone;';
		execute v_query;
	  end loop;
	  -- restore sl_status
	  execute 'create view sl_status as ' || v_keepstatus;
        end if;

	if not exists (select 1 from information_schema.tables where table_schema = '_slony_cursos' and table_name = 'sl_components') then
	   v_query := '
create table "_slony_cursos".sl_components (
	co_actor	 text not null primary key,
	co_pid		 integer not null,
	co_node		 integer not null,
	co_connection_pid integer not null,
	co_activity	  text,
	co_starttime	  timestamptz not null,
	co_event	  bigint,
	co_eventtype 	  text
) without oids;
';
  	   execute v_query;
	end if;

	



	if not exists (select 1 from information_schema.tables t where table_schema = '_slony_cursos' and table_name = 'sl_event_lock') then
	   v_query := 'create table "_slony_cursos".sl_event_lock (dummy integer);';
	   execute v_query;
        end if;
	
	if not exists (select 1 from information_schema.tables t 
			where table_schema = '_slony_cursos' 
			and table_name = 'sl_apply_stats') then
		v_query := '
			create table "_slony_cursos".sl_apply_stats (
				as_origin			int4,
				as_num_insert		int8,
				as_num_update		int8,
				as_num_delete		int8,
				as_num_truncate		int8,
				as_num_script		int8,
				as_num_total		int8,
				as_duration			interval,
				as_apply_first		timestamptz,
				as_apply_last		timestamptz,
				as_cache_prepare	int8,
				as_cache_hit		int8,
				as_cache_evict		int8,
				as_cache_prepare_max int8
			) WITHOUT OIDS;';
		execute v_query;
	end if;
	
	--
	-- On the upgrade to 2.2, we change the layout of sl_log_N by
	-- adding columns log_tablenspname, log_tablerelname, and
	-- log_cmdupdncols as well as changing log_cmddata into
	-- log_cmdargs, which is a text array.
	--
	if not "_slony_cursos".check_table_field_exists('_slony_cursos', 'sl_log_1', 'log_cmdargs') then
		--
		-- Check that the cluster is completely caught up
		--
		if "_slony_cursos".check_unconfirmed_log() then
			raise EXCEPTION 'cannot upgrade to new sl_log_N format due to existing unreplicated data';
		end if;

		--
		-- Drop tables sl_log_1 and sl_log_2
		--
		drop table "_slony_cursos".sl_log_1;
		drop table "_slony_cursos".sl_log_2;

		--
		-- Create the new sl_log_1
		--
		create table "_slony_cursos".sl_log_1 (
			log_origin          int4,
			log_txid            bigint,
			log_tableid         int4,
			log_actionseq       int8,
			log_tablenspname    text,
			log_tablerelname    text,
			log_cmdtype         "char",
			log_cmdupdncols     int4,
			log_cmdargs         text[]
		) without oids;
		create index sl_log_1_idx1 on "_slony_cursos".sl_log_1
			(log_origin, log_txid, log_actionseq);

		comment on table "_slony_cursos".sl_log_1 is 'Stores each change to be propagated to subscriber nodes';
		comment on column "_slony_cursos".sl_log_1.log_origin is 'Origin node from which the change came';
		comment on column "_slony_cursos".sl_log_1.log_txid is 'Transaction ID on the origin node';
		comment on column "_slony_cursos".sl_log_1.log_tableid is 'The table ID (from sl_table.tab_id) that this log entry is to affect';
		comment on column "_slony_cursos".sl_log_1.log_actionseq is 'The sequence number in which actions will be applied on replicas';
		comment on column "_slony_cursos".sl_log_1.log_tablenspname is 'The schema name of the table affected';
		comment on column "_slony_cursos".sl_log_1.log_tablerelname is 'The table name of the table affected';
		comment on column "_slony_cursos".sl_log_1.log_cmdtype is 'Replication action to take. U = Update, I = Insert, D = DELETE, T = TRUNCATE';
		comment on column "_slony_cursos".sl_log_1.log_cmdupdncols is 'For cmdtype=U the number of updated columns in cmdargs';
		comment on column "_slony_cursos".sl_log_1.log_cmdargs is 'The data needed to perform the log action on the replica';

		--
		-- Create the new sl_log_2
		--
		create table "_slony_cursos".sl_log_2 (
			log_origin          int4,
			log_txid            bigint,
			log_tableid         int4,
			log_actionseq       int8,
			log_tablenspname    text,
			log_tablerelname    text,
			log_cmdtype         "char",
			log_cmdupdncols     int4,
			log_cmdargs         text[]
		) without oids;
		create index sl_log_2_idx1 on "_slony_cursos".sl_log_2
			(log_origin, log_txid, log_actionseq);

		comment on table "_slony_cursos".sl_log_2 is 'Stores each change to be propagated to subscriber nodes';
		comment on column "_slony_cursos".sl_log_2.log_origin is 'Origin node from which the change came';
		comment on column "_slony_cursos".sl_log_2.log_txid is 'Transaction ID on the origin node';
		comment on column "_slony_cursos".sl_log_2.log_tableid is 'The table ID (from sl_table.tab_id) that this log entry is to affect';
		comment on column "_slony_cursos".sl_log_2.log_actionseq is 'The sequence number in which actions will be applied on replicas';
		comment on column "_slony_cursos".sl_log_2.log_tablenspname is 'The schema name of the table affected';
		comment on column "_slony_cursos".sl_log_2.log_tablerelname is 'The table name of the table affected';
		comment on column "_slony_cursos".sl_log_2.log_cmdtype is 'Replication action to take. U = Update, I = Insert, D = DELETE, T = TRUNCATE';
		comment on column "_slony_cursos".sl_log_2.log_cmdupdncols is 'For cmdtype=U the number of updated columns in cmdargs';
		comment on column "_slony_cursos".sl_log_2.log_cmdargs is 'The data needed to perform the log action on the replica';

		create table "_slony_cursos".sl_log_script (
			log_origin			int4,
			log_txid			bigint,
			log_actionseq		int8,
			log_cmdtype			"char",
			log_cmdargs			text[]
			) WITHOUT OIDS;
		create index sl_log_script_idx1 on "_slony_cursos".sl_log_script
		(log_origin, log_txid, log_actionseq);

		comment on table "_slony_cursos".sl_log_script is 'Captures SQL script queries to be propagated to subscriber nodes';
		comment on column "_slony_cursos".sl_log_script.log_origin is 'Origin name from which the change came';
		comment on column "_slony_cursos".sl_log_script.log_txid is 'Transaction ID on the origin node';
		comment on column "_slony_cursos".sl_log_script.log_actionseq is 'The sequence number in which actions will be applied on replicas';
		comment on column "_slony_cursos".sl_log_2.log_cmdtype is 'Replication action to take. S = Script statement, s = Script complete';
		comment on column "_slony_cursos".sl_log_script.log_cmdargs is 'The DDL statement, optionally followed by selected nodes to execute it on.';

		--
		-- Put the log apply triggers back onto sl_log_1/2
		--
		create trigger apply_trigger
			before INSERT on "_slony_cursos".sl_log_1
			for each row execute procedure "_slony_cursos".logApply('_slony_cursos');
		alter table "_slony_cursos".sl_log_1
			enable replica trigger apply_trigger;
		create trigger apply_trigger
			before INSERT on "_slony_cursos".sl_log_2
			for each row execute procedure "_slony_cursos".logApply('_slony_cursos');
		alter table "_slony_cursos".sl_log_2
			enable replica trigger apply_trigger;
	end if;
	if not exists (select 1 from information_schema.routines where routine_schema = '_slony_cursos' and routine_name = 'string_agg') then
	       CREATE AGGREGATE "_slony_cursos".string_agg(text) (
	   	      SFUNC="_slony_cursos".agg_text_sum,
		      STYPE=text,
		      INITCOND=''
		      );
	end if;
	if not exists (select 1 from information_schema.views where table_schema='_slony_cursos' and table_name='sl_failover_targets') then
	   create view "_slony_cursos".sl_failover_targets as
	   	  select  set_id,
		  set_origin as set_origin,
		  sub1.sub_receiver as backup_id

		  FROM
		  "_slony_cursos".sl_subscribe sub1
		  ,"_slony_cursos".sl_set set1
		  where
 		  sub1.sub_set=set_id
		  and sub1.sub_forward=true
		  --exclude candidates where the set_origin
		  --has a path a node but the failover
		  --candidate has no path to that node
		  and sub1.sub_receiver not in
	    	  (select p1.pa_client from
	    	  "_slony_cursos".sl_path p1 
	    	  left outer join "_slony_cursos".sl_path p2 on
	    	  (p2.pa_client=p1.pa_client 
	    	  and p2.pa_server=sub1.sub_receiver)
	    	  where p2.pa_client is null
	    	  and p1.pa_server=set_origin
	    	  and p1.pa_client<>sub1.sub_receiver
	    	  )
		  and sub1.sub_provider=set_origin
		  --exclude any subscribers that are not
		  --direct subscribers of all sets on the
		  --origin
		  and sub1.sub_receiver not in
		  (select direct_recv.sub_receiver
		  from
			
			(--all direct receivers of the first set
			select subs2.sub_receiver
			from "_slony_cursos".sl_subscribe subs2
			where subs2.sub_provider=set1.set_origin
		      	and subs2.sub_set=set1.set_id) as
		      	direct_recv
			inner join
			(--all other sets from the origin
			select set_id from "_slony_cursos".sl_set set2
			where set2.set_origin=set1.set_origin
			and set2.set_id<>sub1.sub_set)
			as othersets on(true)
			left outer join "_slony_cursos".sl_subscribe subs3
			on(subs3.sub_set=othersets.set_id
		   	and subs3.sub_forward=true
		   	and subs3.sub_provider=set1.set_origin
		   	and direct_recv.sub_receiver=subs3.sub_receiver)
	    		where subs3.sub_receiver is null
	    	);
	end if;

	if not "_slony_cursos".check_table_field_exists('_slony_cursos', 'sl_node', 'no_failed') then
	   alter table "_slony_cursos".sl_node add column no_failed bool;
	   update "_slony_cursos".sl_node set no_failed=false;
	end if;
	return p_old;
end;
$$;


ALTER FUNCTION _slony_cursos.upgradeschema(p_old text) OWNER TO postgres;

--
-- TOC entry 2768 (class 0 OID 0)
-- Dependencies: 357
-- Name: FUNCTION upgradeschema(p_old text); Type: COMMENT; Schema: _slony_cursos; Owner: postgres
--

COMMENT ON FUNCTION upgradeschema(p_old text) IS 'Called during "update functions" by slonik to perform schema changes';


--
-- TOC entry 382 (class 1255 OID 27704)
-- Name: upgradeschemaaddtruncatetriggers(); Type: FUNCTION; Schema: _slony_cursos; Owner: postgres
--

CREATE FUNCTION upgradeschemaaddtruncatetriggers() RETURNS integer
    LANGUAGE plpgsql
    AS $$
begin

		--- Add truncate triggers
		begin		
		perform "_slony_cursos".alterTableAddTruncateTrigger("_slony_cursos".slon_quote_brute(tab_nspname) || '.' || "_slony_cursos".slon_quote_brute(tab_relname), tab_id)
				from "_slony_cursos".sl_table
                where 2 <> (select count(*) from pg_catalog.pg_trigger,
					  pg_catalog.pg_class, pg_catalog.pg_namespace where 
					  pg_trigger.tgrelid=pg_class.oid
					  AND pg_class.relnamespace=pg_namespace.oid
					  AND
					  pg_namespace.nspname = tab_nspname and tgname in ('_slony_cursos_truncatedeny', '_slony_cursos_truncatetrigger') and
                      pg_class.relname = tab_relname
					  );

		exception when unique_violation then
				  raise warning 'upgradeSchemaAddTruncateTriggers() - uniqueness violation';
				  raise warning 'likely due to truncate triggers existing partially';
				  raise exception 'upgradeSchemaAddTruncateTriggers() - failure - [%][%]', SQLSTATE, SQLERRM;
		end;

		-- Activate truncate triggers for replica
		perform "_slony_cursos".alterTableConfigureTruncateTrigger("_slony_cursos".slon_quote_brute(tab_nspname) || '.' || "_slony_cursos".slon_quote_brute(tab_relname)
		,'disable','enable') 
		        from "_slony_cursos".sl_table
                where tab_set not in (select set_id from "_slony_cursos".sl_set where set_origin = "_slony_cursos".getLocalNodeId('_slony_cursos'))
                      and exists (select 1 from  pg_catalog.pg_trigger
                                           where pg_trigger.tgname like '_slony_cursos_truncatetrigger' and pg_trigger.tgenabled = 'O'
                                                 and pg_trigger.tgrelid=tab_reloid ) 
                      and  exists (select 1 from  pg_catalog.pg_trigger
                                            where pg_trigger.tgname like '_slony_cursos_truncatedeny' and pg_trigger.tgenabled = 'D' 
                                                  and pg_trigger.tgrelid=tab_reloid);

		-- Activate truncate triggers for origin
		perform "_slony_cursos".alterTableConfigureTruncateTrigger("_slony_cursos".slon_quote_brute(tab_nspname) || '.' || "_slony_cursos".slon_quote_brute(tab_relname)
		,'enable','disable') 
		        from "_slony_cursos".sl_table
                where tab_set in (select set_id from "_slony_cursos".sl_set where set_origin = "_slony_cursos".getLocalNodeId('_slony_cursos'))
                      and exists (select 1 from  pg_catalog.pg_trigger
                                           where pg_trigger.tgname like '_slony_cursos_truncatetrigger' and pg_trigger.tgenabled = 'D'
                                                 and pg_trigger.tgrelid=tab_reloid )                                                    
                      and  exists (select 1 from  pg_catalog.pg_trigger
                                            where pg_trigger.tgname like '_slony_cursos_truncatedeny' and pg_trigger.tgenabled = 'O'
                                                  and pg_trigger.tgrelid=tab_reloid);

		return 1;
end
$$;


ALTER FUNCTION _slony_cursos.upgradeschemaaddtruncatetriggers() OWNER TO postgres;

--
-- TOC entry 2769 (class 0 OID 0)
-- Dependencies: 382
-- Name: FUNCTION upgradeschemaaddtruncatetriggers(); Type: COMMENT; Schema: _slony_cursos; Owner: postgres
--

COMMENT ON FUNCTION upgradeschemaaddtruncatetriggers() IS 'Add ON TRUNCATE triggers to replicated tables.';


SET search_path = public, pg_catalog;

--
-- TOC entry 234 (class 1255 OID 27000)
-- Name: validar_aprobacion_de_area(); Type: FUNCTION; Schema: public; Owner: postgres
--

CREATE FUNCTION validar_aprobacion_de_area() RETURNS trigger
    LANGUAGE plpgsql
    AS $$
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

END; $$;


ALTER FUNCTION public.validar_aprobacion_de_area() OWNER TO postgres;

--
-- TOC entry 235 (class 1255 OID 27001)
-- Name: validar_instrcutor_participante(); Type: FUNCTION; Schema: public; Owner: postgres
--

CREATE FUNCTION validar_instrcutor_participante() RETURNS trigger
    LANGUAGE plpgsql
    AS $$
BEGIN
	IF EXISTS ( SELECT 1
				FROM participantes AS p
				WHERE p.ficha = NEW.instructor 
				AND p.lapso = NEW.lapso 
				AND p.cod_curso = NEW.cod_curso) THEN
		RAISE EXCEPTION 'No se pudo insertar al instructor porque el es partipante del curso';
	END IF;

	RETURN NEW;
END; $$;


ALTER FUNCTION public.validar_instrcutor_participante() OWNER TO postgres;

--
-- TOC entry 236 (class 1255 OID 27002)
-- Name: validar_nivel_prelacion(); Type: FUNCTION; Schema: public; Owner: postgres
--

CREATE FUNCTION validar_nivel_prelacion() RETURNS trigger
    LANGUAGE plpgsql
    AS $$
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
END; $$;


ALTER FUNCTION public.validar_nivel_prelacion() OWNER TO postgres;

--
-- TOC entry 237 (class 1255 OID 27003)
-- Name: validar_participante_instrcutor(); Type: FUNCTION; Schema: public; Owner: postgres
--

CREATE FUNCTION validar_participante_instrcutor() RETURNS trigger
    LANGUAGE plpgsql
    AS $$
BEGIN
	IF EXISTS ( SELECT 1
				FROM ediciones AS ed
				WHERE ed.instructor = NEW.ficha 
				AND ed.lapso = NEW.lapso 
				AND ed.cod_curso = NEW.cod_curso) THEN
		RAISE EXCEPTION 'No se pudo insertar al partipante porque es él instructor';
	END IF;

	RETURN NEW;
END; $$;


ALTER FUNCTION public.validar_participante_instrcutor() OWNER TO postgres;

--
-- TOC entry 238 (class 1255 OID 27004)
-- Name: validar_participantes_fragmento_sede1(); Type: FUNCTION; Schema: public; Owner: postgres
--

CREATE FUNCTION validar_participantes_fragmento_sede1() RETURNS trigger
    LANGUAGE plpgsql
    AS $$
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
END; $$;


ALTER FUNCTION public.validar_participantes_fragmento_sede1() OWNER TO postgres;

--
-- TOC entry 239 (class 1255 OID 27005)
-- Name: validar_participantes_fragmento_sede2(); Type: FUNCTION; Schema: public; Owner: postgres
--

CREATE FUNCTION validar_participantes_fragmento_sede2() RETURNS trigger
    LANGUAGE plpgsql
    AS $$
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
END; $$;


ALTER FUNCTION public.validar_participantes_fragmento_sede2() OWNER TO postgres;

--
-- TOC entry 240 (class 1255 OID 27006)
-- Name: validar_participantes_fragmento_sede3(); Type: FUNCTION; Schema: public; Owner: postgres
--

CREATE FUNCTION validar_participantes_fragmento_sede3() RETURNS trigger
    LANGUAGE plpgsql
    AS $$
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
END; $$;


ALTER FUNCTION public.validar_participantes_fragmento_sede3() OWNER TO postgres;

--
-- TOC entry 241 (class 1255 OID 27007)
-- Name: validar_participantes_sede(); Type: FUNCTION; Schema: public; Owner: postgres
--

CREATE FUNCTION validar_participantes_sede() RETURNS trigger
    LANGUAGE plpgsql
    AS $$
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
	
END; $$;


ALTER FUNCTION public.validar_participantes_sede() OWNER TO postgres;

--
-- TOC entry 242 (class 1255 OID 27008)
-- Name: validar_prelaciones(); Type: FUNCTION; Schema: public; Owner: postgres
--

CREATE FUNCTION validar_prelaciones() RETURNS trigger
    LANGUAGE plpgsql
    AS $$
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
END; $$;


ALTER FUNCTION public.validar_prelaciones() OWNER TO postgres;

--
-- TOC entry 243 (class 1255 OID 27009)
-- Name: validar_repeticion_curso_aprobado(); Type: FUNCTION; Schema: public; Owner: postgres
--

CREATE FUNCTION validar_repeticion_curso_aprobado() RETURNS trigger
    LANGUAGE plpgsql
    AS $$
BEGIN 
		
	IF EXISTS (SELECT * 
				FROM participantes AS p 
				WHERE p.cod_curso = NEW.cod_curso AND 
					  p.nota = 'A' AND
					  p.ficha = NEW.ficha ) THEN
			
		RAISE EXCEPTION 'No se ha podido insertar al partipante porque ya aprobó este curso';
	END IF;

	RETURN NEW;
END; $$;


ALTER FUNCTION public.validar_repeticion_curso_aprobado() OWNER TO postgres;

SET search_path = _slony_cursos, pg_catalog;

--
-- TOC entry 884 (class 1255 OID 27553)
-- Name: string_agg(text); Type: AGGREGATE; Schema: _slony_cursos; Owner: postgres
--

CREATE AGGREGATE string_agg(text) (
    SFUNC = agg_text_sum,
    STYPE = text,
    INITCOND = ''
);


ALTER AGGREGATE _slony_cursos.string_agg(text) OWNER TO postgres;

--
-- TOC entry 211 (class 1259 OID 27531)
-- Name: sl_action_seq; Type: SEQUENCE; Schema: _slony_cursos; Owner: postgres
--

CREATE SEQUENCE sl_action_seq
    START WITH 1
    INCREMENT BY 1
    NO MINVALUE
    NO MAXVALUE
    CACHE 1;


ALTER TABLE sl_action_seq OWNER TO postgres;

--
-- TOC entry 2770 (class 0 OID 0)
-- Dependencies: 211
-- Name: SEQUENCE sl_action_seq; Type: COMMENT; Schema: _slony_cursos; Owner: postgres
--

COMMENT ON SEQUENCE sl_action_seq IS 'The sequence to number statements in the transaction logs, so that the replication engines can figure out the "agreeable" order of statements.';


SET default_tablespace = '';

SET default_with_oids = false;

--
-- TOC entry 206 (class 1259 OID 27514)
-- Name: sl_apply_stats; Type: TABLE; Schema: _slony_cursos; Owner: postgres; Tablespace: 
--

CREATE TABLE sl_apply_stats (
    as_origin integer,
    as_num_insert bigint,
    as_num_update bigint,
    as_num_delete bigint,
    as_num_truncate bigint,
    as_num_script bigint,
    as_num_total bigint,
    as_duration interval,
    as_apply_first timestamp with time zone,
    as_apply_last timestamp with time zone,
    as_cache_prepare bigint,
    as_cache_hit bigint,
    as_cache_evict bigint,
    as_cache_prepare_max bigint
);


ALTER TABLE sl_apply_stats OWNER TO postgres;

--
-- TOC entry 2771 (class 0 OID 0)
-- Dependencies: 206
-- Name: TABLE sl_apply_stats; Type: COMMENT; Schema: _slony_cursos; Owner: postgres
--

COMMENT ON TABLE sl_apply_stats IS 'Local SYNC apply statistics (running totals)';


--
-- TOC entry 2772 (class 0 OID 0)
-- Dependencies: 206
-- Name: COLUMN sl_apply_stats.as_origin; Type: COMMENT; Schema: _slony_cursos; Owner: postgres
--

COMMENT ON COLUMN sl_apply_stats.as_origin IS 'Origin of the SYNCs';


--
-- TOC entry 2773 (class 0 OID 0)
-- Dependencies: 206
-- Name: COLUMN sl_apply_stats.as_num_insert; Type: COMMENT; Schema: _slony_cursos; Owner: postgres
--

COMMENT ON COLUMN sl_apply_stats.as_num_insert IS 'Number of INSERT operations performed';


--
-- TOC entry 2774 (class 0 OID 0)
-- Dependencies: 206
-- Name: COLUMN sl_apply_stats.as_num_update; Type: COMMENT; Schema: _slony_cursos; Owner: postgres
--

COMMENT ON COLUMN sl_apply_stats.as_num_update IS 'Number of UPDATE operations performed';


--
-- TOC entry 2775 (class 0 OID 0)
-- Dependencies: 206
-- Name: COLUMN sl_apply_stats.as_num_delete; Type: COMMENT; Schema: _slony_cursos; Owner: postgres
--

COMMENT ON COLUMN sl_apply_stats.as_num_delete IS 'Number of DELETE operations performed';


--
-- TOC entry 2776 (class 0 OID 0)
-- Dependencies: 206
-- Name: COLUMN sl_apply_stats.as_num_truncate; Type: COMMENT; Schema: _slony_cursos; Owner: postgres
--

COMMENT ON COLUMN sl_apply_stats.as_num_truncate IS 'Number of TRUNCATE operations performed';


--
-- TOC entry 2777 (class 0 OID 0)
-- Dependencies: 206
-- Name: COLUMN sl_apply_stats.as_num_script; Type: COMMENT; Schema: _slony_cursos; Owner: postgres
--

COMMENT ON COLUMN sl_apply_stats.as_num_script IS 'Number of DDL operations performed';


--
-- TOC entry 2778 (class 0 OID 0)
-- Dependencies: 206
-- Name: COLUMN sl_apply_stats.as_num_total; Type: COMMENT; Schema: _slony_cursos; Owner: postgres
--

COMMENT ON COLUMN sl_apply_stats.as_num_total IS 'Total number of operations';


--
-- TOC entry 2779 (class 0 OID 0)
-- Dependencies: 206
-- Name: COLUMN sl_apply_stats.as_duration; Type: COMMENT; Schema: _slony_cursos; Owner: postgres
--

COMMENT ON COLUMN sl_apply_stats.as_duration IS 'Processing time';


--
-- TOC entry 2780 (class 0 OID 0)
-- Dependencies: 206
-- Name: COLUMN sl_apply_stats.as_apply_first; Type: COMMENT; Schema: _slony_cursos; Owner: postgres
--

COMMENT ON COLUMN sl_apply_stats.as_apply_first IS 'Timestamp of first recorded SYNC';


--
-- TOC entry 2781 (class 0 OID 0)
-- Dependencies: 206
-- Name: COLUMN sl_apply_stats.as_apply_last; Type: COMMENT; Schema: _slony_cursos; Owner: postgres
--

COMMENT ON COLUMN sl_apply_stats.as_apply_last IS 'Timestamp of most recent recorded SYNC';


--
-- TOC entry 2782 (class 0 OID 0)
-- Dependencies: 206
-- Name: COLUMN sl_apply_stats.as_cache_evict; Type: COMMENT; Schema: _slony_cursos; Owner: postgres
--

COMMENT ON COLUMN sl_apply_stats.as_cache_evict IS 'Number of apply query cache evict operations';


--
-- TOC entry 2783 (class 0 OID 0)
-- Dependencies: 206
-- Name: COLUMN sl_apply_stats.as_cache_prepare_max; Type: COMMENT; Schema: _slony_cursos; Owner: postgres
--

COMMENT ON COLUMN sl_apply_stats.as_cache_prepare_max IS 'Maximum number of apply queries prepared in one SYNC group';


--
-- TOC entry 215 (class 1259 OID 27541)
-- Name: sl_archive_counter; Type: TABLE; Schema: _slony_cursos; Owner: postgres; Tablespace: 
--

CREATE TABLE sl_archive_counter (
    ac_num bigint,
    ac_timestamp timestamp with time zone
);


ALTER TABLE sl_archive_counter OWNER TO postgres;

--
-- TOC entry 2784 (class 0 OID 0)
-- Dependencies: 215
-- Name: TABLE sl_archive_counter; Type: COMMENT; Schema: _slony_cursos; Owner: postgres
--

COMMENT ON TABLE sl_archive_counter IS 'Table used to generate the log shipping archive number.
';


--
-- TOC entry 2785 (class 0 OID 0)
-- Dependencies: 215
-- Name: COLUMN sl_archive_counter.ac_num; Type: COMMENT; Schema: _slony_cursos; Owner: postgres
--

COMMENT ON COLUMN sl_archive_counter.ac_num IS 'Counter of SYNC ID used in log shipping as the archive number';


--
-- TOC entry 2786 (class 0 OID 0)
-- Dependencies: 215
-- Name: COLUMN sl_archive_counter.ac_timestamp; Type: COMMENT; Schema: _slony_cursos; Owner: postgres
--

COMMENT ON COLUMN sl_archive_counter.ac_timestamp IS 'Time at which the archive log was generated on the subscriber';


--
-- TOC entry 216 (class 1259 OID 27544)
-- Name: sl_components; Type: TABLE; Schema: _slony_cursos; Owner: postgres; Tablespace: 
--

CREATE TABLE sl_components (
    co_actor text NOT NULL,
    co_pid integer NOT NULL,
    co_node integer NOT NULL,
    co_connection_pid integer NOT NULL,
    co_activity text,
    co_starttime timestamp with time zone NOT NULL,
    co_event bigint,
    co_eventtype text
);


ALTER TABLE sl_components OWNER TO postgres;

--
-- TOC entry 2787 (class 0 OID 0)
-- Dependencies: 216
-- Name: TABLE sl_components; Type: COMMENT; Schema: _slony_cursos; Owner: postgres
--

COMMENT ON TABLE sl_components IS 'Table used to monitor what various slon/slonik components are doing';


--
-- TOC entry 2788 (class 0 OID 0)
-- Dependencies: 216
-- Name: COLUMN sl_components.co_actor; Type: COMMENT; Schema: _slony_cursos; Owner: postgres
--

COMMENT ON COLUMN sl_components.co_actor IS 'which component am I?';


--
-- TOC entry 2789 (class 0 OID 0)
-- Dependencies: 216
-- Name: COLUMN sl_components.co_pid; Type: COMMENT; Schema: _slony_cursos; Owner: postgres
--

COMMENT ON COLUMN sl_components.co_pid IS 'my process/thread PID on node where slon runs';


--
-- TOC entry 2790 (class 0 OID 0)
-- Dependencies: 216
-- Name: COLUMN sl_components.co_node; Type: COMMENT; Schema: _slony_cursos; Owner: postgres
--

COMMENT ON COLUMN sl_components.co_node IS 'which node am I servicing?';


--
-- TOC entry 2791 (class 0 OID 0)
-- Dependencies: 216
-- Name: COLUMN sl_components.co_connection_pid; Type: COMMENT; Schema: _slony_cursos; Owner: postgres
--

COMMENT ON COLUMN sl_components.co_connection_pid IS 'PID of database connection being used on database server';


--
-- TOC entry 2792 (class 0 OID 0)
-- Dependencies: 216
-- Name: COLUMN sl_components.co_activity; Type: COMMENT; Schema: _slony_cursos; Owner: postgres
--

COMMENT ON COLUMN sl_components.co_activity IS 'activity that I am up to';


--
-- TOC entry 2793 (class 0 OID 0)
-- Dependencies: 216
-- Name: COLUMN sl_components.co_starttime; Type: COMMENT; Schema: _slony_cursos; Owner: postgres
--

COMMENT ON COLUMN sl_components.co_starttime IS 'when did my activity begin?  (timestamp reported as per slon process on server running slon)';


--
-- TOC entry 2794 (class 0 OID 0)
-- Dependencies: 216
-- Name: COLUMN sl_components.co_event; Type: COMMENT; Schema: _slony_cursos; Owner: postgres
--

COMMENT ON COLUMN sl_components.co_event IS 'which event have I started processing?';


--
-- TOC entry 2795 (class 0 OID 0)
-- Dependencies: 216
-- Name: COLUMN sl_components.co_eventtype; Type: COMMENT; Schema: _slony_cursos; Owner: postgres
--

COMMENT ON COLUMN sl_components.co_eventtype IS 'what kind of event am I processing?  (commonly n/a for event loop main threads)';


--
-- TOC entry 213 (class 1259 OID 27535)
-- Name: sl_config_lock; Type: TABLE; Schema: _slony_cursos; Owner: postgres; Tablespace: 
--

CREATE TABLE sl_config_lock (
    dummy integer
);


ALTER TABLE sl_config_lock OWNER TO postgres;

--
-- TOC entry 2796 (class 0 OID 0)
-- Dependencies: 213
-- Name: TABLE sl_config_lock; Type: COMMENT; Schema: _slony_cursos; Owner: postgres
--

COMMENT ON TABLE sl_config_lock IS 'This table exists solely to prevent overlapping execution of configuration change procedures and the resulting possible deadlocks.
';


--
-- TOC entry 2797 (class 0 OID 0)
-- Dependencies: 213
-- Name: COLUMN sl_config_lock.dummy; Type: COMMENT; Schema: _slony_cursos; Owner: postgres
--

COMMENT ON COLUMN sl_config_lock.dummy IS 'No data ever goes in this table so the contents never matter.  Indeed, this column does not really need to exist.';


--
-- TOC entry 200 (class 1259 OID 27474)
-- Name: sl_confirm; Type: TABLE; Schema: _slony_cursos; Owner: postgres; Tablespace: 
--

CREATE TABLE sl_confirm (
    con_origin integer,
    con_received integer,
    con_seqno bigint,
    con_timestamp timestamp with time zone DEFAULT (timeofday())::timestamp with time zone
);


ALTER TABLE sl_confirm OWNER TO postgres;

--
-- TOC entry 2798 (class 0 OID 0)
-- Dependencies: 200
-- Name: TABLE sl_confirm; Type: COMMENT; Schema: _slony_cursos; Owner: postgres
--

COMMENT ON TABLE sl_confirm IS 'Holds confirmation of replication events.  After a period of time, Slony removes old confirmed events from both this table and the sl_event table.';


--
-- TOC entry 2799 (class 0 OID 0)
-- Dependencies: 200
-- Name: COLUMN sl_confirm.con_origin; Type: COMMENT; Schema: _slony_cursos; Owner: postgres
--

COMMENT ON COLUMN sl_confirm.con_origin IS 'The ID # (from sl_node.no_id) of the source node for this event';


--
-- TOC entry 2800 (class 0 OID 0)
-- Dependencies: 200
-- Name: COLUMN sl_confirm.con_seqno; Type: COMMENT; Schema: _slony_cursos; Owner: postgres
--

COMMENT ON COLUMN sl_confirm.con_seqno IS 'The ID # for the event';


--
-- TOC entry 2801 (class 0 OID 0)
-- Dependencies: 200
-- Name: COLUMN sl_confirm.con_timestamp; Type: COMMENT; Schema: _slony_cursos; Owner: postgres
--

COMMENT ON COLUMN sl_confirm.con_timestamp IS 'When this event was confirmed';


--
-- TOC entry 199 (class 1259 OID 27466)
-- Name: sl_event; Type: TABLE; Schema: _slony_cursos; Owner: postgres; Tablespace: 
--

CREATE TABLE sl_event (
    ev_origin integer NOT NULL,
    ev_seqno bigint NOT NULL,
    ev_timestamp timestamp with time zone,
    ev_snapshot txid_snapshot,
    ev_type text,
    ev_data1 text,
    ev_data2 text,
    ev_data3 text,
    ev_data4 text,
    ev_data5 text,
    ev_data6 text,
    ev_data7 text,
    ev_data8 text
);


ALTER TABLE sl_event OWNER TO postgres;

--
-- TOC entry 2802 (class 0 OID 0)
-- Dependencies: 199
-- Name: TABLE sl_event; Type: COMMENT; Schema: _slony_cursos; Owner: postgres
--

COMMENT ON TABLE sl_event IS 'Holds information about replication events.  After a period of time, Slony removes old confirmed events from both this table and the sl_confirm table.';


--
-- TOC entry 2803 (class 0 OID 0)
-- Dependencies: 199
-- Name: COLUMN sl_event.ev_origin; Type: COMMENT; Schema: _slony_cursos; Owner: postgres
--

COMMENT ON COLUMN sl_event.ev_origin IS 'The ID # (from sl_node.no_id) of the source node for this event';


--
-- TOC entry 2804 (class 0 OID 0)
-- Dependencies: 199
-- Name: COLUMN sl_event.ev_seqno; Type: COMMENT; Schema: _slony_cursos; Owner: postgres
--

COMMENT ON COLUMN sl_event.ev_seqno IS 'The ID # for the event';


--
-- TOC entry 2805 (class 0 OID 0)
-- Dependencies: 199
-- Name: COLUMN sl_event.ev_timestamp; Type: COMMENT; Schema: _slony_cursos; Owner: postgres
--

COMMENT ON COLUMN sl_event.ev_timestamp IS 'When this event record was created';


--
-- TOC entry 2806 (class 0 OID 0)
-- Dependencies: 199
-- Name: COLUMN sl_event.ev_snapshot; Type: COMMENT; Schema: _slony_cursos; Owner: postgres
--

COMMENT ON COLUMN sl_event.ev_snapshot IS 'TXID snapshot on provider node for this event';


--
-- TOC entry 2807 (class 0 OID 0)
-- Dependencies: 199
-- Name: COLUMN sl_event.ev_type; Type: COMMENT; Schema: _slony_cursos; Owner: postgres
--

COMMENT ON COLUMN sl_event.ev_type IS 'The type of event this record is for.  
				SYNC				= Synchronise
				STORE_NODE			=
				ENABLE_NODE			=
				DROP_NODE			=
				STORE_PATH			=
				DROP_PATH			=
				STORE_LISTEN		=
				DROP_LISTEN			=
				STORE_SET			=
				DROP_SET			=
				MERGE_SET			=
				SET_ADD_TABLE		=
				SET_ADD_SEQUENCE	=
				STORE_TRIGGER		=
				DROP_TRIGGER		=
				MOVE_SET			=
				ACCEPT_SET			=
				SET_DROP_TABLE			=
				SET_DROP_SEQUENCE		=
				SET_MOVE_TABLE			=
				SET_MOVE_SEQUENCE		=
				FAILOVER_SET		=
				SUBSCRIBE_SET		=
				ENABLE_SUBSCRIPTION	=
				UNSUBSCRIBE_SET		=
				DDL_SCRIPT			=
				ADJUST_SEQ			=
				RESET_CONFIG		=
';


--
-- TOC entry 2808 (class 0 OID 0)
-- Dependencies: 199
-- Name: COLUMN sl_event.ev_data1; Type: COMMENT; Schema: _slony_cursos; Owner: postgres
--

COMMENT ON COLUMN sl_event.ev_data1 IS 'Data field containing an argument needed to process the event';


--
-- TOC entry 2809 (class 0 OID 0)
-- Dependencies: 199
-- Name: COLUMN sl_event.ev_data2; Type: COMMENT; Schema: _slony_cursos; Owner: postgres
--

COMMENT ON COLUMN sl_event.ev_data2 IS 'Data field containing an argument needed to process the event';


--
-- TOC entry 2810 (class 0 OID 0)
-- Dependencies: 199
-- Name: COLUMN sl_event.ev_data3; Type: COMMENT; Schema: _slony_cursos; Owner: postgres
--

COMMENT ON COLUMN sl_event.ev_data3 IS 'Data field containing an argument needed to process the event';


--
-- TOC entry 2811 (class 0 OID 0)
-- Dependencies: 199
-- Name: COLUMN sl_event.ev_data4; Type: COMMENT; Schema: _slony_cursos; Owner: postgres
--

COMMENT ON COLUMN sl_event.ev_data4 IS 'Data field containing an argument needed to process the event';


--
-- TOC entry 2812 (class 0 OID 0)
-- Dependencies: 199
-- Name: COLUMN sl_event.ev_data5; Type: COMMENT; Schema: _slony_cursos; Owner: postgres
--

COMMENT ON COLUMN sl_event.ev_data5 IS 'Data field containing an argument needed to process the event';


--
-- TOC entry 2813 (class 0 OID 0)
-- Dependencies: 199
-- Name: COLUMN sl_event.ev_data6; Type: COMMENT; Schema: _slony_cursos; Owner: postgres
--

COMMENT ON COLUMN sl_event.ev_data6 IS 'Data field containing an argument needed to process the event';


--
-- TOC entry 2814 (class 0 OID 0)
-- Dependencies: 199
-- Name: COLUMN sl_event.ev_data7; Type: COMMENT; Schema: _slony_cursos; Owner: postgres
--

COMMENT ON COLUMN sl_event.ev_data7 IS 'Data field containing an argument needed to process the event';


--
-- TOC entry 2815 (class 0 OID 0)
-- Dependencies: 199
-- Name: COLUMN sl_event.ev_data8; Type: COMMENT; Schema: _slony_cursos; Owner: postgres
--

COMMENT ON COLUMN sl_event.ev_data8 IS 'Data field containing an argument needed to process the event';


--
-- TOC entry 214 (class 1259 OID 27538)
-- Name: sl_event_lock; Type: TABLE; Schema: _slony_cursos; Owner: postgres; Tablespace: 
--

CREATE TABLE sl_event_lock (
    dummy integer
);


ALTER TABLE sl_event_lock OWNER TO postgres;

--
-- TOC entry 2816 (class 0 OID 0)
-- Dependencies: 214
-- Name: TABLE sl_event_lock; Type: COMMENT; Schema: _slony_cursos; Owner: postgres
--

COMMENT ON TABLE sl_event_lock IS 'This table exists solely to prevent multiple connections from concurrently creating new events and perhaps getting them out of order.';


--
-- TOC entry 2817 (class 0 OID 0)
-- Dependencies: 214
-- Name: COLUMN sl_event_lock.dummy; Type: COMMENT; Schema: _slony_cursos; Owner: postgres
--

COMMENT ON COLUMN sl_event_lock.dummy IS 'No data ever goes in this table so the contents never matter.  Indeed, this column does not really need to exist.';


--
-- TOC entry 210 (class 1259 OID 27529)
-- Name: sl_event_seq; Type: SEQUENCE; Schema: _slony_cursos; Owner: postgres
--

CREATE SEQUENCE sl_event_seq
    START WITH 1
    INCREMENT BY 1
    NO MINVALUE
    NO MAXVALUE
    CACHE 1;


ALTER TABLE sl_event_seq OWNER TO postgres;

--
-- TOC entry 2818 (class 0 OID 0)
-- Dependencies: 210
-- Name: SEQUENCE sl_event_seq; Type: COMMENT; Schema: _slony_cursos; Owner: postgres
--

COMMENT ON SEQUENCE sl_event_seq IS 'The sequence for numbering events originating from this node.';


--
-- TOC entry 196 (class 1259 OID 27418)
-- Name: sl_path; Type: TABLE; Schema: _slony_cursos; Owner: postgres; Tablespace: 
--

CREATE TABLE sl_path (
    pa_server integer NOT NULL,
    pa_client integer NOT NULL,
    pa_conninfo text NOT NULL,
    pa_connretry integer
);


ALTER TABLE sl_path OWNER TO postgres;

--
-- TOC entry 2819 (class 0 OID 0)
-- Dependencies: 196
-- Name: TABLE sl_path; Type: COMMENT; Schema: _slony_cursos; Owner: postgres
--

COMMENT ON TABLE sl_path IS 'Holds connection information for the paths between nodes, and the synchronisation delay';


--
-- TOC entry 2820 (class 0 OID 0)
-- Dependencies: 196
-- Name: COLUMN sl_path.pa_server; Type: COMMENT; Schema: _slony_cursos; Owner: postgres
--

COMMENT ON COLUMN sl_path.pa_server IS 'The Node ID # (from sl_node.no_id) of the data source';


--
-- TOC entry 2821 (class 0 OID 0)
-- Dependencies: 196
-- Name: COLUMN sl_path.pa_client; Type: COMMENT; Schema: _slony_cursos; Owner: postgres
--

COMMENT ON COLUMN sl_path.pa_client IS 'The Node ID # (from sl_node.no_id) of the data target';


--
-- TOC entry 2822 (class 0 OID 0)
-- Dependencies: 196
-- Name: COLUMN sl_path.pa_conninfo; Type: COMMENT; Schema: _slony_cursos; Owner: postgres
--

COMMENT ON COLUMN sl_path.pa_conninfo IS 'The PostgreSQL connection string used to connect to the source node.';


--
-- TOC entry 2823 (class 0 OID 0)
-- Dependencies: 196
-- Name: COLUMN sl_path.pa_connretry; Type: COMMENT; Schema: _slony_cursos; Owner: postgres
--

COMMENT ON COLUMN sl_path.pa_connretry IS 'The synchronisation delay, in seconds';


--
-- TOC entry 192 (class 1259 OID 27357)
-- Name: sl_set; Type: TABLE; Schema: _slony_cursos; Owner: postgres; Tablespace: 
--

CREATE TABLE sl_set (
    set_id integer NOT NULL,
    set_origin integer,
    set_locked bigint,
    set_comment text
);


ALTER TABLE sl_set OWNER TO postgres;

--
-- TOC entry 2824 (class 0 OID 0)
-- Dependencies: 192
-- Name: TABLE sl_set; Type: COMMENT; Schema: _slony_cursos; Owner: postgres
--

COMMENT ON TABLE sl_set IS 'Holds definitions of replication sets.';


--
-- TOC entry 2825 (class 0 OID 0)
-- Dependencies: 192
-- Name: COLUMN sl_set.set_id; Type: COMMENT; Schema: _slony_cursos; Owner: postgres
--

COMMENT ON COLUMN sl_set.set_id IS 'A unique ID number for the set.';


--
-- TOC entry 2826 (class 0 OID 0)
-- Dependencies: 192
-- Name: COLUMN sl_set.set_origin; Type: COMMENT; Schema: _slony_cursos; Owner: postgres
--

COMMENT ON COLUMN sl_set.set_origin IS 'The ID number of the source node for the replication set.';


--
-- TOC entry 2827 (class 0 OID 0)
-- Dependencies: 192
-- Name: COLUMN sl_set.set_locked; Type: COMMENT; Schema: _slony_cursos; Owner: postgres
--

COMMENT ON COLUMN sl_set.set_locked IS 'Transaction ID where the set was locked.';


--
-- TOC entry 2828 (class 0 OID 0)
-- Dependencies: 192
-- Name: COLUMN sl_set.set_comment; Type: COMMENT; Schema: _slony_cursos; Owner: postgres
--

COMMENT ON COLUMN sl_set.set_comment IS 'A human-oriented description of the set.';


--
-- TOC entry 198 (class 1259 OID 27451)
-- Name: sl_subscribe; Type: TABLE; Schema: _slony_cursos; Owner: postgres; Tablespace: 
--

CREATE TABLE sl_subscribe (
    sub_set integer NOT NULL,
    sub_provider integer,
    sub_receiver integer NOT NULL,
    sub_forward boolean,
    sub_active boolean
);


ALTER TABLE sl_subscribe OWNER TO postgres;

--
-- TOC entry 2829 (class 0 OID 0)
-- Dependencies: 198
-- Name: TABLE sl_subscribe; Type: COMMENT; Schema: _slony_cursos; Owner: postgres
--

COMMENT ON TABLE sl_subscribe IS 'Holds a list of subscriptions on sets';


--
-- TOC entry 2830 (class 0 OID 0)
-- Dependencies: 198
-- Name: COLUMN sl_subscribe.sub_set; Type: COMMENT; Schema: _slony_cursos; Owner: postgres
--

COMMENT ON COLUMN sl_subscribe.sub_set IS 'ID # (from sl_set) of the set being subscribed to';


--
-- TOC entry 2831 (class 0 OID 0)
-- Dependencies: 198
-- Name: COLUMN sl_subscribe.sub_provider; Type: COMMENT; Schema: _slony_cursos; Owner: postgres
--

COMMENT ON COLUMN sl_subscribe.sub_provider IS 'ID# (from sl_node) of the node providing data';


--
-- TOC entry 2832 (class 0 OID 0)
-- Dependencies: 198
-- Name: COLUMN sl_subscribe.sub_receiver; Type: COMMENT; Schema: _slony_cursos; Owner: postgres
--

COMMENT ON COLUMN sl_subscribe.sub_receiver IS 'ID# (from sl_node) of the node receiving data from the provider';


--
-- TOC entry 2833 (class 0 OID 0)
-- Dependencies: 198
-- Name: COLUMN sl_subscribe.sub_forward; Type: COMMENT; Schema: _slony_cursos; Owner: postgres
--

COMMENT ON COLUMN sl_subscribe.sub_forward IS 'Does this provider keep data in sl_log_1/sl_log_2 to allow it to be a provider for other nodes?';


--
-- TOC entry 2834 (class 0 OID 0)
-- Dependencies: 198
-- Name: COLUMN sl_subscribe.sub_active; Type: COMMENT; Schema: _slony_cursos; Owner: postgres
--

COMMENT ON COLUMN sl_subscribe.sub_active IS 'Has this subscription been activated?  This is not set on the subscriber until AFTER the subscriber has received COPY data from the provider';


--
-- TOC entry 208 (class 1259 OID 27522)
-- Name: sl_failover_targets; Type: VIEW; Schema: _slony_cursos; Owner: postgres
--

CREATE VIEW sl_failover_targets AS
 SELECT set1.set_id,
    set1.set_origin,
    sub1.sub_receiver AS backup_id
   FROM sl_subscribe sub1,
    sl_set set1
  WHERE (((((sub1.sub_set = set1.set_id) AND (sub1.sub_forward = true)) AND (NOT (sub1.sub_receiver IN ( SELECT p1.pa_client
           FROM (sl_path p1
             LEFT JOIN sl_path p2 ON (((p2.pa_client = p1.pa_client) AND (p2.pa_server = sub1.sub_receiver))))
          WHERE (((p2.pa_client IS NULL) AND (p1.pa_server = set1.set_origin)) AND (p1.pa_client <> sub1.sub_receiver)))))) AND (sub1.sub_provider = set1.set_origin)) AND (NOT (sub1.sub_receiver IN ( SELECT direct_recv.sub_receiver
           FROM ((( SELECT subs2.sub_receiver
                   FROM sl_subscribe subs2
                  WHERE ((subs2.sub_provider = set1.set_origin) AND (subs2.sub_set = set1.set_id))) direct_recv
             JOIN ( SELECT set2.set_id
                   FROM sl_set set2
                  WHERE ((set2.set_origin = set1.set_origin) AND (set2.set_id <> sub1.sub_set))) othersets ON (true))
             LEFT JOIN sl_subscribe subs3 ON (((((subs3.sub_set = othersets.set_id) AND (subs3.sub_forward = true)) AND (subs3.sub_provider = set1.set_origin)) AND (direct_recv.sub_receiver = subs3.sub_receiver))))
          WHERE (subs3.sub_receiver IS NULL)))));


ALTER TABLE sl_failover_targets OWNER TO postgres;

--
-- TOC entry 197 (class 1259 OID 27436)
-- Name: sl_listen; Type: TABLE; Schema: _slony_cursos; Owner: postgres; Tablespace: 
--

CREATE TABLE sl_listen (
    li_origin integer NOT NULL,
    li_provider integer NOT NULL,
    li_receiver integer NOT NULL
);


ALTER TABLE sl_listen OWNER TO postgres;

--
-- TOC entry 2835 (class 0 OID 0)
-- Dependencies: 197
-- Name: TABLE sl_listen; Type: COMMENT; Schema: _slony_cursos; Owner: postgres
--

COMMENT ON TABLE sl_listen IS 'Indicates how nodes listen to events from other nodes in the Slony-I network.';


--
-- TOC entry 2836 (class 0 OID 0)
-- Dependencies: 197
-- Name: COLUMN sl_listen.li_origin; Type: COMMENT; Schema: _slony_cursos; Owner: postgres
--

COMMENT ON COLUMN sl_listen.li_origin IS 'The ID # (from sl_node.no_id) of the node this listener is operating on';


--
-- TOC entry 2837 (class 0 OID 0)
-- Dependencies: 197
-- Name: COLUMN sl_listen.li_provider; Type: COMMENT; Schema: _slony_cursos; Owner: postgres
--

COMMENT ON COLUMN sl_listen.li_provider IS 'The ID # (from sl_node.no_id) of the source node for this listening event';


--
-- TOC entry 2838 (class 0 OID 0)
-- Dependencies: 197
-- Name: COLUMN sl_listen.li_receiver; Type: COMMENT; Schema: _slony_cursos; Owner: postgres
--

COMMENT ON COLUMN sl_listen.li_receiver IS 'The ID # (from sl_node.no_id) of the target node for this listening event';


--
-- TOC entry 209 (class 1259 OID 27527)
-- Name: sl_local_node_id; Type: SEQUENCE; Schema: _slony_cursos; Owner: postgres
--

CREATE SEQUENCE sl_local_node_id
    START WITH -1
    INCREMENT BY 1
    MINVALUE -1
    NO MAXVALUE
    CACHE 1;


ALTER TABLE sl_local_node_id OWNER TO postgres;

--
-- TOC entry 2839 (class 0 OID 0)
-- Dependencies: 209
-- Name: SEQUENCE sl_local_node_id; Type: COMMENT; Schema: _slony_cursos; Owner: postgres
--

COMMENT ON SEQUENCE sl_local_node_id IS 'The local node ID is initialized to -1, meaning that this node is not initialized yet.';


--
-- TOC entry 202 (class 1259 OID 27485)
-- Name: sl_log_1; Type: TABLE; Schema: _slony_cursos; Owner: postgres; Tablespace: 
--

CREATE TABLE sl_log_1 (
    log_origin integer,
    log_txid bigint,
    log_tableid integer,
    log_actionseq bigint,
    log_tablenspname text,
    log_tablerelname text,
    log_cmdtype "char",
    log_cmdupdncols integer,
    log_cmdargs text[]
);


ALTER TABLE sl_log_1 OWNER TO postgres;

--
-- TOC entry 2840 (class 0 OID 0)
-- Dependencies: 202
-- Name: TABLE sl_log_1; Type: COMMENT; Schema: _slony_cursos; Owner: postgres
--

COMMENT ON TABLE sl_log_1 IS 'Stores each change to be propagated to subscriber nodes';


--
-- TOC entry 2841 (class 0 OID 0)
-- Dependencies: 202
-- Name: COLUMN sl_log_1.log_origin; Type: COMMENT; Schema: _slony_cursos; Owner: postgres
--

COMMENT ON COLUMN sl_log_1.log_origin IS 'Origin node from which the change came';


--
-- TOC entry 2842 (class 0 OID 0)
-- Dependencies: 202
-- Name: COLUMN sl_log_1.log_txid; Type: COMMENT; Schema: _slony_cursos; Owner: postgres
--

COMMENT ON COLUMN sl_log_1.log_txid IS 'Transaction ID on the origin node';


--
-- TOC entry 2843 (class 0 OID 0)
-- Dependencies: 202
-- Name: COLUMN sl_log_1.log_tableid; Type: COMMENT; Schema: _slony_cursos; Owner: postgres
--

COMMENT ON COLUMN sl_log_1.log_tableid IS 'The table ID (from sl_table.tab_id) that this log entry is to affect';


--
-- TOC entry 2844 (class 0 OID 0)
-- Dependencies: 202
-- Name: COLUMN sl_log_1.log_actionseq; Type: COMMENT; Schema: _slony_cursos; Owner: postgres
--

COMMENT ON COLUMN sl_log_1.log_actionseq IS 'The sequence number in which actions will be applied on replicas';


--
-- TOC entry 2845 (class 0 OID 0)
-- Dependencies: 202
-- Name: COLUMN sl_log_1.log_tablenspname; Type: COMMENT; Schema: _slony_cursos; Owner: postgres
--

COMMENT ON COLUMN sl_log_1.log_tablenspname IS 'The schema name of the table affected';


--
-- TOC entry 2846 (class 0 OID 0)
-- Dependencies: 202
-- Name: COLUMN sl_log_1.log_tablerelname; Type: COMMENT; Schema: _slony_cursos; Owner: postgres
--

COMMENT ON COLUMN sl_log_1.log_tablerelname IS 'The table name of the table affected';


--
-- TOC entry 2847 (class 0 OID 0)
-- Dependencies: 202
-- Name: COLUMN sl_log_1.log_cmdtype; Type: COMMENT; Schema: _slony_cursos; Owner: postgres
--

COMMENT ON COLUMN sl_log_1.log_cmdtype IS 'Replication action to take. U = Update, I = Insert, D = DELETE, T = TRUNCATE';


--
-- TOC entry 2848 (class 0 OID 0)
-- Dependencies: 202
-- Name: COLUMN sl_log_1.log_cmdupdncols; Type: COMMENT; Schema: _slony_cursos; Owner: postgres
--

COMMENT ON COLUMN sl_log_1.log_cmdupdncols IS 'For cmdtype=U the number of updated columns in cmdargs';


--
-- TOC entry 2849 (class 0 OID 0)
-- Dependencies: 202
-- Name: COLUMN sl_log_1.log_cmdargs; Type: COMMENT; Schema: _slony_cursos; Owner: postgres
--

COMMENT ON COLUMN sl_log_1.log_cmdargs IS 'The data needed to perform the log action on the replica';


--
-- TOC entry 203 (class 1259 OID 27492)
-- Name: sl_log_2; Type: TABLE; Schema: _slony_cursos; Owner: postgres; Tablespace: 
--

CREATE TABLE sl_log_2 (
    log_origin integer,
    log_txid bigint,
    log_tableid integer,
    log_actionseq bigint,
    log_tablenspname text,
    log_tablerelname text,
    log_cmdtype "char",
    log_cmdupdncols integer,
    log_cmdargs text[]
);


ALTER TABLE sl_log_2 OWNER TO postgres;

--
-- TOC entry 2850 (class 0 OID 0)
-- Dependencies: 203
-- Name: TABLE sl_log_2; Type: COMMENT; Schema: _slony_cursos; Owner: postgres
--

COMMENT ON TABLE sl_log_2 IS 'Stores each change to be propagated to subscriber nodes';


--
-- TOC entry 2851 (class 0 OID 0)
-- Dependencies: 203
-- Name: COLUMN sl_log_2.log_origin; Type: COMMENT; Schema: _slony_cursos; Owner: postgres
--

COMMENT ON COLUMN sl_log_2.log_origin IS 'Origin node from which the change came';


--
-- TOC entry 2852 (class 0 OID 0)
-- Dependencies: 203
-- Name: COLUMN sl_log_2.log_txid; Type: COMMENT; Schema: _slony_cursos; Owner: postgres
--

COMMENT ON COLUMN sl_log_2.log_txid IS 'Transaction ID on the origin node';


--
-- TOC entry 2853 (class 0 OID 0)
-- Dependencies: 203
-- Name: COLUMN sl_log_2.log_tableid; Type: COMMENT; Schema: _slony_cursos; Owner: postgres
--

COMMENT ON COLUMN sl_log_2.log_tableid IS 'The table ID (from sl_table.tab_id) that this log entry is to affect';


--
-- TOC entry 2854 (class 0 OID 0)
-- Dependencies: 203
-- Name: COLUMN sl_log_2.log_actionseq; Type: COMMENT; Schema: _slony_cursos; Owner: postgres
--

COMMENT ON COLUMN sl_log_2.log_actionseq IS 'The sequence number in which actions will be applied on replicas';


--
-- TOC entry 2855 (class 0 OID 0)
-- Dependencies: 203
-- Name: COLUMN sl_log_2.log_tablenspname; Type: COMMENT; Schema: _slony_cursos; Owner: postgres
--

COMMENT ON COLUMN sl_log_2.log_tablenspname IS 'The schema name of the table affected';


--
-- TOC entry 2856 (class 0 OID 0)
-- Dependencies: 203
-- Name: COLUMN sl_log_2.log_tablerelname; Type: COMMENT; Schema: _slony_cursos; Owner: postgres
--

COMMENT ON COLUMN sl_log_2.log_tablerelname IS 'The table name of the table affected';


--
-- TOC entry 2857 (class 0 OID 0)
-- Dependencies: 203
-- Name: COLUMN sl_log_2.log_cmdtype; Type: COMMENT; Schema: _slony_cursos; Owner: postgres
--

COMMENT ON COLUMN sl_log_2.log_cmdtype IS 'Replication action to take. S = Script statement, s = Script complete';


--
-- TOC entry 2858 (class 0 OID 0)
-- Dependencies: 203
-- Name: COLUMN sl_log_2.log_cmdupdncols; Type: COMMENT; Schema: _slony_cursos; Owner: postgres
--

COMMENT ON COLUMN sl_log_2.log_cmdupdncols IS 'For cmdtype=U the number of updated columns in cmdargs';


--
-- TOC entry 2859 (class 0 OID 0)
-- Dependencies: 203
-- Name: COLUMN sl_log_2.log_cmdargs; Type: COMMENT; Schema: _slony_cursos; Owner: postgres
--

COMMENT ON COLUMN sl_log_2.log_cmdargs IS 'The data needed to perform the log action on the replica';


--
-- TOC entry 204 (class 1259 OID 27499)
-- Name: sl_log_script; Type: TABLE; Schema: _slony_cursos; Owner: postgres; Tablespace: 
--

CREATE TABLE sl_log_script (
    log_origin integer,
    log_txid bigint,
    log_actionseq bigint,
    log_cmdtype "char",
    log_cmdargs text[]
);


ALTER TABLE sl_log_script OWNER TO postgres;

--
-- TOC entry 2860 (class 0 OID 0)
-- Dependencies: 204
-- Name: TABLE sl_log_script; Type: COMMENT; Schema: _slony_cursos; Owner: postgres
--

COMMENT ON TABLE sl_log_script IS 'Captures SQL script queries to be propagated to subscriber nodes';


--
-- TOC entry 2861 (class 0 OID 0)
-- Dependencies: 204
-- Name: COLUMN sl_log_script.log_origin; Type: COMMENT; Schema: _slony_cursos; Owner: postgres
--

COMMENT ON COLUMN sl_log_script.log_origin IS 'Origin name from which the change came';


--
-- TOC entry 2862 (class 0 OID 0)
-- Dependencies: 204
-- Name: COLUMN sl_log_script.log_txid; Type: COMMENT; Schema: _slony_cursos; Owner: postgres
--

COMMENT ON COLUMN sl_log_script.log_txid IS 'Transaction ID on the origin node';


--
-- TOC entry 2863 (class 0 OID 0)
-- Dependencies: 204
-- Name: COLUMN sl_log_script.log_actionseq; Type: COMMENT; Schema: _slony_cursos; Owner: postgres
--

COMMENT ON COLUMN sl_log_script.log_actionseq IS 'The sequence number in which actions will be applied on replicas';


--
-- TOC entry 2864 (class 0 OID 0)
-- Dependencies: 204
-- Name: COLUMN sl_log_script.log_cmdargs; Type: COMMENT; Schema: _slony_cursos; Owner: postgres
--

COMMENT ON COLUMN sl_log_script.log_cmdargs IS 'The DDL statement, optionally followed by selected nodes to execute it on.';


--
-- TOC entry 212 (class 1259 OID 27533)
-- Name: sl_log_status; Type: SEQUENCE; Schema: _slony_cursos; Owner: postgres
--

CREATE SEQUENCE sl_log_status
    START WITH 0
    INCREMENT BY 1
    MINVALUE 0
    MAXVALUE 3
    CACHE 1;


ALTER TABLE sl_log_status OWNER TO postgres;

--
-- TOC entry 2865 (class 0 OID 0)
-- Dependencies: 212
-- Name: SEQUENCE sl_log_status; Type: COMMENT; Schema: _slony_cursos; Owner: postgres
--

COMMENT ON SEQUENCE sl_log_status IS '
Bit 0x01 determines the currently active log table
Bit 0x02 tells if the engine needs to read both logs
after switching until the old log is clean and truncated.

Possible values:
	0		sl_log_1 active, sl_log_2 clean
	1		sl_log_2 active, sl_log_1 clean
	2		sl_log_1 active, sl_log_2 unknown - cleanup
	3		sl_log_2 active, sl_log_1 unknown - cleanup

This is not yet in use.
';


--
-- TOC entry 189 (class 1259 OID 27341)
-- Name: sl_node; Type: TABLE; Schema: _slony_cursos; Owner: postgres; Tablespace: 
--

CREATE TABLE sl_node (
    no_id integer NOT NULL,
    no_active boolean,
    no_comment text,
    no_failed boolean
);


ALTER TABLE sl_node OWNER TO postgres;

--
-- TOC entry 2866 (class 0 OID 0)
-- Dependencies: 189
-- Name: TABLE sl_node; Type: COMMENT; Schema: _slony_cursos; Owner: postgres
--

COMMENT ON TABLE sl_node IS 'Holds the list of nodes associated with this namespace.';


--
-- TOC entry 2867 (class 0 OID 0)
-- Dependencies: 189
-- Name: COLUMN sl_node.no_id; Type: COMMENT; Schema: _slony_cursos; Owner: postgres
--

COMMENT ON COLUMN sl_node.no_id IS 'The unique ID number for the node';


--
-- TOC entry 2868 (class 0 OID 0)
-- Dependencies: 189
-- Name: COLUMN sl_node.no_active; Type: COMMENT; Schema: _slony_cursos; Owner: postgres
--

COMMENT ON COLUMN sl_node.no_active IS 'Is the node active in replication yet?';


--
-- TOC entry 2869 (class 0 OID 0)
-- Dependencies: 189
-- Name: COLUMN sl_node.no_comment; Type: COMMENT; Schema: _slony_cursos; Owner: postgres
--

COMMENT ON COLUMN sl_node.no_comment IS 'A human-oriented description of the node';


--
-- TOC entry 191 (class 1259 OID 27351)
-- Name: sl_nodelock; Type: TABLE; Schema: _slony_cursos; Owner: postgres; Tablespace: 
--

CREATE TABLE sl_nodelock (
    nl_nodeid integer NOT NULL,
    nl_conncnt integer NOT NULL,
    nl_backendpid integer
);


ALTER TABLE sl_nodelock OWNER TO postgres;

--
-- TOC entry 2870 (class 0 OID 0)
-- Dependencies: 191
-- Name: TABLE sl_nodelock; Type: COMMENT; Schema: _slony_cursos; Owner: postgres
--

COMMENT ON TABLE sl_nodelock IS 'Used to prevent multiple slon instances and to identify the backends to kill in terminateNodeConnections().';


--
-- TOC entry 2871 (class 0 OID 0)
-- Dependencies: 191
-- Name: COLUMN sl_nodelock.nl_nodeid; Type: COMMENT; Schema: _slony_cursos; Owner: postgres
--

COMMENT ON COLUMN sl_nodelock.nl_nodeid IS 'Clients node_id';


--
-- TOC entry 2872 (class 0 OID 0)
-- Dependencies: 191
-- Name: COLUMN sl_nodelock.nl_conncnt; Type: COMMENT; Schema: _slony_cursos; Owner: postgres
--

COMMENT ON COLUMN sl_nodelock.nl_conncnt IS 'Clients connection number';


--
-- TOC entry 2873 (class 0 OID 0)
-- Dependencies: 191
-- Name: COLUMN sl_nodelock.nl_backendpid; Type: COMMENT; Schema: _slony_cursos; Owner: postgres
--

COMMENT ON COLUMN sl_nodelock.nl_backendpid IS 'PID of database backend owning this lock';


--
-- TOC entry 190 (class 1259 OID 27349)
-- Name: sl_nodelock_nl_conncnt_seq; Type: SEQUENCE; Schema: _slony_cursos; Owner: postgres
--

CREATE SEQUENCE sl_nodelock_nl_conncnt_seq
    START WITH 1
    INCREMENT BY 1
    NO MINVALUE
    NO MAXVALUE
    CACHE 1;


ALTER TABLE sl_nodelock_nl_conncnt_seq OWNER TO postgres;

--
-- TOC entry 2874 (class 0 OID 0)
-- Dependencies: 190
-- Name: sl_nodelock_nl_conncnt_seq; Type: SEQUENCE OWNED BY; Schema: _slony_cursos; Owner: postgres
--

ALTER SEQUENCE sl_nodelock_nl_conncnt_seq OWNED BY sl_nodelock.nl_conncnt;


--
-- TOC entry 205 (class 1259 OID 27506)
-- Name: sl_registry; Type: TABLE; Schema: _slony_cursos; Owner: postgres; Tablespace: 
--

CREATE TABLE sl_registry (
    reg_key text NOT NULL,
    reg_int4 integer,
    reg_text text,
    reg_timestamp timestamp with time zone
);


ALTER TABLE sl_registry OWNER TO postgres;

--
-- TOC entry 2875 (class 0 OID 0)
-- Dependencies: 205
-- Name: TABLE sl_registry; Type: COMMENT; Schema: _slony_cursos; Owner: postgres
--

COMMENT ON TABLE sl_registry IS 'Stores miscellaneous runtime data';


--
-- TOC entry 2876 (class 0 OID 0)
-- Dependencies: 205
-- Name: COLUMN sl_registry.reg_key; Type: COMMENT; Schema: _slony_cursos; Owner: postgres
--

COMMENT ON COLUMN sl_registry.reg_key IS 'Unique key of the runtime option';


--
-- TOC entry 2877 (class 0 OID 0)
-- Dependencies: 205
-- Name: COLUMN sl_registry.reg_int4; Type: COMMENT; Schema: _slony_cursos; Owner: postgres
--

COMMENT ON COLUMN sl_registry.reg_int4 IS 'Option value if type int4';


--
-- TOC entry 2878 (class 0 OID 0)
-- Dependencies: 205
-- Name: COLUMN sl_registry.reg_text; Type: COMMENT; Schema: _slony_cursos; Owner: postgres
--

COMMENT ON COLUMN sl_registry.reg_text IS 'Option value if type text';


--
-- TOC entry 2879 (class 0 OID 0)
-- Dependencies: 205
-- Name: COLUMN sl_registry.reg_timestamp; Type: COMMENT; Schema: _slony_cursos; Owner: postgres
--

COMMENT ON COLUMN sl_registry.reg_timestamp IS 'Option value if type timestamp';


--
-- TOC entry 195 (class 1259 OID 27403)
-- Name: sl_sequence; Type: TABLE; Schema: _slony_cursos; Owner: postgres; Tablespace: 
--

CREATE TABLE sl_sequence (
    seq_id integer NOT NULL,
    seq_reloid oid NOT NULL,
    seq_relname name NOT NULL,
    seq_nspname name NOT NULL,
    seq_set integer,
    seq_comment text
);


ALTER TABLE sl_sequence OWNER TO postgres;

--
-- TOC entry 2880 (class 0 OID 0)
-- Dependencies: 195
-- Name: TABLE sl_sequence; Type: COMMENT; Schema: _slony_cursos; Owner: postgres
--

COMMENT ON TABLE sl_sequence IS 'Similar to sl_table, each entry identifies a sequence being replicated.';


--
-- TOC entry 2881 (class 0 OID 0)
-- Dependencies: 195
-- Name: COLUMN sl_sequence.seq_id; Type: COMMENT; Schema: _slony_cursos; Owner: postgres
--

COMMENT ON COLUMN sl_sequence.seq_id IS 'An internally-used ID for Slony-I to use in its sequencing of updates';


--
-- TOC entry 2882 (class 0 OID 0)
-- Dependencies: 195
-- Name: COLUMN sl_sequence.seq_reloid; Type: COMMENT; Schema: _slony_cursos; Owner: postgres
--

COMMENT ON COLUMN sl_sequence.seq_reloid IS 'The OID of the sequence object';


--
-- TOC entry 2883 (class 0 OID 0)
-- Dependencies: 195
-- Name: COLUMN sl_sequence.seq_relname; Type: COMMENT; Schema: _slony_cursos; Owner: postgres
--

COMMENT ON COLUMN sl_sequence.seq_relname IS 'The name of the sequence in pg_catalog.pg_class.relname used to recover from a dump/restore cycle';


--
-- TOC entry 2884 (class 0 OID 0)
-- Dependencies: 195
-- Name: COLUMN sl_sequence.seq_nspname; Type: COMMENT; Schema: _slony_cursos; Owner: postgres
--

COMMENT ON COLUMN sl_sequence.seq_nspname IS 'The name of the schema in pg_catalog.pg_namespace.nspname used to recover from a dump/restore cycle';


--
-- TOC entry 2885 (class 0 OID 0)
-- Dependencies: 195
-- Name: COLUMN sl_sequence.seq_set; Type: COMMENT; Schema: _slony_cursos; Owner: postgres
--

COMMENT ON COLUMN sl_sequence.seq_set IS 'Indicates which replication set the object is in';


--
-- TOC entry 2886 (class 0 OID 0)
-- Dependencies: 195
-- Name: COLUMN sl_sequence.seq_comment; Type: COMMENT; Schema: _slony_cursos; Owner: postgres
--

COMMENT ON COLUMN sl_sequence.seq_comment IS 'A human-oriented comment';


--
-- TOC entry 207 (class 1259 OID 27518)
-- Name: sl_seqlastvalue; Type: VIEW; Schema: _slony_cursos; Owner: postgres
--

CREATE VIEW sl_seqlastvalue AS
 SELECT sq.seq_id,
    sq.seq_set,
    sq.seq_reloid,
    s.set_origin AS seq_origin,
    sequencelastvalue(((quote_ident((pgn.nspname)::text) || '.'::text) || quote_ident((pgc.relname)::text))) AS seq_last_value
   FROM sl_sequence sq,
    sl_set s,
    pg_class pgc,
    pg_namespace pgn
  WHERE (((s.set_id = sq.seq_set) AND (pgc.oid = sq.seq_reloid)) AND (pgn.oid = pgc.relnamespace));


ALTER TABLE sl_seqlastvalue OWNER TO postgres;

--
-- TOC entry 201 (class 1259 OID 27480)
-- Name: sl_seqlog; Type: TABLE; Schema: _slony_cursos; Owner: postgres; Tablespace: 
--

CREATE TABLE sl_seqlog (
    seql_seqid integer,
    seql_origin integer,
    seql_ev_seqno bigint,
    seql_last_value bigint
);


ALTER TABLE sl_seqlog OWNER TO postgres;

--
-- TOC entry 2887 (class 0 OID 0)
-- Dependencies: 201
-- Name: TABLE sl_seqlog; Type: COMMENT; Schema: _slony_cursos; Owner: postgres
--

COMMENT ON TABLE sl_seqlog IS 'Log of Sequence updates';


--
-- TOC entry 2888 (class 0 OID 0)
-- Dependencies: 201
-- Name: COLUMN sl_seqlog.seql_seqid; Type: COMMENT; Schema: _slony_cursos; Owner: postgres
--

COMMENT ON COLUMN sl_seqlog.seql_seqid IS 'Sequence ID';


--
-- TOC entry 2889 (class 0 OID 0)
-- Dependencies: 201
-- Name: COLUMN sl_seqlog.seql_origin; Type: COMMENT; Schema: _slony_cursos; Owner: postgres
--

COMMENT ON COLUMN sl_seqlog.seql_origin IS 'Publisher node at which the sequence originates';


--
-- TOC entry 2890 (class 0 OID 0)
-- Dependencies: 201
-- Name: COLUMN sl_seqlog.seql_ev_seqno; Type: COMMENT; Schema: _slony_cursos; Owner: postgres
--

COMMENT ON COLUMN sl_seqlog.seql_ev_seqno IS 'Slony-I Event with which this sequence update is associated';


--
-- TOC entry 2891 (class 0 OID 0)
-- Dependencies: 201
-- Name: COLUMN sl_seqlog.seql_last_value; Type: COMMENT; Schema: _slony_cursos; Owner: postgres
--

COMMENT ON COLUMN sl_seqlog.seql_last_value IS 'Last value published for this sequence';


--
-- TOC entry 193 (class 1259 OID 27370)
-- Name: sl_setsync; Type: TABLE; Schema: _slony_cursos; Owner: postgres; Tablespace: 
--

CREATE TABLE sl_setsync (
    ssy_setid integer NOT NULL,
    ssy_origin integer,
    ssy_seqno bigint,
    ssy_snapshot txid_snapshot,
    ssy_action_list text
);


ALTER TABLE sl_setsync OWNER TO postgres;

--
-- TOC entry 2892 (class 0 OID 0)
-- Dependencies: 193
-- Name: TABLE sl_setsync; Type: COMMENT; Schema: _slony_cursos; Owner: postgres
--

COMMENT ON TABLE sl_setsync IS 'SYNC information';


--
-- TOC entry 2893 (class 0 OID 0)
-- Dependencies: 193
-- Name: COLUMN sl_setsync.ssy_setid; Type: COMMENT; Schema: _slony_cursos; Owner: postgres
--

COMMENT ON COLUMN sl_setsync.ssy_setid IS 'ID number of the replication set';


--
-- TOC entry 2894 (class 0 OID 0)
-- Dependencies: 193
-- Name: COLUMN sl_setsync.ssy_origin; Type: COMMENT; Schema: _slony_cursos; Owner: postgres
--

COMMENT ON COLUMN sl_setsync.ssy_origin IS 'ID number of the node';


--
-- TOC entry 2895 (class 0 OID 0)
-- Dependencies: 193
-- Name: COLUMN sl_setsync.ssy_seqno; Type: COMMENT; Schema: _slony_cursos; Owner: postgres
--

COMMENT ON COLUMN sl_setsync.ssy_seqno IS 'Slony-I sequence number';


--
-- TOC entry 2896 (class 0 OID 0)
-- Dependencies: 193
-- Name: COLUMN sl_setsync.ssy_snapshot; Type: COMMENT; Schema: _slony_cursos; Owner: postgres
--

COMMENT ON COLUMN sl_setsync.ssy_snapshot IS 'TXID in provider system seen by the event';


--
-- TOC entry 2897 (class 0 OID 0)
-- Dependencies: 193
-- Name: COLUMN sl_setsync.ssy_action_list; Type: COMMENT; Schema: _slony_cursos; Owner: postgres
--

COMMENT ON COLUMN sl_setsync.ssy_action_list IS 'action list used during the subscription process. At the time a subscriber copies over data from the origin, it sees all tables in a state somewhere between two SYNC events. Therefore this list must contains all log_actionseqs that are visible at that time, whose operations have therefore already been included in the data copied at the time the initial data copy is done.  Those actions may therefore be filtered out of the first SYNC done after subscribing.';


--
-- TOC entry 218 (class 1259 OID 27675)
-- Name: sl_status; Type: VIEW; Schema: _slony_cursos; Owner: postgres
--

CREATE VIEW sl_status AS
 SELECT e.ev_origin AS st_origin,
    c.con_received AS st_received,
    e.ev_seqno AS st_last_event,
    e.ev_timestamp AS st_last_event_ts,
    c.con_seqno AS st_last_received,
    c.con_timestamp AS st_last_received_ts,
    ce.ev_timestamp AS st_last_received_event_ts,
    (e.ev_seqno - c.con_seqno) AS st_lag_num_events,
    (now() - ce.ev_timestamp) AS st_lag_time
   FROM sl_event e,
    sl_confirm c,
    sl_event ce
  WHERE (((((e.ev_origin = c.con_origin) AND (ce.ev_origin = e.ev_origin)) AND (ce.ev_seqno = c.con_seqno)) AND ((e.ev_origin, e.ev_seqno) IN ( SELECT sl_event.ev_origin,
            max(sl_event.ev_seqno) AS max
           FROM sl_event
          WHERE (sl_event.ev_origin = getlocalnodeid('_slony_cursos'::name))
          GROUP BY sl_event.ev_origin))) AND ((c.con_origin, c.con_received, c.con_seqno) IN ( SELECT sl_confirm.con_origin,
            sl_confirm.con_received,
            max(sl_confirm.con_seqno) AS max
           FROM sl_confirm
          WHERE (sl_confirm.con_origin = getlocalnodeid('_slony_cursos'::name))
          GROUP BY sl_confirm.con_origin, sl_confirm.con_received)));


ALTER TABLE sl_status OWNER TO postgres;

--
-- TOC entry 2898 (class 0 OID 0)
-- Dependencies: 218
-- Name: VIEW sl_status; Type: COMMENT; Schema: _slony_cursos; Owner: postgres
--

COMMENT ON VIEW sl_status IS 'View showing how far behind remote nodes are.';


--
-- TOC entry 194 (class 1259 OID 27388)
-- Name: sl_table; Type: TABLE; Schema: _slony_cursos; Owner: postgres; Tablespace: 
--

CREATE TABLE sl_table (
    tab_id integer NOT NULL,
    tab_reloid oid NOT NULL,
    tab_relname name NOT NULL,
    tab_nspname name NOT NULL,
    tab_set integer,
    tab_idxname name NOT NULL,
    tab_altered boolean NOT NULL,
    tab_comment text
);


ALTER TABLE sl_table OWNER TO postgres;

--
-- TOC entry 2899 (class 0 OID 0)
-- Dependencies: 194
-- Name: TABLE sl_table; Type: COMMENT; Schema: _slony_cursos; Owner: postgres
--

COMMENT ON TABLE sl_table IS 'Holds information about the tables being replicated.';


--
-- TOC entry 2900 (class 0 OID 0)
-- Dependencies: 194
-- Name: COLUMN sl_table.tab_id; Type: COMMENT; Schema: _slony_cursos; Owner: postgres
--

COMMENT ON COLUMN sl_table.tab_id IS 'Unique key for Slony-I to use to identify the table';


--
-- TOC entry 2901 (class 0 OID 0)
-- Dependencies: 194
-- Name: COLUMN sl_table.tab_reloid; Type: COMMENT; Schema: _slony_cursos; Owner: postgres
--

COMMENT ON COLUMN sl_table.tab_reloid IS 'The OID of the table in pg_catalog.pg_class.oid';


--
-- TOC entry 2902 (class 0 OID 0)
-- Dependencies: 194
-- Name: COLUMN sl_table.tab_relname; Type: COMMENT; Schema: _slony_cursos; Owner: postgres
--

COMMENT ON COLUMN sl_table.tab_relname IS 'The name of the table in pg_catalog.pg_class.relname used to recover from a dump/restore cycle';


--
-- TOC entry 2903 (class 0 OID 0)
-- Dependencies: 194
-- Name: COLUMN sl_table.tab_nspname; Type: COMMENT; Schema: _slony_cursos; Owner: postgres
--

COMMENT ON COLUMN sl_table.tab_nspname IS 'The name of the schema in pg_catalog.pg_namespace.nspname used to recover from a dump/restore cycle';


--
-- TOC entry 2904 (class 0 OID 0)
-- Dependencies: 194
-- Name: COLUMN sl_table.tab_set; Type: COMMENT; Schema: _slony_cursos; Owner: postgres
--

COMMENT ON COLUMN sl_table.tab_set IS 'ID of the replication set the table is in';


--
-- TOC entry 2905 (class 0 OID 0)
-- Dependencies: 194
-- Name: COLUMN sl_table.tab_idxname; Type: COMMENT; Schema: _slony_cursos; Owner: postgres
--

COMMENT ON COLUMN sl_table.tab_idxname IS 'The name of the primary index of the table';


--
-- TOC entry 2906 (class 0 OID 0)
-- Dependencies: 194
-- Name: COLUMN sl_table.tab_altered; Type: COMMENT; Schema: _slony_cursos; Owner: postgres
--

COMMENT ON COLUMN sl_table.tab_altered IS 'Has the table been modified for replication?';


--
-- TOC entry 2907 (class 0 OID 0)
-- Dependencies: 194
-- Name: COLUMN sl_table.tab_comment; Type: COMMENT; Schema: _slony_cursos; Owner: postgres
--

COMMENT ON COLUMN sl_table.tab_comment IS 'Human-oriented description of the table';


SET search_path = public, pg_catalog;

--
-- TOC entry 173 (class 1259 OID 27010)
-- Name: cursos; Type: TABLE; Schema: public; Owner: grupo_admin; Tablespace: 
--

CREATE TABLE cursos (
    cod_curso integer NOT NULL,
    des_curso character varying(120),
    area area_curso,
    nivel integer,
    CONSTRAINT cursos_nivel_check CHECK (((nivel >= 1) AND (nivel <= 3)))
);


ALTER TABLE cursos OWNER TO grupo_admin;

--
-- TOC entry 174 (class 1259 OID 27017)
-- Name: departamentos; Type: TABLE; Schema: public; Owner: grupo_admin; Tablespace: 
--

CREATE TABLE departamentos (
    cod_dpto character(6) NOT NULL,
    des_dpto character varying(120)
);


ALTER TABLE departamentos OWNER TO grupo_admin;

--
-- TOC entry 175 (class 1259 OID 27020)
-- Name: ediciones; Type: TABLE; Schema: public; Owner: grupo_admin; Tablespace: 
--

CREATE TABLE ediciones (
    cod_curso integer NOT NULL,
    lapso character(7) NOT NULL,
    status status_edicion,
    instructor character(5),
    cod_sede character(3) NOT NULL
);


ALTER TABLE ediciones OWNER TO grupo_admin;

--
-- TOC entry 176 (class 1259 OID 27026)
-- Name: ediciones_sede1; Type: TABLE; Schema: public; Owner: grupo_admin; Tablespace: 
--

CREATE TABLE ediciones_sede1 (
    cod_curso integer NOT NULL,
    lapso character(7) NOT NULL,
    status status_edicion,
    instructor character(5),
    cod_sede character(3) NOT NULL
);


ALTER TABLE ediciones_sede1 OWNER TO grupo_admin;

--
-- TOC entry 177 (class 1259 OID 27032)
-- Name: ediciones_sede2; Type: TABLE; Schema: public; Owner: grupo_admin; Tablespace: 
--

CREATE TABLE ediciones_sede2 (
    cod_curso integer NOT NULL,
    lapso character(7) NOT NULL,
    status status_edicion,
    instructor character(5),
    cod_sede character(3) NOT NULL
);


ALTER TABLE ediciones_sede2 OWNER TO grupo_admin;

--
-- TOC entry 178 (class 1259 OID 27038)
-- Name: ediciones_sede3; Type: TABLE; Schema: public; Owner: grupo_admin; Tablespace: 
--

CREATE TABLE ediciones_sede3 (
    cod_curso integer NOT NULL,
    lapso character(7) NOT NULL,
    status status_edicion,
    instructor character(5),
    cod_sede character(3) NOT NULL
);


ALTER TABLE ediciones_sede3 OWNER TO grupo_admin;

--
-- TOC entry 179 (class 1259 OID 27044)
-- Name: empleados; Type: TABLE; Schema: public; Owner: grupo_admin; Tablespace: 
--

CREATE TABLE empleados (
    ficha character(5) NOT NULL,
    nombre character varying(80),
    cargo character varying(80),
    sueldo numeric(12,2),
    fecha_ingreso date,
    status status_empleado,
    cod_dpto character(6),
    cod_sede character(3),
    CONSTRAINT empleados_sueldo_check CHECK ((sueldo >= (0)::numeric))
);


ALTER TABLE empleados OWNER TO grupo_admin;

--
-- TOC entry 180 (class 1259 OID 27051)
-- Name: empleados_sede1; Type: TABLE; Schema: public; Owner: grupo_admin; Tablespace: 
--

CREATE TABLE empleados_sede1 (
    ficha character(5) NOT NULL,
    nombre character varying(80),
    cargo character varying(80),
    sueldo numeric(12,2),
    fecha_ingreso date,
    status status_empleado,
    cod_dpto character(6),
    cod_sede character(3),
    CONSTRAINT empleados_sede1_sueldo_check CHECK ((sueldo >= (0)::numeric))
);


ALTER TABLE empleados_sede1 OWNER TO grupo_admin;

--
-- TOC entry 181 (class 1259 OID 27058)
-- Name: empleados_sede2; Type: TABLE; Schema: public; Owner: grupo_admin; Tablespace: 
--

CREATE TABLE empleados_sede2 (
    ficha character(5) NOT NULL,
    nombre character varying(80),
    cargo character varying(80),
    sueldo numeric(12,2),
    fecha_ingreso date,
    status status_empleado,
    cod_dpto character(6),
    cod_sede character(3),
    CONSTRAINT empleados_sede1_sueldo_check CHECK ((sueldo >= (0)::numeric))
);


ALTER TABLE empleados_sede2 OWNER TO grupo_admin;

--
-- TOC entry 182 (class 1259 OID 27065)
-- Name: empleados_sede3; Type: TABLE; Schema: public; Owner: grupo_admin; Tablespace: 
--

CREATE TABLE empleados_sede3 (
    ficha character(5) NOT NULL,
    nombre character varying(80),
    cargo character varying(80),
    sueldo numeric(12,2),
    fecha_ingreso date,
    status status_empleado,
    cod_dpto character(6),
    cod_sede character(3),
    CONSTRAINT empleados_sede3_sueldo_check CHECK ((sueldo >= (0)::numeric))
);


ALTER TABLE empleados_sede3 OWNER TO grupo_admin;

--
-- TOC entry 183 (class 1259 OID 27072)
-- Name: participantes; Type: TABLE; Schema: public; Owner: grupo_admin; Tablespace: 
--

CREATE TABLE participantes (
    cod_curso integer NOT NULL,
    lapso character(7) NOT NULL,
    nota nota_participante,
    ficha character(5) NOT NULL
);


ALTER TABLE participantes OWNER TO grupo_admin;

--
-- TOC entry 184 (class 1259 OID 27078)
-- Name: participantes_sede1; Type: TABLE; Schema: public; Owner: grupo_admin; Tablespace: 
--

CREATE TABLE participantes_sede1 (
    cod_curso integer NOT NULL,
    lapso character(7) NOT NULL,
    nota nota_participante,
    ficha character(5) NOT NULL
);


ALTER TABLE participantes_sede1 OWNER TO grupo_admin;

--
-- TOC entry 185 (class 1259 OID 27084)
-- Name: participantes_sede2; Type: TABLE; Schema: public; Owner: grupo_admin; Tablespace: 
--

CREATE TABLE participantes_sede2 (
    cod_curso integer NOT NULL,
    lapso character(7) NOT NULL,
    nota nota_participante,
    ficha character(5) NOT NULL
);


ALTER TABLE participantes_sede2 OWNER TO grupo_admin;

--
-- TOC entry 186 (class 1259 OID 27090)
-- Name: participantes_sede3; Type: TABLE; Schema: public; Owner: grupo_admin; Tablespace: 
--

CREATE TABLE participantes_sede3 (
    cod_curso integer NOT NULL,
    lapso character(7) NOT NULL,
    nota nota_participante,
    ficha character(5) NOT NULL
);


ALTER TABLE participantes_sede3 OWNER TO grupo_admin;

--
-- TOC entry 187 (class 1259 OID 27096)
-- Name: prelaciones; Type: TABLE; Schema: public; Owner: grupo_admin; Tablespace: 
--

CREATE TABLE prelaciones (
    cod_curso integer NOT NULL,
    cod_prelacion integer NOT NULL
);


ALTER TABLE prelaciones OWNER TO grupo_admin;

--
-- TOC entry 188 (class 1259 OID 27099)
-- Name: sedes; Type: TABLE; Schema: public; Owner: grupo_admin; Tablespace: 
--

CREATE TABLE sedes (
    cod_sede character(3) NOT NULL,
    des_sede character varying(80)
);


ALTER TABLE sedes OWNER TO grupo_admin;

SET search_path = _slony_cursos, pg_catalog;

--
-- TOC entry 2240 (class 2604 OID 27354)
-- Name: nl_conncnt; Type: DEFAULT; Schema: _slony_cursos; Owner: postgres
--

ALTER TABLE ONLY sl_nodelock ALTER COLUMN nl_conncnt SET DEFAULT nextval('sl_nodelock_nl_conncnt_seq'::regclass);


--
-- TOC entry 2924 (class 0 OID 0)
-- Dependencies: 211
-- Name: sl_action_seq; Type: SEQUENCE SET; Schema: _slony_cursos; Owner: postgres
--

SELECT pg_catalog.setval('sl_action_seq', 7, true);


--
-- TOC entry 2616 (class 0 OID 27514)
-- Dependencies: 206
-- Data for Name: sl_apply_stats; Type: TABLE DATA; Schema: _slony_cursos; Owner: postgres
--

COPY sl_apply_stats (as_origin, as_num_insert, as_num_update, as_num_delete, as_num_truncate, as_num_script, as_num_total, as_duration, as_apply_first, as_apply_last, as_cache_prepare, as_cache_hit, as_cache_evict, as_cache_prepare_max) FROM stdin;
\.


--
-- TOC entry 2623 (class 0 OID 27541)
-- Dependencies: 215
-- Data for Name: sl_archive_counter; Type: TABLE DATA; Schema: _slony_cursos; Owner: postgres
--

COPY sl_archive_counter (ac_num, ac_timestamp) FROM stdin;
0	1969-12-31 20:00:00-04
\.


--
-- TOC entry 2624 (class 0 OID 27544)
-- Dependencies: 216
-- Data for Name: sl_components; Type: TABLE DATA; Schema: _slony_cursos; Owner: postgres
--

COPY sl_components (co_actor, co_pid, co_node, co_connection_pid, co_activity, co_starttime, co_event, co_eventtype) FROM stdin;
\.


--
-- TOC entry 2621 (class 0 OID 27535)
-- Dependencies: 213
-- Data for Name: sl_config_lock; Type: TABLE DATA; Schema: _slony_cursos; Owner: postgres
--

COPY sl_config_lock (dummy) FROM stdin;
\.


--
-- TOC entry 2610 (class 0 OID 27474)
-- Dependencies: 200
-- Data for Name: sl_confirm; Type: TABLE DATA; Schema: _slony_cursos; Owner: postgres
--

COPY sl_confirm (con_origin, con_received, con_seqno, con_timestamp) FROM stdin;
1	2	0	2015-06-19 15:27:06.452-04:30
2	1	0	2015-06-19 15:27:06.461-04:30
\.


--
-- TOC entry 2609 (class 0 OID 27466)
-- Dependencies: 199
-- Data for Name: sl_event; Type: TABLE DATA; Schema: _slony_cursos; Owner: postgres
--

COPY sl_event (ev_origin, ev_seqno, ev_timestamp, ev_snapshot, ev_type, ev_data1, ev_data2, ev_data3, ev_data4, ev_data5, ev_data6, ev_data7, ev_data8) FROM stdin;
1	5000000001	2015-06-19 15:27:02.569-04:30	3348:3348:	ENABLE_NODE	1	\N	\N	\N	\N	\N	\N	\N
1	5000000002	2015-06-19 15:27:04.984-04:30	3349:3349:	STORE_SET	1	1	tablas de cursos	\N	\N	\N	\N	\N
1	5000000003	2015-06-19 15:27:04.988-04:30	3350:3350:	SYNC	\N	\N	\N	\N	\N	\N	\N	\N
1	5000000004	2015-06-19 15:27:04.994-04:30	3351:3351:	SET_ADD_TABLE	1	1	public.empleados	empleados_pkey	tabla empleados	\N	\N	\N
1	5000000005	2015-06-19 15:27:05.035-04:30	3352:3352:	SYNC	\N	\N	\N	\N	\N	\N	\N	\N
1	5000000006	2015-06-19 15:27:05.036-04:30	3353:3353:	SET_ADD_TABLE	1	2	public.empleados_sede1	empleados_sede1_pkey	tabla empleados sede 1	\N	\N	\N
1	5000000007	2015-06-19 15:27:05.063-04:30	3354:3354:	SYNC	\N	\N	\N	\N	\N	\N	\N	\N
1	5000000008	2015-06-19 15:27:05.064-04:30	3355:3355:	SET_ADD_TABLE	1	3	public.empleados_sede2	empleados_sede2_pkey	tabla empleados sede 2	\N	\N	\N
1	5000000009	2015-06-19 15:27:05.09-04:30	3356:3356:	SYNC	\N	\N	\N	\N	\N	\N	\N	\N
1	5000000010	2015-06-19 15:27:05.091-04:30	3357:3357:	SET_ADD_TABLE	1	4	public.empleados_sede3	empleados_sede3_pkey	tabla empleados sede 3	\N	\N	\N
1	5000000011	2015-06-19 15:27:05.117-04:30	3358:3358:	SYNC	\N	\N	\N	\N	\N	\N	\N	\N
1	5000000012	2015-06-19 15:27:05.119-04:30	3359:3359:	SET_ADD_TABLE	1	5	public.ediciones	ediciones_pkey	tabla ediciones	\N	\N	\N
1	5000000013	2015-06-19 15:27:05.145-04:30	3360:3360:	SYNC	\N	\N	\N	\N	\N	\N	\N	\N
1	5000000014	2015-06-19 15:27:05.146-04:30	3361:3361:	SET_ADD_TABLE	1	6	public.ediciones_sede1	ediciones_sede1_pkey	tabla ediciones sede 1	\N	\N	\N
1	5000000015	2015-06-19 15:27:05.173-04:30	3362:3362:	SYNC	\N	\N	\N	\N	\N	\N	\N	\N
1	5000000016	2015-06-19 15:27:05.174-04:30	3363:3363:	SET_ADD_TABLE	1	7	public.ediciones_sede2	ediciones_sede2_pkey	tabla ediciones sede 2	\N	\N	\N
1	5000000017	2015-06-19 15:27:05.195-04:30	3364:3364:	SYNC	\N	\N	\N	\N	\N	\N	\N	\N
1	5000000018	2015-06-19 15:27:05.196-04:30	3365:3365:	SET_ADD_TABLE	1	8	public.ediciones_sede3	ediciones_sede3_pkey	tabla ediciones sede 3	\N	\N	\N
1	5000000019	2015-06-19 15:27:05.216-04:30	3366:3366:	SYNC	\N	\N	\N	\N	\N	\N	\N	\N
1	5000000020	2015-06-19 15:27:05.217-04:30	3367:3367:	SET_ADD_TABLE	1	9	public.participantes	participantes_pkey	tabla participantes	\N	\N	\N
1	5000000021	2015-06-19 15:27:05.237-04:30	3368:3368:	SYNC	\N	\N	\N	\N	\N	\N	\N	\N
1	5000000022	2015-06-19 15:27:05.238-04:30	3369:3369:	SET_ADD_TABLE	1	10	public.participantes_sede1	participantes_sede1_pkey	tabla participantes sede 1	\N	\N	\N
1	5000000023	2015-06-19 15:27:05.258-04:30	3370:3370:	SYNC	\N	\N	\N	\N	\N	\N	\N	\N
1	5000000024	2015-06-19 15:27:05.259-04:30	3371:3371:	SET_ADD_TABLE	1	11	public.participantes_sede2	participantes_sede2_pkey	tabla participantes sede 2	\N	\N	\N
1	5000000025	2015-06-19 15:27:05.279-04:30	3372:3372:	SYNC	\N	\N	\N	\N	\N	\N	\N	\N
1	5000000026	2015-06-19 15:27:05.28-04:30	3373:3373:	SET_ADD_TABLE	1	12	public.participantes_sede3	participantes_sede3_pkey	tabla participantes sede 3	\N	\N	\N
1	5000000027	2015-06-19 15:27:05.301-04:30	3374:3374:	SYNC	\N	\N	\N	\N	\N	\N	\N	\N
1	5000000028	2015-06-19 15:27:05.302-04:30	3375:3375:	SET_ADD_TABLE	1	13	public.cursos	cursos_pkey	tabla cursos	\N	\N	\N
1	5000000029	2015-06-19 15:27:05.321-04:30	3376:3376:	SYNC	\N	\N	\N	\N	\N	\N	\N	\N
1	5000000030	2015-06-19 15:27:05.322-04:30	3377:3377:	SET_ADD_TABLE	1	14	public.sedes	sedes_pkey	tabla sedes	\N	\N	\N
1	5000000031	2015-06-19 15:27:05.341-04:30	3378:3378:	SYNC	\N	\N	\N	\N	\N	\N	\N	\N
1	5000000032	2015-06-19 15:27:05.342-04:30	3379:3379:	SET_ADD_TABLE	1	15	public.departamentos	departamentos_pkey	tabla departamentos	\N	\N	\N
1	5000000033	2015-06-19 15:27:05.399-04:30	3380:3380:	SYNC	\N	\N	\N	\N	\N	\N	\N	\N
1	5000000034	2015-06-19 15:27:05.402-04:30	3381:3381:	SET_ADD_TABLE	1	16	public.prelaciones	prelaciones_pkey	tabla sedes	\N	\N	\N
1	5000000035	2015-06-19 15:27:05.421-04:30	3382:3382:	STORE_NODE	2	Sede foranea 2/sede2	\N	\N	\N	\N	\N	\N
1	5000000036	2015-06-19 15:27:05.421-04:30	3382:3382:	ENABLE_NODE	2	\N	\N	\N	\N	\N	\N	\N
1	5000000037	2015-06-19 15:27:06.506-04:30	3383:3383:	STORE_PATH	2	1	dbname= cursos host = 192.168.0.100 user=postgres password= 2222222	10	\N	\N	\N	\N
\.


--
-- TOC entry 2622 (class 0 OID 27538)
-- Dependencies: 214
-- Data for Name: sl_event_lock; Type: TABLE DATA; Schema: _slony_cursos; Owner: postgres
--

COPY sl_event_lock (dummy) FROM stdin;
\.


--
-- TOC entry 2925 (class 0 OID 0)
-- Dependencies: 210
-- Name: sl_event_seq; Type: SEQUENCE SET; Schema: _slony_cursos; Owner: postgres
--

SELECT pg_catalog.setval('sl_event_seq', 5000000037, true);


--
-- TOC entry 2607 (class 0 OID 27436)
-- Dependencies: 197
-- Data for Name: sl_listen; Type: TABLE DATA; Schema: _slony_cursos; Owner: postgres
--

COPY sl_listen (li_origin, li_provider, li_receiver) FROM stdin;
2	2	1
\.


--
-- TOC entry 2926 (class 0 OID 0)
-- Dependencies: 209
-- Name: sl_local_node_id; Type: SEQUENCE SET; Schema: _slony_cursos; Owner: postgres
--

SELECT pg_catalog.setval('sl_local_node_id', 1, true);


--
-- TOC entry 2612 (class 0 OID 27485)
-- Dependencies: 202
-- Data for Name: sl_log_1; Type: TABLE DATA; Schema: _slony_cursos; Owner: postgres
--

COPY sl_log_1 (log_origin, log_txid, log_tableid, log_actionseq, log_tablenspname, log_tablerelname, log_cmdtype, log_cmdupdncols, log_cmdargs) FROM stdin;
1	3388	14	1	public	sedes	I	0	{cod_sede,"3  ",des_sede,"Nueva sede"}
1	3391	14	2	public	sedes	U	1	{des_sede,africa,cod_sede,"3  "}
1	3392	14	3	public	sedes	I	0	{cod_sede,"4  ",des_sede,trtr}
1	3393	14	4	public	sedes	U	1	{des_sede,y,cod_sede,"4  "}
1	3394	14	5	public	sedes	U	1	{des_sede,df,cod_sede,"4  "}
1	3402	14	6	public	sedes	U	1	{des_sede,"Nueva Sede",cod_sede,"3  "}
1	3404	14	7	public	sedes	U	1	{des_sede,sede_prueba,cod_sede,"4  "}
\.


--
-- TOC entry 2613 (class 0 OID 27492)
-- Dependencies: 203
-- Data for Name: sl_log_2; Type: TABLE DATA; Schema: _slony_cursos; Owner: postgres
--

COPY sl_log_2 (log_origin, log_txid, log_tableid, log_actionseq, log_tablenspname, log_tablerelname, log_cmdtype, log_cmdupdncols, log_cmdargs) FROM stdin;
\.


--
-- TOC entry 2614 (class 0 OID 27499)
-- Dependencies: 204
-- Data for Name: sl_log_script; Type: TABLE DATA; Schema: _slony_cursos; Owner: postgres
--

COPY sl_log_script (log_origin, log_txid, log_actionseq, log_cmdtype, log_cmdargs) FROM stdin;
\.


--
-- TOC entry 2927 (class 0 OID 0)
-- Dependencies: 212
-- Name: sl_log_status; Type: SEQUENCE SET; Schema: _slony_cursos; Owner: postgres
--

SELECT pg_catalog.setval('sl_log_status', 0, true);


--
-- TOC entry 2599 (class 0 OID 27341)
-- Dependencies: 189
-- Data for Name: sl_node; Type: TABLE DATA; Schema: _slony_cursos; Owner: postgres
--

COPY sl_node (no_id, no_active, no_comment, no_failed) FROM stdin;
1	t	Sede Principal/sede1	f
2	t	Sede foranea 2/sede2	f
\.


--
-- TOC entry 2601 (class 0 OID 27351)
-- Dependencies: 191
-- Data for Name: sl_nodelock; Type: TABLE DATA; Schema: _slony_cursos; Owner: postgres
--

COPY sl_nodelock (nl_nodeid, nl_conncnt, nl_backendpid) FROM stdin;
\.


--
-- TOC entry 2928 (class 0 OID 0)
-- Dependencies: 190
-- Name: sl_nodelock_nl_conncnt_seq; Type: SEQUENCE SET; Schema: _slony_cursos; Owner: postgres
--

SELECT pg_catalog.setval('sl_nodelock_nl_conncnt_seq', 1, false);


--
-- TOC entry 2606 (class 0 OID 27418)
-- Dependencies: 196
-- Data for Name: sl_path; Type: TABLE DATA; Schema: _slony_cursos; Owner: postgres
--

COPY sl_path (pa_server, pa_client, pa_conninfo, pa_connretry) FROM stdin;
2	1	dbname= cursos host = 192.168.0.100 user=postgres password= 2222222	10
\.


--
-- TOC entry 2615 (class 0 OID 27506)
-- Dependencies: 205
-- Data for Name: sl_registry; Type: TABLE DATA; Schema: _slony_cursos; Owner: postgres
--

COPY sl_registry (reg_key, reg_int4, reg_text, reg_timestamp) FROM stdin;
\.


--
-- TOC entry 2611 (class 0 OID 27480)
-- Dependencies: 201
-- Data for Name: sl_seqlog; Type: TABLE DATA; Schema: _slony_cursos; Owner: postgres
--

COPY sl_seqlog (seql_seqid, seql_origin, seql_ev_seqno, seql_last_value) FROM stdin;
\.


--
-- TOC entry 2605 (class 0 OID 27403)
-- Dependencies: 195
-- Data for Name: sl_sequence; Type: TABLE DATA; Schema: _slony_cursos; Owner: postgres
--

COPY sl_sequence (seq_id, seq_reloid, seq_relname, seq_nspname, seq_set, seq_comment) FROM stdin;
\.


--
-- TOC entry 2602 (class 0 OID 27357)
-- Dependencies: 192
-- Data for Name: sl_set; Type: TABLE DATA; Schema: _slony_cursos; Owner: postgres
--

COPY sl_set (set_id, set_origin, set_locked, set_comment) FROM stdin;
1	1	\N	tablas de cursos
\.


--
-- TOC entry 2603 (class 0 OID 27370)
-- Dependencies: 193
-- Data for Name: sl_setsync; Type: TABLE DATA; Schema: _slony_cursos; Owner: postgres
--

COPY sl_setsync (ssy_setid, ssy_origin, ssy_seqno, ssy_snapshot, ssy_action_list) FROM stdin;
\.


--
-- TOC entry 2608 (class 0 OID 27451)
-- Dependencies: 198
-- Data for Name: sl_subscribe; Type: TABLE DATA; Schema: _slony_cursos; Owner: postgres
--

COPY sl_subscribe (sub_set, sub_provider, sub_receiver, sub_forward, sub_active) FROM stdin;
\.


--
-- TOC entry 2604 (class 0 OID 27388)
-- Dependencies: 194
-- Data for Name: sl_table; Type: TABLE DATA; Schema: _slony_cursos; Owner: postgres
--

COPY sl_table (tab_id, tab_reloid, tab_relname, tab_nspname, tab_set, tab_idxname, tab_altered, tab_comment) FROM stdin;
1	27044	empleados	public	1	empleados_pkey	f	tabla empleados
2	27051	empleados_sede1	public	1	empleados_sede1_pkey	f	tabla empleados sede 1
3	27058	empleados_sede2	public	1	empleados_sede2_pkey	f	tabla empleados sede 2
4	27065	empleados_sede3	public	1	empleados_sede3_pkey	f	tabla empleados sede 3
5	27020	ediciones	public	1	ediciones_pkey	f	tabla ediciones
6	27026	ediciones_sede1	public	1	ediciones_sede1_pkey	f	tabla ediciones sede 1
7	27032	ediciones_sede2	public	1	ediciones_sede2_pkey	f	tabla ediciones sede 2
8	27038	ediciones_sede3	public	1	ediciones_sede3_pkey	f	tabla ediciones sede 3
9	27072	participantes	public	1	participantes_pkey	f	tabla participantes
10	27078	participantes_sede1	public	1	participantes_sede1_pkey	f	tabla participantes sede 1
11	27084	participantes_sede2	public	1	participantes_sede2_pkey	f	tabla participantes sede 2
12	27090	participantes_sede3	public	1	participantes_sede3_pkey	f	tabla participantes sede 3
13	27010	cursos	public	1	cursos_pkey	f	tabla cursos
14	27099	sedes	public	1	sedes_pkey	f	tabla sedes
15	27017	departamentos	public	1	departamentos_pkey	f	tabla departamentos
16	27096	prelaciones	public	1	prelaciones_pkey	f	tabla sedes
\.


SET search_path = public, pg_catalog;

--
-- TOC entry 2583 (class 0 OID 27010)
-- Dependencies: 173
-- Data for Name: cursos; Type: TABLE DATA; Schema: public; Owner: grupo_admin
--

COPY cursos (cod_curso, des_curso, area, nivel) FROM stdin;
1	progamacion 1	Computación	1
2	programacion 2	Computación	2
3	programacion 3	Computación	3
\.


--
-- TOC entry 2584 (class 0 OID 27017)
-- Dependencies: 174
-- Data for Name: departamentos; Type: TABLE DATA; Schema: public; Owner: grupo_admin
--

COPY departamentos (cod_dpto, des_dpto) FROM stdin;
1     	estadistica
2     	rrhh
\.


--
-- TOC entry 2585 (class 0 OID 27020)
-- Dependencies: 175
-- Data for Name: ediciones; Type: TABLE DATA; Schema: public; Owner: grupo_admin
--

COPY ediciones (cod_curso, lapso, status, instructor, cod_sede) FROM stdin;
1	2015-01	O	3    	1  
2	2015-02	O	3    	1  
3	2016-01	O	3    	1  
2	2016-02	O	1    	1  
\.


--
-- TOC entry 2586 (class 0 OID 27026)
-- Dependencies: 176
-- Data for Name: ediciones_sede1; Type: TABLE DATA; Schema: public; Owner: grupo_admin
--

COPY ediciones_sede1 (cod_curso, lapso, status, instructor, cod_sede) FROM stdin;
\.


--
-- TOC entry 2587 (class 0 OID 27032)
-- Dependencies: 177
-- Data for Name: ediciones_sede2; Type: TABLE DATA; Schema: public; Owner: grupo_admin
--

COPY ediciones_sede2 (cod_curso, lapso, status, instructor, cod_sede) FROM stdin;
\.


--
-- TOC entry 2588 (class 0 OID 27038)
-- Dependencies: 178
-- Data for Name: ediciones_sede3; Type: TABLE DATA; Schema: public; Owner: grupo_admin
--

COPY ediciones_sede3 (cod_curso, lapso, status, instructor, cod_sede) FROM stdin;
\.


--
-- TOC entry 2589 (class 0 OID 27044)
-- Dependencies: 179
-- Data for Name: empleados; Type: TABLE DATA; Schema: public; Owner: grupo_admin
--

COPY empleados (ficha, nombre, cargo, sueldo, fecha_ingreso, status, cod_dpto, cod_sede) FROM stdin;
1    	stalin	analista	200.00	2015-05-02	A	1     	1  
2    	loyher	diseñador	1000.00	2009-05-02	J	2     	2  
3    	prof	prof	10000.00	2099-11-15	A	1     	1  
\.


--
-- TOC entry 2590 (class 0 OID 27051)
-- Dependencies: 180
-- Data for Name: empleados_sede1; Type: TABLE DATA; Schema: public; Owner: grupo_admin
--

COPY empleados_sede1 (ficha, nombre, cargo, sueldo, fecha_ingreso, status, cod_dpto, cod_sede) FROM stdin;
\.


--
-- TOC entry 2591 (class 0 OID 27058)
-- Dependencies: 181
-- Data for Name: empleados_sede2; Type: TABLE DATA; Schema: public; Owner: grupo_admin
--

COPY empleados_sede2 (ficha, nombre, cargo, sueldo, fecha_ingreso, status, cod_dpto, cod_sede) FROM stdin;
\.


--
-- TOC entry 2592 (class 0 OID 27065)
-- Dependencies: 182
-- Data for Name: empleados_sede3; Type: TABLE DATA; Schema: public; Owner: grupo_admin
--

COPY empleados_sede3 (ficha, nombre, cargo, sueldo, fecha_ingreso, status, cod_dpto, cod_sede) FROM stdin;
\.


--
-- TOC entry 2593 (class 0 OID 27072)
-- Dependencies: 183
-- Data for Name: participantes; Type: TABLE DATA; Schema: public; Owner: grupo_admin
--

COPY participantes (cod_curso, lapso, nota, ficha) FROM stdin;
1	2015-01	A	1    
2	2015-02	A	1    
3	2016-01	A	1    
\.


--
-- TOC entry 2594 (class 0 OID 27078)
-- Dependencies: 184
-- Data for Name: participantes_sede1; Type: TABLE DATA; Schema: public; Owner: grupo_admin
--

COPY participantes_sede1 (cod_curso, lapso, nota, ficha) FROM stdin;
\.


--
-- TOC entry 2595 (class 0 OID 27084)
-- Dependencies: 185
-- Data for Name: participantes_sede2; Type: TABLE DATA; Schema: public; Owner: grupo_admin
--

COPY participantes_sede2 (cod_curso, lapso, nota, ficha) FROM stdin;
\.


--
-- TOC entry 2596 (class 0 OID 27090)
-- Dependencies: 186
-- Data for Name: participantes_sede3; Type: TABLE DATA; Schema: public; Owner: grupo_admin
--

COPY participantes_sede3 (cod_curso, lapso, nota, ficha) FROM stdin;
\.


--
-- TOC entry 2597 (class 0 OID 27096)
-- Dependencies: 187
-- Data for Name: prelaciones; Type: TABLE DATA; Schema: public; Owner: grupo_admin
--

COPY prelaciones (cod_curso, cod_prelacion) FROM stdin;
\.


--
-- TOC entry 2598 (class 0 OID 27099)
-- Dependencies: 188
-- Data for Name: sedes; Type: TABLE DATA; Schema: public; Owner: grupo_admin
--

COPY sedes (cod_sede, des_sede) FROM stdin;
1  	atlantico
2  	villa asia
3  	Nueva Sede
4  	sede_prueba
\.


SET search_path = _slony_cursos, pg_catalog;

--
-- TOC entry 2308 (class 2606 OID 27551)
-- Name: sl_components_pkey; Type: CONSTRAINT; Schema: _slony_cursos; Owner: postgres; Tablespace: 
--

ALTER TABLE ONLY sl_components
    ADD CONSTRAINT sl_components_pkey PRIMARY KEY (co_actor);


--
-- TOC entry 2297 (class 2606 OID 27473)
-- Name: sl_event-pkey; Type: CONSTRAINT; Schema: _slony_cursos; Owner: postgres; Tablespace: 
--

ALTER TABLE ONLY sl_event
    ADD CONSTRAINT "sl_event-pkey" PRIMARY KEY (ev_origin, ev_seqno);


--
-- TOC entry 2293 (class 2606 OID 27440)
-- Name: sl_listen-pkey; Type: CONSTRAINT; Schema: _slony_cursos; Owner: postgres; Tablespace: 
--

ALTER TABLE ONLY sl_listen
    ADD CONSTRAINT "sl_listen-pkey" PRIMARY KEY (li_origin, li_provider, li_receiver);


--
-- TOC entry 2275 (class 2606 OID 27348)
-- Name: sl_node-pkey; Type: CONSTRAINT; Schema: _slony_cursos; Owner: postgres; Tablespace: 
--

ALTER TABLE ONLY sl_node
    ADD CONSTRAINT "sl_node-pkey" PRIMARY KEY (no_id);


--
-- TOC entry 2277 (class 2606 OID 27356)
-- Name: sl_nodelock-pkey; Type: CONSTRAINT; Schema: _slony_cursos; Owner: postgres; Tablespace: 
--

ALTER TABLE ONLY sl_nodelock
    ADD CONSTRAINT "sl_nodelock-pkey" PRIMARY KEY (nl_nodeid, nl_conncnt);


--
-- TOC entry 2291 (class 2606 OID 27425)
-- Name: sl_path-pkey; Type: CONSTRAINT; Schema: _slony_cursos; Owner: postgres; Tablespace: 
--

ALTER TABLE ONLY sl_path
    ADD CONSTRAINT "sl_path-pkey" PRIMARY KEY (pa_server, pa_client);


--
-- TOC entry 2305 (class 2606 OID 27513)
-- Name: sl_registry_pkey; Type: CONSTRAINT; Schema: _slony_cursos; Owner: postgres; Tablespace: 
--

ALTER TABLE ONLY sl_registry
    ADD CONSTRAINT sl_registry_pkey PRIMARY KEY (reg_key);


--
-- TOC entry 2287 (class 2606 OID 27410)
-- Name: sl_sequence-pkey; Type: CONSTRAINT; Schema: _slony_cursos; Owner: postgres; Tablespace: 
--

ALTER TABLE ONLY sl_sequence
    ADD CONSTRAINT "sl_sequence-pkey" PRIMARY KEY (seq_id);


--
-- TOC entry 2289 (class 2606 OID 27412)
-- Name: sl_sequence_seq_reloid_key; Type: CONSTRAINT; Schema: _slony_cursos; Owner: postgres; Tablespace: 
--

ALTER TABLE ONLY sl_sequence
    ADD CONSTRAINT sl_sequence_seq_reloid_key UNIQUE (seq_reloid);


--
-- TOC entry 2279 (class 2606 OID 27364)
-- Name: sl_set-pkey; Type: CONSTRAINT; Schema: _slony_cursos; Owner: postgres; Tablespace: 
--

ALTER TABLE ONLY sl_set
    ADD CONSTRAINT "sl_set-pkey" PRIMARY KEY (set_id);


--
-- TOC entry 2281 (class 2606 OID 27377)
-- Name: sl_setsync-pkey; Type: CONSTRAINT; Schema: _slony_cursos; Owner: postgres; Tablespace: 
--

ALTER TABLE ONLY sl_setsync
    ADD CONSTRAINT "sl_setsync-pkey" PRIMARY KEY (ssy_setid);


--
-- TOC entry 2295 (class 2606 OID 27455)
-- Name: sl_subscribe-pkey; Type: CONSTRAINT; Schema: _slony_cursos; Owner: postgres; Tablespace: 
--

ALTER TABLE ONLY sl_subscribe
    ADD CONSTRAINT "sl_subscribe-pkey" PRIMARY KEY (sub_receiver, sub_set);


--
-- TOC entry 2283 (class 2606 OID 27395)
-- Name: sl_table-pkey; Type: CONSTRAINT; Schema: _slony_cursos; Owner: postgres; Tablespace: 
--

ALTER TABLE ONLY sl_table
    ADD CONSTRAINT "sl_table-pkey" PRIMARY KEY (tab_id);


--
-- TOC entry 2285 (class 2606 OID 27397)
-- Name: sl_table_tab_reloid_key; Type: CONSTRAINT; Schema: _slony_cursos; Owner: postgres; Tablespace: 
--

ALTER TABLE ONLY sl_table
    ADD CONSTRAINT sl_table_tab_reloid_key UNIQUE (tab_reloid);


SET search_path = public, pg_catalog;

--
-- TOC entry 2243 (class 2606 OID 27103)
-- Name: cursos_pkey; Type: CONSTRAINT; Schema: public; Owner: grupo_admin; Tablespace: 
--

ALTER TABLE ONLY cursos
    ADD CONSTRAINT cursos_pkey PRIMARY KEY (cod_curso);


--
-- TOC entry 2245 (class 2606 OID 27105)
-- Name: departamentos_pkey; Type: CONSTRAINT; Schema: public; Owner: grupo_admin; Tablespace: 
--

ALTER TABLE ONLY departamentos
    ADD CONSTRAINT departamentos_pkey PRIMARY KEY (cod_dpto);


--
-- TOC entry 2247 (class 2606 OID 27107)
-- Name: ediciones_pkey; Type: CONSTRAINT; Schema: public; Owner: grupo_admin; Tablespace: 
--

ALTER TABLE ONLY ediciones
    ADD CONSTRAINT ediciones_pkey PRIMARY KEY (cod_curso, lapso, cod_sede);


--
-- TOC entry 2249 (class 2606 OID 27109)
-- Name: ediciones_sede1_pkey; Type: CONSTRAINT; Schema: public; Owner: grupo_admin; Tablespace: 
--

ALTER TABLE ONLY ediciones_sede1
    ADD CONSTRAINT ediciones_sede1_pkey PRIMARY KEY (cod_curso, lapso, cod_sede);


--
-- TOC entry 2251 (class 2606 OID 27111)
-- Name: ediciones_sede2_pkey; Type: CONSTRAINT; Schema: public; Owner: grupo_admin; Tablespace: 
--

ALTER TABLE ONLY ediciones_sede2
    ADD CONSTRAINT ediciones_sede2_pkey PRIMARY KEY (cod_curso, lapso, cod_sede);


--
-- TOC entry 2253 (class 2606 OID 27113)
-- Name: ediciones_sede3_pkey; Type: CONSTRAINT; Schema: public; Owner: grupo_admin; Tablespace: 
--

ALTER TABLE ONLY ediciones_sede3
    ADD CONSTRAINT ediciones_sede3_pkey PRIMARY KEY (cod_curso, lapso, cod_sede);


--
-- TOC entry 2255 (class 2606 OID 27115)
-- Name: empleados_pkey; Type: CONSTRAINT; Schema: public; Owner: grupo_admin; Tablespace: 
--

ALTER TABLE ONLY empleados
    ADD CONSTRAINT empleados_pkey PRIMARY KEY (ficha);


--
-- TOC entry 2257 (class 2606 OID 27117)
-- Name: empleados_sede1_pkey; Type: CONSTRAINT; Schema: public; Owner: grupo_admin; Tablespace: 
--

ALTER TABLE ONLY empleados_sede1
    ADD CONSTRAINT empleados_sede1_pkey PRIMARY KEY (ficha);


--
-- TOC entry 2259 (class 2606 OID 27119)
-- Name: empleados_sede2_pkey; Type: CONSTRAINT; Schema: public; Owner: grupo_admin; Tablespace: 
--

ALTER TABLE ONLY empleados_sede2
    ADD CONSTRAINT empleados_sede2_pkey PRIMARY KEY (ficha);


--
-- TOC entry 2261 (class 2606 OID 27121)
-- Name: empleados_sede3_pkey; Type: CONSTRAINT; Schema: public; Owner: grupo_admin; Tablespace: 
--

ALTER TABLE ONLY empleados_sede3
    ADD CONSTRAINT empleados_sede3_pkey PRIMARY KEY (ficha);


--
-- TOC entry 2263 (class 2606 OID 27123)
-- Name: participantes_pkey; Type: CONSTRAINT; Schema: public; Owner: grupo_admin; Tablespace: 
--

ALTER TABLE ONLY participantes
    ADD CONSTRAINT participantes_pkey PRIMARY KEY (cod_curso, lapso, ficha);


--
-- TOC entry 2265 (class 2606 OID 27125)
-- Name: participantes_sede1_pkey; Type: CONSTRAINT; Schema: public; Owner: grupo_admin; Tablespace: 
--

ALTER TABLE ONLY participantes_sede1
    ADD CONSTRAINT participantes_sede1_pkey PRIMARY KEY (cod_curso, lapso, ficha);


--
-- TOC entry 2267 (class 2606 OID 27127)
-- Name: participantes_sede2_pkey; Type: CONSTRAINT; Schema: public; Owner: grupo_admin; Tablespace: 
--

ALTER TABLE ONLY participantes_sede2
    ADD CONSTRAINT participantes_sede2_pkey PRIMARY KEY (cod_curso, lapso, ficha);


--
-- TOC entry 2269 (class 2606 OID 27129)
-- Name: participantes_sede3_pkey; Type: CONSTRAINT; Schema: public; Owner: grupo_admin; Tablespace: 
--

ALTER TABLE ONLY participantes_sede3
    ADD CONSTRAINT participantes_sede3_pkey PRIMARY KEY (cod_curso, lapso, ficha);


--
-- TOC entry 2271 (class 2606 OID 27131)
-- Name: prelaciones_pkey; Type: CONSTRAINT; Schema: public; Owner: grupo_admin; Tablespace: 
--

ALTER TABLE ONLY prelaciones
    ADD CONSTRAINT prelaciones_pkey PRIMARY KEY (cod_curso, cod_prelacion);


--
-- TOC entry 2273 (class 2606 OID 27133)
-- Name: sedes_pkey; Type: CONSTRAINT; Schema: public; Owner: grupo_admin; Tablespace: 
--

ALTER TABLE ONLY sedes
    ADD CONSTRAINT sedes_pkey PRIMARY KEY (cod_sede);


SET search_path = _slony_cursos, pg_catalog;

--
-- TOC entry 2306 (class 1259 OID 27517)
-- Name: sl_apply_stats_idx1; Type: INDEX; Schema: _slony_cursos; Owner: postgres; Tablespace: 
--

CREATE INDEX sl_apply_stats_idx1 ON sl_apply_stats USING btree (as_origin);


--
-- TOC entry 2298 (class 1259 OID 27478)
-- Name: sl_confirm_idx1; Type: INDEX; Schema: _slony_cursos; Owner: postgres; Tablespace: 
--

CREATE INDEX sl_confirm_idx1 ON sl_confirm USING btree (con_origin, con_received, con_seqno);


--
-- TOC entry 2299 (class 1259 OID 27479)
-- Name: sl_confirm_idx2; Type: INDEX; Schema: _slony_cursos; Owner: postgres; Tablespace: 
--

CREATE INDEX sl_confirm_idx2 ON sl_confirm USING btree (con_received, con_seqno);


--
-- TOC entry 2301 (class 1259 OID 27491)
-- Name: sl_log_1_idx1; Type: INDEX; Schema: _slony_cursos; Owner: postgres; Tablespace: 
--

CREATE INDEX sl_log_1_idx1 ON sl_log_1 USING btree (log_origin, log_txid, log_actionseq);


--
-- TOC entry 2302 (class 1259 OID 27498)
-- Name: sl_log_2_idx1; Type: INDEX; Schema: _slony_cursos; Owner: postgres; Tablespace: 
--

CREATE INDEX sl_log_2_idx1 ON sl_log_2 USING btree (log_origin, log_txid, log_actionseq);


--
-- TOC entry 2303 (class 1259 OID 27505)
-- Name: sl_log_script_idx1; Type: INDEX; Schema: _slony_cursos; Owner: postgres; Tablespace: 
--

CREATE INDEX sl_log_script_idx1 ON sl_log_script USING btree (log_origin, log_txid, log_actionseq);


--
-- TOC entry 2300 (class 1259 OID 27483)
-- Name: sl_seqlog_idx; Type: INDEX; Schema: _slony_cursos; Owner: postgres; Tablespace: 
--

CREATE INDEX sl_seqlog_idx ON sl_seqlog USING btree (seql_origin, seql_ev_seqno, seql_seqid);


SET search_path = public, pg_catalog;

--
-- TOC entry 2553 (class 2618 OID 27134)
-- Name: ediciones_sede1_del; Type: RULE; Schema: public; Owner: grupo_admin
--

CREATE RULE ediciones_sede1_del AS
    ON DELETE TO ediciones_sede1
   WHERE (old.cod_sede = '001'::bpchar) DO INSTEAD  DELETE FROM ediciones
  WHERE (((ediciones.cod_curso = old.cod_curso) AND (ediciones.lapso = old.lapso)) AND (ediciones.cod_sede = old.cod_sede));


--
-- TOC entry 2554 (class 2618 OID 27135)
-- Name: ediciones_sede1_ins; Type: RULE; Schema: public; Owner: grupo_admin
--

CREATE RULE ediciones_sede1_ins AS
    ON INSERT TO ediciones_sede1
   WHERE (new.cod_sede = '001'::bpchar) DO INSTEAD  INSERT INTO ediciones (cod_curso, lapso, status, instructor, cod_sede)
  VALUES (new.cod_curso, new.lapso, new.status, new.instructor, new.cod_sede);


--
-- TOC entry 2555 (class 2618 OID 27136)
-- Name: ediciones_sede1_upd; Type: RULE; Schema: public; Owner: grupo_admin
--

CREATE RULE ediciones_sede1_upd AS
    ON UPDATE TO ediciones_sede1
   WHERE (old.cod_sede = '001'::bpchar) DO INSTEAD  UPDATE ediciones SET cod_curso = new.cod_curso, lapso = new.lapso, status = new.status, instructor = new.instructor, cod_sede = new.cod_sede
  WHERE (((ediciones.cod_curso = old.cod_curso) AND (ediciones.lapso = old.lapso)) AND (ediciones.cod_sede = old.cod_sede));


--
-- TOC entry 2556 (class 2618 OID 27137)
-- Name: ediciones_sede2_del; Type: RULE; Schema: public; Owner: grupo_admin
--

CREATE RULE ediciones_sede2_del AS
    ON DELETE TO ediciones_sede2
   WHERE (old.cod_sede = '002'::bpchar) DO INSTEAD  DELETE FROM ediciones
  WHERE (((ediciones.cod_curso = old.cod_curso) AND (ediciones.lapso = old.lapso)) AND (ediciones.cod_sede = old.cod_sede));


--
-- TOC entry 2557 (class 2618 OID 27138)
-- Name: ediciones_sede2_ins; Type: RULE; Schema: public; Owner: grupo_admin
--

CREATE RULE ediciones_sede2_ins AS
    ON INSERT TO ediciones_sede2
   WHERE (new.cod_sede = '002'::bpchar) DO INSTEAD  INSERT INTO ediciones (cod_curso, lapso, status, instructor, cod_sede)
  VALUES (new.cod_curso, new.lapso, new.status, new.instructor, new.cod_sede);


--
-- TOC entry 2558 (class 2618 OID 27139)
-- Name: ediciones_sede2_upd; Type: RULE; Schema: public; Owner: grupo_admin
--

CREATE RULE ediciones_sede2_upd AS
    ON UPDATE TO ediciones_sede2
   WHERE (old.cod_sede = '002'::bpchar) DO INSTEAD  UPDATE ediciones SET cod_curso = new.cod_curso, lapso = new.lapso, status = new.status, instructor = new.instructor, cod_sede = new.cod_sede
  WHERE (((ediciones.cod_curso = old.cod_curso) AND (ediciones.lapso = old.lapso)) AND (ediciones.cod_sede = old.cod_sede));


--
-- TOC entry 2559 (class 2618 OID 27140)
-- Name: ediciones_sede3_del; Type: RULE; Schema: public; Owner: grupo_admin
--

CREATE RULE ediciones_sede3_del AS
    ON DELETE TO ediciones_sede3
   WHERE (old.cod_sede = '003'::bpchar) DO INSTEAD  DELETE FROM ediciones
  WHERE (((ediciones.cod_curso = old.cod_curso) AND (ediciones.lapso = old.lapso)) AND (ediciones.cod_sede = old.cod_sede));


--
-- TOC entry 2560 (class 2618 OID 27141)
-- Name: ediciones_sede3_ins; Type: RULE; Schema: public; Owner: grupo_admin
--

CREATE RULE ediciones_sede3_ins AS
    ON INSERT TO ediciones_sede3
   WHERE (new.cod_sede = '003'::bpchar) DO INSTEAD  INSERT INTO ediciones (cod_curso, lapso, status, instructor, cod_sede)
  VALUES (new.cod_curso, new.lapso, new.status, new.instructor, new.cod_sede);


--
-- TOC entry 2561 (class 2618 OID 27142)
-- Name: ediciones_sede3_upd; Type: RULE; Schema: public; Owner: grupo_admin
--

CREATE RULE ediciones_sede3_upd AS
    ON UPDATE TO ediciones_sede3
   WHERE (old.cod_sede = '003'::bpchar) DO INSTEAD  UPDATE ediciones SET cod_curso = new.cod_curso, lapso = new.lapso, status = new.status, instructor = new.instructor, cod_sede = new.cod_sede
  WHERE (((ediciones.cod_curso = old.cod_curso) AND (ediciones.lapso = old.lapso)) AND (ediciones.cod_sede = old.cod_sede));


--
-- TOC entry 2562 (class 2618 OID 27143)
-- Name: empleados_sede1_del; Type: RULE; Schema: public; Owner: grupo_admin
--

CREATE RULE empleados_sede1_del AS
    ON DELETE TO empleados_sede1
   WHERE (old.cod_sede = '001'::bpchar) DO INSTEAD  DELETE FROM empleados
  WHERE (empleados.ficha = old.ficha);


--
-- TOC entry 2563 (class 2618 OID 27144)
-- Name: empleados_sede1_ins; Type: RULE; Schema: public; Owner: grupo_admin
--

CREATE RULE empleados_sede1_ins AS
    ON INSERT TO empleados_sede1
   WHERE (new.cod_sede = '001'::bpchar) DO INSTEAD  INSERT INTO empleados (ficha, nombre, cargo, sueldo, fecha_ingreso, status, cod_dpto, cod_sede)
  VALUES (new.ficha, new.nombre, new.cargo, new.sueldo, new.fecha_ingreso, new.status, new.cod_dpto, new.cod_sede);


--
-- TOC entry 2564 (class 2618 OID 27145)
-- Name: empleados_sede1_upd; Type: RULE; Schema: public; Owner: grupo_admin
--

CREATE RULE empleados_sede1_upd AS
    ON UPDATE TO empleados_sede1
   WHERE (old.cod_sede = '001'::bpchar) DO INSTEAD  UPDATE empleados SET ficha = new.ficha, nombre = new.nombre, cargo = new.cargo, sueldo = new.sueldo, fecha_ingreso = new.fecha_ingreso, status = new.status, cod_dpto = new.cod_dpto
  WHERE (empleados.ficha = old.ficha);


--
-- TOC entry 2565 (class 2618 OID 27146)
-- Name: empleados_sede2_del; Type: RULE; Schema: public; Owner: grupo_admin
--

CREATE RULE empleados_sede2_del AS
    ON DELETE TO empleados_sede2
   WHERE (old.cod_sede = '002'::bpchar) DO INSTEAD  DELETE FROM empleados
  WHERE (empleados.ficha = old.ficha);


--
-- TOC entry 2566 (class 2618 OID 27147)
-- Name: empleados_sede2_ins; Type: RULE; Schema: public; Owner: grupo_admin
--

CREATE RULE empleados_sede2_ins AS
    ON INSERT TO empleados_sede2
   WHERE (new.cod_sede = '002'::bpchar) DO INSTEAD  INSERT INTO empleados (ficha, nombre, cargo, sueldo, fecha_ingreso, status, cod_dpto, cod_sede)
  VALUES (new.ficha, new.nombre, new.cargo, new.sueldo, new.fecha_ingreso, new.status, new.cod_dpto, new.cod_sede);


--
-- TOC entry 2567 (class 2618 OID 27148)
-- Name: empleados_sede2_upd; Type: RULE; Schema: public; Owner: grupo_admin
--

CREATE RULE empleados_sede2_upd AS
    ON UPDATE TO empleados_sede2
   WHERE (old.cod_sede = '002'::bpchar) DO INSTEAD  UPDATE empleados SET ficha = new.ficha, nombre = new.nombre, cargo = new.cargo, sueldo = new.sueldo, fecha_ingreso = new.fecha_ingreso, status = new.status, cod_dpto = new.cod_dpto
  WHERE (empleados.ficha = old.ficha);


--
-- TOC entry 2568 (class 2618 OID 27149)
-- Name: empleados_sede3_del; Type: RULE; Schema: public; Owner: grupo_admin
--

CREATE RULE empleados_sede3_del AS
    ON DELETE TO empleados_sede3
   WHERE (old.cod_sede = '003'::bpchar) DO INSTEAD  DELETE FROM empleados
  WHERE (empleados.ficha = old.ficha);


--
-- TOC entry 2569 (class 2618 OID 27150)
-- Name: empleados_sede3_ins; Type: RULE; Schema: public; Owner: grupo_admin
--

CREATE RULE empleados_sede3_ins AS
    ON INSERT TO empleados_sede3
   WHERE (new.cod_sede = '003'::bpchar) DO INSTEAD  INSERT INTO empleados (ficha, nombre, cargo, sueldo, fecha_ingreso, status, cod_dpto, cod_sede)
  VALUES (new.ficha, new.nombre, new.cargo, new.sueldo, new.fecha_ingreso, new.status, new.cod_dpto, new.cod_sede);


--
-- TOC entry 2570 (class 2618 OID 27151)
-- Name: empleados_sede3_upd; Type: RULE; Schema: public; Owner: grupo_admin
--

CREATE RULE empleados_sede3_upd AS
    ON UPDATE TO empleados_sede3
   WHERE (old.cod_sede = '003'::bpchar) DO INSTEAD  UPDATE empleados SET ficha = new.ficha, nombre = new.nombre, cargo = new.cargo, sueldo = new.sueldo, fecha_ingreso = new.fecha_ingreso, status = new.status, cod_dpto = new.cod_dpto
  WHERE (empleados.ficha = old.ficha);


--
-- TOC entry 2571 (class 2618 OID 27152)
-- Name: participantes_sede1_del; Type: RULE; Schema: public; Owner: grupo_admin
--

CREATE RULE participantes_sede1_del AS
    ON DELETE TO participantes_sede1 DO INSTEAD  DELETE FROM participantes
  WHERE (participantes.ficha = old.ficha);


--
-- TOC entry 2572 (class 2618 OID 27153)
-- Name: participantes_sede1_ins; Type: RULE; Schema: public; Owner: grupo_admin
--

CREATE RULE participantes_sede1_ins AS
    ON INSERT TO participantes_sede1 DO INSTEAD  INSERT INTO participantes (cod_curso, lapso, nota, ficha)
  VALUES (new.cod_curso, new.lapso, new.nota, new.ficha);


--
-- TOC entry 2573 (class 2618 OID 27154)
-- Name: participantes_sede1_upd; Type: RULE; Schema: public; Owner: grupo_admin
--

CREATE RULE participantes_sede1_upd AS
    ON UPDATE TO participantes_sede1 DO INSTEAD  UPDATE participantes SET cod_curso = new.cod_curso, lapso = new.lapso, nota = new.nota, ficha = new.ficha
  WHERE (participantes.ficha = old.ficha);


--
-- TOC entry 2574 (class 2618 OID 27155)
-- Name: participantes_sede2_del; Type: RULE; Schema: public; Owner: grupo_admin
--

CREATE RULE participantes_sede2_del AS
    ON DELETE TO participantes_sede2 DO INSTEAD  DELETE FROM participantes
  WHERE (participantes.ficha = old.ficha);


--
-- TOC entry 2575 (class 2618 OID 27156)
-- Name: participantes_sede2_ins; Type: RULE; Schema: public; Owner: grupo_admin
--

CREATE RULE participantes_sede2_ins AS
    ON INSERT TO participantes_sede2 DO INSTEAD  INSERT INTO participantes (cod_curso, lapso, nota, ficha)
  VALUES (new.cod_curso, new.lapso, new.nota, new.ficha);


--
-- TOC entry 2576 (class 2618 OID 27157)
-- Name: participantes_sede2_upd; Type: RULE; Schema: public; Owner: grupo_admin
--

CREATE RULE participantes_sede2_upd AS
    ON UPDATE TO participantes_sede2 DO INSTEAD  UPDATE participantes SET cod_curso = new.cod_curso, lapso = new.lapso, nota = new.nota, ficha = new.ficha
  WHERE (participantes.ficha = old.ficha);


--
-- TOC entry 2577 (class 2618 OID 27158)
-- Name: participantes_sede3_del; Type: RULE; Schema: public; Owner: grupo_admin
--

CREATE RULE participantes_sede3_del AS
    ON DELETE TO participantes_sede3 DO INSTEAD  DELETE FROM participantes
  WHERE (participantes.ficha = old.ficha);


--
-- TOC entry 2578 (class 2618 OID 27159)
-- Name: participantes_sede3_ins; Type: RULE; Schema: public; Owner: grupo_admin
--

CREATE RULE participantes_sede3_ins AS
    ON INSERT TO participantes_sede3 DO INSTEAD  INSERT INTO participantes (cod_curso, lapso, nota, ficha)
  VALUES (new.cod_curso, new.lapso, new.nota, new.ficha);


--
-- TOC entry 2579 (class 2618 OID 27160)
-- Name: participantes_sede3_upd; Type: RULE; Schema: public; Owner: grupo_admin
--

CREATE RULE participantes_sede3_upd AS
    ON UPDATE TO participantes_sede3 DO INSTEAD  UPDATE participantes SET cod_curso = new.cod_curso, lapso = new.lapso, nota = new.nota, ficha = new.ficha
  WHERE (participantes.ficha = old.ficha);


SET search_path = _slony_cursos, pg_catalog;

--
-- TOC entry 2442 (class 2620 OID 27705)
-- Name: apply_trigger; Type: TRIGGER; Schema: _slony_cursos; Owner: postgres
--

CREATE TRIGGER apply_trigger BEFORE INSERT ON sl_log_1 FOR EACH ROW EXECUTE PROCEDURE logapply('_slony_cursos');

ALTER TABLE sl_log_1 ENABLE REPLICA TRIGGER apply_trigger;


--
-- TOC entry 2443 (class 2620 OID 27706)
-- Name: apply_trigger; Type: TRIGGER; Schema: _slony_cursos; Owner: postgres
--

CREATE TRIGGER apply_trigger BEFORE INSERT ON sl_log_2 FOR EACH ROW EXECUTE PROCEDURE logapply('_slony_cursos');

ALTER TABLE sl_log_2 ENABLE REPLICA TRIGGER apply_trigger;


SET search_path = public, pg_catalog;

--
-- TOC entry 2384 (class 2620 OID 27708)
-- Name: _slony_cursos_denyaccess; Type: TRIGGER; Schema: public; Owner: grupo_admin
--

CREATE TRIGGER _slony_cursos_denyaccess BEFORE INSERT OR DELETE OR UPDATE ON empleados FOR EACH ROW EXECUTE PROCEDURE _slony_cursos.denyaccess('_slony_cursos');

ALTER TABLE empleados DISABLE TRIGGER _slony_cursos_denyaccess;


--
-- TOC entry 2388 (class 2620 OID 27712)
-- Name: _slony_cursos_denyaccess; Type: TRIGGER; Schema: public; Owner: grupo_admin
--

CREATE TRIGGER _slony_cursos_denyaccess BEFORE INSERT OR DELETE OR UPDATE ON empleados_sede1 FOR EACH ROW EXECUTE PROCEDURE _slony_cursos.denyaccess('_slony_cursos');

ALTER TABLE empleados_sede1 DISABLE TRIGGER _slony_cursos_denyaccess;


--
-- TOC entry 2392 (class 2620 OID 27716)
-- Name: _slony_cursos_denyaccess; Type: TRIGGER; Schema: public; Owner: grupo_admin
--

CREATE TRIGGER _slony_cursos_denyaccess BEFORE INSERT OR DELETE OR UPDATE ON empleados_sede2 FOR EACH ROW EXECUTE PROCEDURE _slony_cursos.denyaccess('_slony_cursos');

ALTER TABLE empleados_sede2 DISABLE TRIGGER _slony_cursos_denyaccess;


--
-- TOC entry 2396 (class 2620 OID 27720)
-- Name: _slony_cursos_denyaccess; Type: TRIGGER; Schema: public; Owner: grupo_admin
--

CREATE TRIGGER _slony_cursos_denyaccess BEFORE INSERT OR DELETE OR UPDATE ON empleados_sede3 FOR EACH ROW EXECUTE PROCEDURE _slony_cursos.denyaccess('_slony_cursos');

ALTER TABLE empleados_sede3 DISABLE TRIGGER _slony_cursos_denyaccess;


--
-- TOC entry 2362 (class 2620 OID 27724)
-- Name: _slony_cursos_denyaccess; Type: TRIGGER; Schema: public; Owner: grupo_admin
--

CREATE TRIGGER _slony_cursos_denyaccess BEFORE INSERT OR DELETE OR UPDATE ON ediciones FOR EACH ROW EXECUTE PROCEDURE _slony_cursos.denyaccess('_slony_cursos');

ALTER TABLE ediciones DISABLE TRIGGER _slony_cursos_denyaccess;


--
-- TOC entry 2368 (class 2620 OID 27728)
-- Name: _slony_cursos_denyaccess; Type: TRIGGER; Schema: public; Owner: grupo_admin
--

CREATE TRIGGER _slony_cursos_denyaccess BEFORE INSERT OR DELETE OR UPDATE ON ediciones_sede1 FOR EACH ROW EXECUTE PROCEDURE _slony_cursos.denyaccess('_slony_cursos');

ALTER TABLE ediciones_sede1 DISABLE TRIGGER _slony_cursos_denyaccess;


--
-- TOC entry 2374 (class 2620 OID 27732)
-- Name: _slony_cursos_denyaccess; Type: TRIGGER; Schema: public; Owner: grupo_admin
--

CREATE TRIGGER _slony_cursos_denyaccess BEFORE INSERT OR DELETE OR UPDATE ON ediciones_sede2 FOR EACH ROW EXECUTE PROCEDURE _slony_cursos.denyaccess('_slony_cursos');

ALTER TABLE ediciones_sede2 DISABLE TRIGGER _slony_cursos_denyaccess;


--
-- TOC entry 2380 (class 2620 OID 27736)
-- Name: _slony_cursos_denyaccess; Type: TRIGGER; Schema: public; Owner: grupo_admin
--

CREATE TRIGGER _slony_cursos_denyaccess BEFORE INSERT OR DELETE OR UPDATE ON ediciones_sede3 FOR EACH ROW EXECUTE PROCEDURE _slony_cursos.denyaccess('_slony_cursos');

ALTER TABLE ediciones_sede3 DISABLE TRIGGER _slony_cursos_denyaccess;


--
-- TOC entry 2404 (class 2620 OID 27740)
-- Name: _slony_cursos_denyaccess; Type: TRIGGER; Schema: public; Owner: grupo_admin
--

CREATE TRIGGER _slony_cursos_denyaccess BEFORE INSERT OR DELETE OR UPDATE ON participantes FOR EACH ROW EXECUTE PROCEDURE _slony_cursos.denyaccess('_slony_cursos');

ALTER TABLE participantes DISABLE TRIGGER _slony_cursos_denyaccess;


--
-- TOC entry 2413 (class 2620 OID 27744)
-- Name: _slony_cursos_denyaccess; Type: TRIGGER; Schema: public; Owner: grupo_admin
--

CREATE TRIGGER _slony_cursos_denyaccess BEFORE INSERT OR DELETE OR UPDATE ON participantes_sede1 FOR EACH ROW EXECUTE PROCEDURE _slony_cursos.denyaccess('_slony_cursos');

ALTER TABLE participantes_sede1 DISABLE TRIGGER _slony_cursos_denyaccess;


--
-- TOC entry 2422 (class 2620 OID 27748)
-- Name: _slony_cursos_denyaccess; Type: TRIGGER; Schema: public; Owner: grupo_admin
--

CREATE TRIGGER _slony_cursos_denyaccess BEFORE INSERT OR DELETE OR UPDATE ON participantes_sede2 FOR EACH ROW EXECUTE PROCEDURE _slony_cursos.denyaccess('_slony_cursos');

ALTER TABLE participantes_sede2 DISABLE TRIGGER _slony_cursos_denyaccess;


--
-- TOC entry 2431 (class 2620 OID 27752)
-- Name: _slony_cursos_denyaccess; Type: TRIGGER; Schema: public; Owner: grupo_admin
--

CREATE TRIGGER _slony_cursos_denyaccess BEFORE INSERT OR DELETE OR UPDATE ON participantes_sede3 FOR EACH ROW EXECUTE PROCEDURE _slony_cursos.denyaccess('_slony_cursos');

ALTER TABLE participantes_sede3 DISABLE TRIGGER _slony_cursos_denyaccess;


--
-- TOC entry 2352 (class 2620 OID 27756)
-- Name: _slony_cursos_denyaccess; Type: TRIGGER; Schema: public; Owner: grupo_admin
--

CREATE TRIGGER _slony_cursos_denyaccess BEFORE INSERT OR DELETE OR UPDATE ON cursos FOR EACH ROW EXECUTE PROCEDURE _slony_cursos.denyaccess('_slony_cursos');

ALTER TABLE cursos DISABLE TRIGGER _slony_cursos_denyaccess;


--
-- TOC entry 2440 (class 2620 OID 27760)
-- Name: _slony_cursos_denyaccess; Type: TRIGGER; Schema: public; Owner: grupo_admin
--

CREATE TRIGGER _slony_cursos_denyaccess BEFORE INSERT OR DELETE OR UPDATE ON sedes FOR EACH ROW EXECUTE PROCEDURE _slony_cursos.denyaccess('_slony_cursos');

ALTER TABLE sedes DISABLE TRIGGER _slony_cursos_denyaccess;


--
-- TOC entry 2356 (class 2620 OID 27764)
-- Name: _slony_cursos_denyaccess; Type: TRIGGER; Schema: public; Owner: grupo_admin
--

CREATE TRIGGER _slony_cursos_denyaccess BEFORE INSERT OR DELETE OR UPDATE ON departamentos FOR EACH ROW EXECUTE PROCEDURE _slony_cursos.denyaccess('_slony_cursos');

ALTER TABLE departamentos DISABLE TRIGGER _slony_cursos_denyaccess;


--
-- TOC entry 2436 (class 2620 OID 27768)
-- Name: _slony_cursos_denyaccess; Type: TRIGGER; Schema: public; Owner: grupo_admin
--

CREATE TRIGGER _slony_cursos_denyaccess BEFORE INSERT OR DELETE OR UPDATE ON prelaciones FOR EACH ROW EXECUTE PROCEDURE _slony_cursos.denyaccess('_slony_cursos');

ALTER TABLE prelaciones DISABLE TRIGGER _slony_cursos_denyaccess;


--
-- TOC entry 2382 (class 2620 OID 27707)
-- Name: _slony_cursos_logtrigger; Type: TRIGGER; Schema: public; Owner: grupo_admin
--

CREATE TRIGGER _slony_cursos_logtrigger AFTER INSERT OR DELETE OR UPDATE ON empleados FOR EACH ROW EXECUTE PROCEDURE _slony_cursos.logtrigger('_slony_cursos', '1', 'k');


--
-- TOC entry 2386 (class 2620 OID 27711)
-- Name: _slony_cursos_logtrigger; Type: TRIGGER; Schema: public; Owner: grupo_admin
--

CREATE TRIGGER _slony_cursos_logtrigger AFTER INSERT OR DELETE OR UPDATE ON empleados_sede1 FOR EACH ROW EXECUTE PROCEDURE _slony_cursos.logtrigger('_slony_cursos', '2', 'k');


--
-- TOC entry 2390 (class 2620 OID 27715)
-- Name: _slony_cursos_logtrigger; Type: TRIGGER; Schema: public; Owner: grupo_admin
--

CREATE TRIGGER _slony_cursos_logtrigger AFTER INSERT OR DELETE OR UPDATE ON empleados_sede2 FOR EACH ROW EXECUTE PROCEDURE _slony_cursos.logtrigger('_slony_cursos', '3', 'k');


--
-- TOC entry 2394 (class 2620 OID 27719)
-- Name: _slony_cursos_logtrigger; Type: TRIGGER; Schema: public; Owner: grupo_admin
--

CREATE TRIGGER _slony_cursos_logtrigger AFTER INSERT OR DELETE OR UPDATE ON empleados_sede3 FOR EACH ROW EXECUTE PROCEDURE _slony_cursos.logtrigger('_slony_cursos', '4', 'k');


--
-- TOC entry 2360 (class 2620 OID 27723)
-- Name: _slony_cursos_logtrigger; Type: TRIGGER; Schema: public; Owner: grupo_admin
--

CREATE TRIGGER _slony_cursos_logtrigger AFTER INSERT OR DELETE OR UPDATE ON ediciones FOR EACH ROW EXECUTE PROCEDURE _slony_cursos.logtrigger('_slony_cursos', '5', 'kkvvk');


--
-- TOC entry 2366 (class 2620 OID 27727)
-- Name: _slony_cursos_logtrigger; Type: TRIGGER; Schema: public; Owner: grupo_admin
--

CREATE TRIGGER _slony_cursos_logtrigger AFTER INSERT OR DELETE OR UPDATE ON ediciones_sede1 FOR EACH ROW EXECUTE PROCEDURE _slony_cursos.logtrigger('_slony_cursos', '6', 'kkvvk');


--
-- TOC entry 2372 (class 2620 OID 27731)
-- Name: _slony_cursos_logtrigger; Type: TRIGGER; Schema: public; Owner: grupo_admin
--

CREATE TRIGGER _slony_cursos_logtrigger AFTER INSERT OR DELETE OR UPDATE ON ediciones_sede2 FOR EACH ROW EXECUTE PROCEDURE _slony_cursos.logtrigger('_slony_cursos', '7', 'kkvvk');


--
-- TOC entry 2378 (class 2620 OID 27735)
-- Name: _slony_cursos_logtrigger; Type: TRIGGER; Schema: public; Owner: grupo_admin
--

CREATE TRIGGER _slony_cursos_logtrigger AFTER INSERT OR DELETE OR UPDATE ON ediciones_sede3 FOR EACH ROW EXECUTE PROCEDURE _slony_cursos.logtrigger('_slony_cursos', '8', 'kkvvk');


--
-- TOC entry 2402 (class 2620 OID 27739)
-- Name: _slony_cursos_logtrigger; Type: TRIGGER; Schema: public; Owner: grupo_admin
--

CREATE TRIGGER _slony_cursos_logtrigger AFTER INSERT OR DELETE OR UPDATE ON participantes FOR EACH ROW EXECUTE PROCEDURE _slony_cursos.logtrigger('_slony_cursos', '9', 'kkvk');


--
-- TOC entry 2411 (class 2620 OID 27743)
-- Name: _slony_cursos_logtrigger; Type: TRIGGER; Schema: public; Owner: grupo_admin
--

CREATE TRIGGER _slony_cursos_logtrigger AFTER INSERT OR DELETE OR UPDATE ON participantes_sede1 FOR EACH ROW EXECUTE PROCEDURE _slony_cursos.logtrigger('_slony_cursos', '10', 'kkvk');


--
-- TOC entry 2420 (class 2620 OID 27747)
-- Name: _slony_cursos_logtrigger; Type: TRIGGER; Schema: public; Owner: grupo_admin
--

CREATE TRIGGER _slony_cursos_logtrigger AFTER INSERT OR DELETE OR UPDATE ON participantes_sede2 FOR EACH ROW EXECUTE PROCEDURE _slony_cursos.logtrigger('_slony_cursos', '11', 'kkvk');


--
-- TOC entry 2429 (class 2620 OID 27751)
-- Name: _slony_cursos_logtrigger; Type: TRIGGER; Schema: public; Owner: grupo_admin
--

CREATE TRIGGER _slony_cursos_logtrigger AFTER INSERT OR DELETE OR UPDATE ON participantes_sede3 FOR EACH ROW EXECUTE PROCEDURE _slony_cursos.logtrigger('_slony_cursos', '12', 'kkvk');


--
-- TOC entry 2350 (class 2620 OID 27755)
-- Name: _slony_cursos_logtrigger; Type: TRIGGER; Schema: public; Owner: grupo_admin
--

CREATE TRIGGER _slony_cursos_logtrigger AFTER INSERT OR DELETE OR UPDATE ON cursos FOR EACH ROW EXECUTE PROCEDURE _slony_cursos.logtrigger('_slony_cursos', '13', 'k');


--
-- TOC entry 2438 (class 2620 OID 27759)
-- Name: _slony_cursos_logtrigger; Type: TRIGGER; Schema: public; Owner: grupo_admin
--

CREATE TRIGGER _slony_cursos_logtrigger AFTER INSERT OR DELETE OR UPDATE ON sedes FOR EACH ROW EXECUTE PROCEDURE _slony_cursos.logtrigger('_slony_cursos', '14', 'k');


--
-- TOC entry 2354 (class 2620 OID 27763)
-- Name: _slony_cursos_logtrigger; Type: TRIGGER; Schema: public; Owner: grupo_admin
--

CREATE TRIGGER _slony_cursos_logtrigger AFTER INSERT OR DELETE OR UPDATE ON departamentos FOR EACH ROW EXECUTE PROCEDURE _slony_cursos.logtrigger('_slony_cursos', '15', 'k');


--
-- TOC entry 2434 (class 2620 OID 27767)
-- Name: _slony_cursos_logtrigger; Type: TRIGGER; Schema: public; Owner: grupo_admin
--

CREATE TRIGGER _slony_cursos_logtrigger AFTER INSERT OR DELETE OR UPDATE ON prelaciones FOR EACH ROW EXECUTE PROCEDURE _slony_cursos.logtrigger('_slony_cursos', '16', 'kk');


--
-- TOC entry 2385 (class 2620 OID 27710)
-- Name: _slony_cursos_truncatedeny; Type: TRIGGER; Schema: public; Owner: grupo_admin
--

CREATE TRIGGER _slony_cursos_truncatedeny BEFORE TRUNCATE ON empleados FOR EACH STATEMENT EXECUTE PROCEDURE _slony_cursos.deny_truncate();

ALTER TABLE empleados DISABLE TRIGGER _slony_cursos_truncatedeny;


--
-- TOC entry 2389 (class 2620 OID 27714)
-- Name: _slony_cursos_truncatedeny; Type: TRIGGER; Schema: public; Owner: grupo_admin
--

CREATE TRIGGER _slony_cursos_truncatedeny BEFORE TRUNCATE ON empleados_sede1 FOR EACH STATEMENT EXECUTE PROCEDURE _slony_cursos.deny_truncate();

ALTER TABLE empleados_sede1 DISABLE TRIGGER _slony_cursos_truncatedeny;


--
-- TOC entry 2393 (class 2620 OID 27718)
-- Name: _slony_cursos_truncatedeny; Type: TRIGGER; Schema: public; Owner: grupo_admin
--

CREATE TRIGGER _slony_cursos_truncatedeny BEFORE TRUNCATE ON empleados_sede2 FOR EACH STATEMENT EXECUTE PROCEDURE _slony_cursos.deny_truncate();

ALTER TABLE empleados_sede2 DISABLE TRIGGER _slony_cursos_truncatedeny;


--
-- TOC entry 2397 (class 2620 OID 27722)
-- Name: _slony_cursos_truncatedeny; Type: TRIGGER; Schema: public; Owner: grupo_admin
--

CREATE TRIGGER _slony_cursos_truncatedeny BEFORE TRUNCATE ON empleados_sede3 FOR EACH STATEMENT EXECUTE PROCEDURE _slony_cursos.deny_truncate();

ALTER TABLE empleados_sede3 DISABLE TRIGGER _slony_cursos_truncatedeny;


--
-- TOC entry 2363 (class 2620 OID 27726)
-- Name: _slony_cursos_truncatedeny; Type: TRIGGER; Schema: public; Owner: grupo_admin
--

CREATE TRIGGER _slony_cursos_truncatedeny BEFORE TRUNCATE ON ediciones FOR EACH STATEMENT EXECUTE PROCEDURE _slony_cursos.deny_truncate();

ALTER TABLE ediciones DISABLE TRIGGER _slony_cursos_truncatedeny;


--
-- TOC entry 2369 (class 2620 OID 27730)
-- Name: _slony_cursos_truncatedeny; Type: TRIGGER; Schema: public; Owner: grupo_admin
--

CREATE TRIGGER _slony_cursos_truncatedeny BEFORE TRUNCATE ON ediciones_sede1 FOR EACH STATEMENT EXECUTE PROCEDURE _slony_cursos.deny_truncate();

ALTER TABLE ediciones_sede1 DISABLE TRIGGER _slony_cursos_truncatedeny;


--
-- TOC entry 2375 (class 2620 OID 27734)
-- Name: _slony_cursos_truncatedeny; Type: TRIGGER; Schema: public; Owner: grupo_admin
--

CREATE TRIGGER _slony_cursos_truncatedeny BEFORE TRUNCATE ON ediciones_sede2 FOR EACH STATEMENT EXECUTE PROCEDURE _slony_cursos.deny_truncate();

ALTER TABLE ediciones_sede2 DISABLE TRIGGER _slony_cursos_truncatedeny;


--
-- TOC entry 2381 (class 2620 OID 27738)
-- Name: _slony_cursos_truncatedeny; Type: TRIGGER; Schema: public; Owner: grupo_admin
--

CREATE TRIGGER _slony_cursos_truncatedeny BEFORE TRUNCATE ON ediciones_sede3 FOR EACH STATEMENT EXECUTE PROCEDURE _slony_cursos.deny_truncate();

ALTER TABLE ediciones_sede3 DISABLE TRIGGER _slony_cursos_truncatedeny;


--
-- TOC entry 2405 (class 2620 OID 27742)
-- Name: _slony_cursos_truncatedeny; Type: TRIGGER; Schema: public; Owner: grupo_admin
--

CREATE TRIGGER _slony_cursos_truncatedeny BEFORE TRUNCATE ON participantes FOR EACH STATEMENT EXECUTE PROCEDURE _slony_cursos.deny_truncate();

ALTER TABLE participantes DISABLE TRIGGER _slony_cursos_truncatedeny;


--
-- TOC entry 2414 (class 2620 OID 27746)
-- Name: _slony_cursos_truncatedeny; Type: TRIGGER; Schema: public; Owner: grupo_admin
--

CREATE TRIGGER _slony_cursos_truncatedeny BEFORE TRUNCATE ON participantes_sede1 FOR EACH STATEMENT EXECUTE PROCEDURE _slony_cursos.deny_truncate();

ALTER TABLE participantes_sede1 DISABLE TRIGGER _slony_cursos_truncatedeny;


--
-- TOC entry 2423 (class 2620 OID 27750)
-- Name: _slony_cursos_truncatedeny; Type: TRIGGER; Schema: public; Owner: grupo_admin
--

CREATE TRIGGER _slony_cursos_truncatedeny BEFORE TRUNCATE ON participantes_sede2 FOR EACH STATEMENT EXECUTE PROCEDURE _slony_cursos.deny_truncate();

ALTER TABLE participantes_sede2 DISABLE TRIGGER _slony_cursos_truncatedeny;


--
-- TOC entry 2432 (class 2620 OID 27754)
-- Name: _slony_cursos_truncatedeny; Type: TRIGGER; Schema: public; Owner: grupo_admin
--

CREATE TRIGGER _slony_cursos_truncatedeny BEFORE TRUNCATE ON participantes_sede3 FOR EACH STATEMENT EXECUTE PROCEDURE _slony_cursos.deny_truncate();

ALTER TABLE participantes_sede3 DISABLE TRIGGER _slony_cursos_truncatedeny;


--
-- TOC entry 2353 (class 2620 OID 27758)
-- Name: _slony_cursos_truncatedeny; Type: TRIGGER; Schema: public; Owner: grupo_admin
--

CREATE TRIGGER _slony_cursos_truncatedeny BEFORE TRUNCATE ON cursos FOR EACH STATEMENT EXECUTE PROCEDURE _slony_cursos.deny_truncate();

ALTER TABLE cursos DISABLE TRIGGER _slony_cursos_truncatedeny;


--
-- TOC entry 2441 (class 2620 OID 27762)
-- Name: _slony_cursos_truncatedeny; Type: TRIGGER; Schema: public; Owner: grupo_admin
--

CREATE TRIGGER _slony_cursos_truncatedeny BEFORE TRUNCATE ON sedes FOR EACH STATEMENT EXECUTE PROCEDURE _slony_cursos.deny_truncate();

ALTER TABLE sedes DISABLE TRIGGER _slony_cursos_truncatedeny;


--
-- TOC entry 2357 (class 2620 OID 27766)
-- Name: _slony_cursos_truncatedeny; Type: TRIGGER; Schema: public; Owner: grupo_admin
--

CREATE TRIGGER _slony_cursos_truncatedeny BEFORE TRUNCATE ON departamentos FOR EACH STATEMENT EXECUTE PROCEDURE _slony_cursos.deny_truncate();

ALTER TABLE departamentos DISABLE TRIGGER _slony_cursos_truncatedeny;


--
-- TOC entry 2437 (class 2620 OID 27770)
-- Name: _slony_cursos_truncatedeny; Type: TRIGGER; Schema: public; Owner: grupo_admin
--

CREATE TRIGGER _slony_cursos_truncatedeny BEFORE TRUNCATE ON prelaciones FOR EACH STATEMENT EXECUTE PROCEDURE _slony_cursos.deny_truncate();

ALTER TABLE prelaciones DISABLE TRIGGER _slony_cursos_truncatedeny;


--
-- TOC entry 2383 (class 2620 OID 27709)
-- Name: _slony_cursos_truncatetrigger; Type: TRIGGER; Schema: public; Owner: grupo_admin
--

CREATE TRIGGER _slony_cursos_truncatetrigger BEFORE TRUNCATE ON empleados FOR EACH STATEMENT EXECUTE PROCEDURE _slony_cursos.log_truncate('1');


--
-- TOC entry 2387 (class 2620 OID 27713)
-- Name: _slony_cursos_truncatetrigger; Type: TRIGGER; Schema: public; Owner: grupo_admin
--

CREATE TRIGGER _slony_cursos_truncatetrigger BEFORE TRUNCATE ON empleados_sede1 FOR EACH STATEMENT EXECUTE PROCEDURE _slony_cursos.log_truncate('2');


--
-- TOC entry 2391 (class 2620 OID 27717)
-- Name: _slony_cursos_truncatetrigger; Type: TRIGGER; Schema: public; Owner: grupo_admin
--

CREATE TRIGGER _slony_cursos_truncatetrigger BEFORE TRUNCATE ON empleados_sede2 FOR EACH STATEMENT EXECUTE PROCEDURE _slony_cursos.log_truncate('3');


--
-- TOC entry 2395 (class 2620 OID 27721)
-- Name: _slony_cursos_truncatetrigger; Type: TRIGGER; Schema: public; Owner: grupo_admin
--

CREATE TRIGGER _slony_cursos_truncatetrigger BEFORE TRUNCATE ON empleados_sede3 FOR EACH STATEMENT EXECUTE PROCEDURE _slony_cursos.log_truncate('4');


--
-- TOC entry 2361 (class 2620 OID 27725)
-- Name: _slony_cursos_truncatetrigger; Type: TRIGGER; Schema: public; Owner: grupo_admin
--

CREATE TRIGGER _slony_cursos_truncatetrigger BEFORE TRUNCATE ON ediciones FOR EACH STATEMENT EXECUTE PROCEDURE _slony_cursos.log_truncate('5');


--
-- TOC entry 2367 (class 2620 OID 27729)
-- Name: _slony_cursos_truncatetrigger; Type: TRIGGER; Schema: public; Owner: grupo_admin
--

CREATE TRIGGER _slony_cursos_truncatetrigger BEFORE TRUNCATE ON ediciones_sede1 FOR EACH STATEMENT EXECUTE PROCEDURE _slony_cursos.log_truncate('6');


--
-- TOC entry 2373 (class 2620 OID 27733)
-- Name: _slony_cursos_truncatetrigger; Type: TRIGGER; Schema: public; Owner: grupo_admin
--

CREATE TRIGGER _slony_cursos_truncatetrigger BEFORE TRUNCATE ON ediciones_sede2 FOR EACH STATEMENT EXECUTE PROCEDURE _slony_cursos.log_truncate('7');


--
-- TOC entry 2379 (class 2620 OID 27737)
-- Name: _slony_cursos_truncatetrigger; Type: TRIGGER; Schema: public; Owner: grupo_admin
--

CREATE TRIGGER _slony_cursos_truncatetrigger BEFORE TRUNCATE ON ediciones_sede3 FOR EACH STATEMENT EXECUTE PROCEDURE _slony_cursos.log_truncate('8');


--
-- TOC entry 2403 (class 2620 OID 27741)
-- Name: _slony_cursos_truncatetrigger; Type: TRIGGER; Schema: public; Owner: grupo_admin
--

CREATE TRIGGER _slony_cursos_truncatetrigger BEFORE TRUNCATE ON participantes FOR EACH STATEMENT EXECUTE PROCEDURE _slony_cursos.log_truncate('9');


--
-- TOC entry 2412 (class 2620 OID 27745)
-- Name: _slony_cursos_truncatetrigger; Type: TRIGGER; Schema: public; Owner: grupo_admin
--

CREATE TRIGGER _slony_cursos_truncatetrigger BEFORE TRUNCATE ON participantes_sede1 FOR EACH STATEMENT EXECUTE PROCEDURE _slony_cursos.log_truncate('10');


--
-- TOC entry 2421 (class 2620 OID 27749)
-- Name: _slony_cursos_truncatetrigger; Type: TRIGGER; Schema: public; Owner: grupo_admin
--

CREATE TRIGGER _slony_cursos_truncatetrigger BEFORE TRUNCATE ON participantes_sede2 FOR EACH STATEMENT EXECUTE PROCEDURE _slony_cursos.log_truncate('11');


--
-- TOC entry 2430 (class 2620 OID 27753)
-- Name: _slony_cursos_truncatetrigger; Type: TRIGGER; Schema: public; Owner: grupo_admin
--

CREATE TRIGGER _slony_cursos_truncatetrigger BEFORE TRUNCATE ON participantes_sede3 FOR EACH STATEMENT EXECUTE PROCEDURE _slony_cursos.log_truncate('12');


--
-- TOC entry 2351 (class 2620 OID 27757)
-- Name: _slony_cursos_truncatetrigger; Type: TRIGGER; Schema: public; Owner: grupo_admin
--

CREATE TRIGGER _slony_cursos_truncatetrigger BEFORE TRUNCATE ON cursos FOR EACH STATEMENT EXECUTE PROCEDURE _slony_cursos.log_truncate('13');


--
-- TOC entry 2439 (class 2620 OID 27761)
-- Name: _slony_cursos_truncatetrigger; Type: TRIGGER; Schema: public; Owner: grupo_admin
--

CREATE TRIGGER _slony_cursos_truncatetrigger BEFORE TRUNCATE ON sedes FOR EACH STATEMENT EXECUTE PROCEDURE _slony_cursos.log_truncate('14');


--
-- TOC entry 2355 (class 2620 OID 27765)
-- Name: _slony_cursos_truncatetrigger; Type: TRIGGER; Schema: public; Owner: grupo_admin
--

CREATE TRIGGER _slony_cursos_truncatetrigger BEFORE TRUNCATE ON departamentos FOR EACH STATEMENT EXECUTE PROCEDURE _slony_cursos.log_truncate('15');


--
-- TOC entry 2435 (class 2620 OID 27769)
-- Name: _slony_cursos_truncatetrigger; Type: TRIGGER; Schema: public; Owner: grupo_admin
--

CREATE TRIGGER _slony_cursos_truncatetrigger BEFORE TRUNCATE ON prelaciones FOR EACH STATEMENT EXECUTE PROCEDURE _slony_cursos.log_truncate('16');


--
-- TOC entry 2398 (class 2620 OID 27161)
-- Name: antes_insertar_participante_curso_aprobado; Type: TRIGGER; Schema: public; Owner: grupo_admin
--

CREATE TRIGGER antes_insertar_participante_curso_aprobado BEFORE INSERT ON participantes FOR EACH ROW EXECUTE PROCEDURE validar_repeticion_curso_aprobado();


--
-- TOC entry 2406 (class 2620 OID 27162)
-- Name: antes_insertar_participante_curso_aprobado; Type: TRIGGER; Schema: public; Owner: grupo_admin
--

CREATE TRIGGER antes_insertar_participante_curso_aprobado BEFORE INSERT ON participantes_sede1 FOR EACH ROW EXECUTE PROCEDURE validar_repeticion_curso_aprobado();


--
-- TOC entry 2415 (class 2620 OID 27163)
-- Name: antes_insertar_participante_curso_aprobado; Type: TRIGGER; Schema: public; Owner: grupo_admin
--

CREATE TRIGGER antes_insertar_participante_curso_aprobado BEFORE INSERT ON participantes_sede2 FOR EACH ROW EXECUTE PROCEDURE validar_repeticion_curso_aprobado();


--
-- TOC entry 2424 (class 2620 OID 27164)
-- Name: antes_insertar_participante_curso_aprobado; Type: TRIGGER; Schema: public; Owner: grupo_admin
--

CREATE TRIGGER antes_insertar_participante_curso_aprobado BEFORE INSERT ON participantes_sede3 FOR EACH ROW EXECUTE PROCEDURE validar_repeticion_curso_aprobado();


--
-- TOC entry 2433 (class 2620 OID 27165)
-- Name: antes_insertar_prelacion; Type: TRIGGER; Schema: public; Owner: grupo_admin
--

CREATE TRIGGER antes_insertar_prelacion BEFORE INSERT OR UPDATE ON prelaciones FOR EACH ROW EXECUTE PROCEDURE validar_nivel_prelacion();


--
-- TOC entry 2358 (class 2620 OID 27166)
-- Name: antes_insertaromodificar_edicion_instructorarea; Type: TRIGGER; Schema: public; Owner: grupo_admin
--

CREATE TRIGGER antes_insertaromodificar_edicion_instructorarea BEFORE INSERT OR UPDATE ON ediciones FOR EACH ROW EXECUTE PROCEDURE validar_aprobacion_de_area();


--
-- TOC entry 2364 (class 2620 OID 27167)
-- Name: antes_insertaromodificar_edicion_instructorarea; Type: TRIGGER; Schema: public; Owner: grupo_admin
--

CREATE TRIGGER antes_insertaromodificar_edicion_instructorarea BEFORE INSERT OR UPDATE ON ediciones_sede1 FOR EACH ROW EXECUTE PROCEDURE validar_aprobacion_de_area();


--
-- TOC entry 2370 (class 2620 OID 27168)
-- Name: antes_insertaromodificar_edicion_instructorarea; Type: TRIGGER; Schema: public; Owner: grupo_admin
--

CREATE TRIGGER antes_insertaromodificar_edicion_instructorarea BEFORE INSERT OR UPDATE ON ediciones_sede2 FOR EACH ROW EXECUTE PROCEDURE validar_aprobacion_de_area();


--
-- TOC entry 2376 (class 2620 OID 27169)
-- Name: antes_insertaromodificar_edicion_instructorarea; Type: TRIGGER; Schema: public; Owner: grupo_admin
--

CREATE TRIGGER antes_insertaromodificar_edicion_instructorarea BEFORE INSERT OR UPDATE ON ediciones_sede3 FOR EACH ROW EXECUTE PROCEDURE validar_aprobacion_de_area();


--
-- TOC entry 2359 (class 2620 OID 27170)
-- Name: antes_insertaromodificar_instructorparticipante; Type: TRIGGER; Schema: public; Owner: grupo_admin
--

CREATE TRIGGER antes_insertaromodificar_instructorparticipante BEFORE INSERT OR UPDATE ON ediciones FOR EACH ROW EXECUTE PROCEDURE validar_instrcutor_participante();


--
-- TOC entry 2365 (class 2620 OID 27171)
-- Name: antes_insertaromodificar_instructorparticipante; Type: TRIGGER; Schema: public; Owner: grupo_admin
--

CREATE TRIGGER antes_insertaromodificar_instructorparticipante BEFORE INSERT OR UPDATE ON ediciones_sede1 FOR EACH ROW EXECUTE PROCEDURE validar_instrcutor_participante();


--
-- TOC entry 2371 (class 2620 OID 27172)
-- Name: antes_insertaromodificar_instructorparticipante; Type: TRIGGER; Schema: public; Owner: grupo_admin
--

CREATE TRIGGER antes_insertaromodificar_instructorparticipante BEFORE INSERT OR UPDATE ON ediciones_sede2 FOR EACH ROW EXECUTE PROCEDURE validar_instrcutor_participante();


--
-- TOC entry 2377 (class 2620 OID 27173)
-- Name: antes_insertaromodificar_instructorparticipante; Type: TRIGGER; Schema: public; Owner: grupo_admin
--

CREATE TRIGGER antes_insertaromodificar_instructorparticipante BEFORE INSERT OR UPDATE ON ediciones_sede3 FOR EACH ROW EXECUTE PROCEDURE validar_instrcutor_participante();


--
-- TOC entry 2399 (class 2620 OID 27174)
-- Name: antes_insertaromodificar_participante_prelaciones; Type: TRIGGER; Schema: public; Owner: grupo_admin
--

CREATE TRIGGER antes_insertaromodificar_participante_prelaciones BEFORE INSERT OR UPDATE ON participantes FOR EACH ROW EXECUTE PROCEDURE validar_prelaciones();


--
-- TOC entry 2407 (class 2620 OID 27175)
-- Name: antes_insertaromodificar_participante_prelaciones; Type: TRIGGER; Schema: public; Owner: grupo_admin
--

CREATE TRIGGER antes_insertaromodificar_participante_prelaciones BEFORE INSERT OR UPDATE ON participantes_sede1 FOR EACH ROW EXECUTE PROCEDURE validar_prelaciones();


--
-- TOC entry 2416 (class 2620 OID 27176)
-- Name: antes_insertaromodificar_participante_prelaciones; Type: TRIGGER; Schema: public; Owner: grupo_admin
--

CREATE TRIGGER antes_insertaromodificar_participante_prelaciones BEFORE INSERT OR UPDATE ON participantes_sede2 FOR EACH ROW EXECUTE PROCEDURE validar_prelaciones();


--
-- TOC entry 2425 (class 2620 OID 27177)
-- Name: antes_insertaromodificar_participante_prelaciones; Type: TRIGGER; Schema: public; Owner: grupo_admin
--

CREATE TRIGGER antes_insertaromodificar_participante_prelaciones BEFORE INSERT OR UPDATE ON participantes_sede3 FOR EACH ROW EXECUTE PROCEDURE validar_prelaciones();


--
-- TOC entry 2400 (class 2620 OID 27178)
-- Name: antes_insertaromodificar_participanteinstructor; Type: TRIGGER; Schema: public; Owner: grupo_admin
--

CREATE TRIGGER antes_insertaromodificar_participanteinstructor BEFORE INSERT OR UPDATE ON participantes FOR EACH ROW EXECUTE PROCEDURE validar_participante_instrcutor();


--
-- TOC entry 2408 (class 2620 OID 27179)
-- Name: antes_insertaromodificar_participanteinstructor; Type: TRIGGER; Schema: public; Owner: grupo_admin
--

CREATE TRIGGER antes_insertaromodificar_participanteinstructor BEFORE INSERT OR UPDATE ON participantes_sede1 FOR EACH ROW EXECUTE PROCEDURE validar_participante_instrcutor();


--
-- TOC entry 2417 (class 2620 OID 27180)
-- Name: antes_insertaromodificar_participanteinstructor; Type: TRIGGER; Schema: public; Owner: grupo_admin
--

CREATE TRIGGER antes_insertaromodificar_participanteinstructor BEFORE INSERT OR UPDATE ON participantes_sede2 FOR EACH ROW EXECUTE PROCEDURE validar_participante_instrcutor();


--
-- TOC entry 2426 (class 2620 OID 27181)
-- Name: antes_insertaromodificar_participanteinstructor; Type: TRIGGER; Schema: public; Owner: grupo_admin
--

CREATE TRIGGER antes_insertaromodificar_participanteinstructor BEFORE INSERT OR UPDATE ON participantes_sede3 FOR EACH ROW EXECUTE PROCEDURE validar_participante_instrcutor();


--
-- TOC entry 2401 (class 2620 OID 27182)
-- Name: antes_insertaromodificar_participantesede; Type: TRIGGER; Schema: public; Owner: grupo_admin
--

CREATE TRIGGER antes_insertaromodificar_participantesede BEFORE INSERT OR UPDATE ON participantes FOR EACH ROW EXECUTE PROCEDURE validar_participantes_sede();


--
-- TOC entry 2409 (class 2620 OID 27183)
-- Name: antes_insertaromodificar_participantesede; Type: TRIGGER; Schema: public; Owner: grupo_admin
--

CREATE TRIGGER antes_insertaromodificar_participantesede BEFORE INSERT OR UPDATE ON participantes_sede1 FOR EACH ROW EXECUTE PROCEDURE validar_participantes_sede();


--
-- TOC entry 2418 (class 2620 OID 27184)
-- Name: antes_insertaromodificar_participantesede; Type: TRIGGER; Schema: public; Owner: grupo_admin
--

CREATE TRIGGER antes_insertaromodificar_participantesede BEFORE INSERT OR UPDATE ON participantes_sede2 FOR EACH ROW EXECUTE PROCEDURE validar_participantes_sede();


--
-- TOC entry 2427 (class 2620 OID 27185)
-- Name: antes_insertaromodificar_participantesede; Type: TRIGGER; Schema: public; Owner: grupo_admin
--

CREATE TRIGGER antes_insertaromodificar_participantesede BEFORE INSERT OR UPDATE ON participantes_sede3 FOR EACH ROW EXECUTE PROCEDURE validar_participantes_sede();


--
-- TOC entry 2410 (class 2620 OID 27186)
-- Name: antes_insertaromodificar_participantessede1; Type: TRIGGER; Schema: public; Owner: grupo_admin
--

CREATE TRIGGER antes_insertaromodificar_participantessede1 BEFORE INSERT OR DELETE OR UPDATE ON participantes_sede1 FOR EACH ROW EXECUTE PROCEDURE validar_participantes_fragmento_sede1();


--
-- TOC entry 2419 (class 2620 OID 27187)
-- Name: antes_insertaromodificar_participantessede2; Type: TRIGGER; Schema: public; Owner: grupo_admin
--

CREATE TRIGGER antes_insertaromodificar_participantessede2 BEFORE INSERT OR DELETE OR UPDATE ON participantes_sede2 FOR EACH ROW EXECUTE PROCEDURE validar_participantes_fragmento_sede1();


--
-- TOC entry 2428 (class 2620 OID 27188)
-- Name: antes_insertaromodificar_participantessede3; Type: TRIGGER; Schema: public; Owner: grupo_admin
--

CREATE TRIGGER antes_insertaromodificar_participantessede3 BEFORE INSERT OR DELETE OR UPDATE ON participantes_sede3 FOR EACH ROW EXECUTE PROCEDURE validar_participantes_fragmento_sede3();


SET search_path = _slony_cursos, pg_catalog;

--
-- TOC entry 2346 (class 2606 OID 27441)
-- Name: li_origin-no_id-ref; Type: FK CONSTRAINT; Schema: _slony_cursos; Owner: postgres
--

ALTER TABLE ONLY sl_listen
    ADD CONSTRAINT "li_origin-no_id-ref" FOREIGN KEY (li_origin) REFERENCES sl_node(no_id);


--
-- TOC entry 2345 (class 2606 OID 27431)
-- Name: pa_client-no_id-ref; Type: FK CONSTRAINT; Schema: _slony_cursos; Owner: postgres
--

ALTER TABLE ONLY sl_path
    ADD CONSTRAINT "pa_client-no_id-ref" FOREIGN KEY (pa_client) REFERENCES sl_node(no_id);


--
-- TOC entry 2344 (class 2606 OID 27426)
-- Name: pa_server-no_id-ref; Type: FK CONSTRAINT; Schema: _slony_cursos; Owner: postgres
--

ALTER TABLE ONLY sl_path
    ADD CONSTRAINT "pa_server-no_id-ref" FOREIGN KEY (pa_server) REFERENCES sl_node(no_id);


--
-- TOC entry 2343 (class 2606 OID 27413)
-- Name: seq_set-set_id-ref; Type: FK CONSTRAINT; Schema: _slony_cursos; Owner: postgres
--

ALTER TABLE ONLY sl_sequence
    ADD CONSTRAINT "seq_set-set_id-ref" FOREIGN KEY (seq_set) REFERENCES sl_set(set_id);


--
-- TOC entry 2339 (class 2606 OID 27365)
-- Name: set_origin-no_id-ref; Type: FK CONSTRAINT; Schema: _slony_cursos; Owner: postgres
--

ALTER TABLE ONLY sl_set
    ADD CONSTRAINT "set_origin-no_id-ref" FOREIGN KEY (set_origin) REFERENCES sl_node(no_id);


--
-- TOC entry 2347 (class 2606 OID 27446)
-- Name: sl_listen-sl_path-ref; Type: FK CONSTRAINT; Schema: _slony_cursos; Owner: postgres
--

ALTER TABLE ONLY sl_listen
    ADD CONSTRAINT "sl_listen-sl_path-ref" FOREIGN KEY (li_provider, li_receiver) REFERENCES sl_path(pa_server, pa_client);


--
-- TOC entry 2348 (class 2606 OID 27456)
-- Name: sl_subscribe-sl_path-ref; Type: FK CONSTRAINT; Schema: _slony_cursos; Owner: postgres
--

ALTER TABLE ONLY sl_subscribe
    ADD CONSTRAINT "sl_subscribe-sl_path-ref" FOREIGN KEY (sub_provider, sub_receiver) REFERENCES sl_path(pa_server, pa_client);


--
-- TOC entry 2341 (class 2606 OID 27383)
-- Name: ssy_origin-no_id-ref; Type: FK CONSTRAINT; Schema: _slony_cursos; Owner: postgres
--

ALTER TABLE ONLY sl_setsync
    ADD CONSTRAINT "ssy_origin-no_id-ref" FOREIGN KEY (ssy_origin) REFERENCES sl_node(no_id);


--
-- TOC entry 2340 (class 2606 OID 27378)
-- Name: ssy_setid-set_id-ref; Type: FK CONSTRAINT; Schema: _slony_cursos; Owner: postgres
--

ALTER TABLE ONLY sl_setsync
    ADD CONSTRAINT "ssy_setid-set_id-ref" FOREIGN KEY (ssy_setid) REFERENCES sl_set(set_id);


--
-- TOC entry 2349 (class 2606 OID 27461)
-- Name: sub_set-set_id-ref; Type: FK CONSTRAINT; Schema: _slony_cursos; Owner: postgres
--

ALTER TABLE ONLY sl_subscribe
    ADD CONSTRAINT "sub_set-set_id-ref" FOREIGN KEY (sub_set) REFERENCES sl_set(set_id);


--
-- TOC entry 2342 (class 2606 OID 27398)
-- Name: tab_set-set_id-ref; Type: FK CONSTRAINT; Schema: _slony_cursos; Owner: postgres
--

ALTER TABLE ONLY sl_table
    ADD CONSTRAINT "tab_set-set_id-ref" FOREIGN KEY (tab_set) REFERENCES sl_set(set_id);


SET search_path = public, pg_catalog;

--
-- TOC entry 2309 (class 2606 OID 27189)
-- Name: ediciones_cod_curso_fkey; Type: FK CONSTRAINT; Schema: public; Owner: grupo_admin
--

ALTER TABLE ONLY ediciones
    ADD CONSTRAINT ediciones_cod_curso_fkey FOREIGN KEY (cod_curso) REFERENCES cursos(cod_curso) ON UPDATE CASCADE ON DELETE RESTRICT;


--
-- TOC entry 2310 (class 2606 OID 27194)
-- Name: ediciones_cod_sede_fkey; Type: FK CONSTRAINT; Schema: public; Owner: grupo_admin
--

ALTER TABLE ONLY ediciones
    ADD CONSTRAINT ediciones_cod_sede_fkey FOREIGN KEY (cod_sede) REFERENCES sedes(cod_sede) ON UPDATE CASCADE ON DELETE RESTRICT;


--
-- TOC entry 2311 (class 2606 OID 27199)
-- Name: ediciones_instructor_fkey; Type: FK CONSTRAINT; Schema: public; Owner: grupo_admin
--

ALTER TABLE ONLY ediciones
    ADD CONSTRAINT ediciones_instructor_fkey FOREIGN KEY (instructor) REFERENCES empleados(ficha) ON UPDATE CASCADE ON DELETE RESTRICT;


--
-- TOC entry 2312 (class 2606 OID 27204)
-- Name: ediciones_sede1_cod_curso_fkey; Type: FK CONSTRAINT; Schema: public; Owner: grupo_admin
--

ALTER TABLE ONLY ediciones_sede1
    ADD CONSTRAINT ediciones_sede1_cod_curso_fkey FOREIGN KEY (cod_curso) REFERENCES cursos(cod_curso) ON UPDATE CASCADE ON DELETE RESTRICT;


--
-- TOC entry 2313 (class 2606 OID 27209)
-- Name: ediciones_sede1_cod_sede_fkey; Type: FK CONSTRAINT; Schema: public; Owner: grupo_admin
--

ALTER TABLE ONLY ediciones_sede1
    ADD CONSTRAINT ediciones_sede1_cod_sede_fkey FOREIGN KEY (cod_sede) REFERENCES sedes(cod_sede) ON UPDATE CASCADE ON DELETE RESTRICT;


--
-- TOC entry 2314 (class 2606 OID 27214)
-- Name: ediciones_sede1_instructor_fkey; Type: FK CONSTRAINT; Schema: public; Owner: grupo_admin
--

ALTER TABLE ONLY ediciones_sede1
    ADD CONSTRAINT ediciones_sede1_instructor_fkey FOREIGN KEY (instructor) REFERENCES empleados(ficha) ON UPDATE CASCADE ON DELETE RESTRICT;


--
-- TOC entry 2315 (class 2606 OID 27219)
-- Name: ediciones_sede2_cod_curso_fkey; Type: FK CONSTRAINT; Schema: public; Owner: grupo_admin
--

ALTER TABLE ONLY ediciones_sede2
    ADD CONSTRAINT ediciones_sede2_cod_curso_fkey FOREIGN KEY (cod_curso) REFERENCES cursos(cod_curso) ON UPDATE CASCADE ON DELETE RESTRICT;


--
-- TOC entry 2316 (class 2606 OID 27224)
-- Name: ediciones_sede2_cod_sede_fkey; Type: FK CONSTRAINT; Schema: public; Owner: grupo_admin
--

ALTER TABLE ONLY ediciones_sede2
    ADD CONSTRAINT ediciones_sede2_cod_sede_fkey FOREIGN KEY (cod_sede) REFERENCES sedes(cod_sede) ON UPDATE CASCADE ON DELETE RESTRICT;


--
-- TOC entry 2317 (class 2606 OID 27229)
-- Name: ediciones_sede2_instructor_fkey; Type: FK CONSTRAINT; Schema: public; Owner: grupo_admin
--

ALTER TABLE ONLY ediciones_sede2
    ADD CONSTRAINT ediciones_sede2_instructor_fkey FOREIGN KEY (instructor) REFERENCES empleados(ficha) ON UPDATE CASCADE ON DELETE RESTRICT;


--
-- TOC entry 2319 (class 2606 OID 27234)
-- Name: ediciones_sede3_cod_curso_fkey; Type: FK CONSTRAINT; Schema: public; Owner: grupo_admin
--

ALTER TABLE ONLY ediciones_sede3
    ADD CONSTRAINT ediciones_sede3_cod_curso_fkey FOREIGN KEY (cod_curso) REFERENCES cursos(cod_curso) ON UPDATE CASCADE ON DELETE RESTRICT;


--
-- TOC entry 2318 (class 2606 OID 27239)
-- Name: ediciones_sede3_cod_sede_fkey; Type: FK CONSTRAINT; Schema: public; Owner: grupo_admin
--

ALTER TABLE ONLY ediciones_sede3
    ADD CONSTRAINT ediciones_sede3_cod_sede_fkey FOREIGN KEY (cod_sede) REFERENCES sedes(cod_sede) ON UPDATE CASCADE ON DELETE RESTRICT;


--
-- TOC entry 2320 (class 2606 OID 27244)
-- Name: ediciones_sede3_instructor_fkey; Type: FK CONSTRAINT; Schema: public; Owner: grupo_admin
--

ALTER TABLE ONLY ediciones_sede3
    ADD CONSTRAINT ediciones_sede3_instructor_fkey FOREIGN KEY (instructor) REFERENCES empleados(ficha) ON UPDATE CASCADE ON DELETE RESTRICT;


--
-- TOC entry 2321 (class 2606 OID 27249)
-- Name: empleados_cod_dpto_fkey; Type: FK CONSTRAINT; Schema: public; Owner: grupo_admin
--

ALTER TABLE ONLY empleados
    ADD CONSTRAINT empleados_cod_dpto_fkey FOREIGN KEY (cod_dpto) REFERENCES departamentos(cod_dpto) ON UPDATE CASCADE ON DELETE RESTRICT;


--
-- TOC entry 2322 (class 2606 OID 27254)
-- Name: empleados_cod_sede_fkey; Type: FK CONSTRAINT; Schema: public; Owner: grupo_admin
--

ALTER TABLE ONLY empleados
    ADD CONSTRAINT empleados_cod_sede_fkey FOREIGN KEY (cod_sede) REFERENCES sedes(cod_sede) ON UPDATE CASCADE ON DELETE RESTRICT;


--
-- TOC entry 2323 (class 2606 OID 27259)
-- Name: empleados_sede1_cod_dpto_fkey; Type: FK CONSTRAINT; Schema: public; Owner: grupo_admin
--

ALTER TABLE ONLY empleados_sede1
    ADD CONSTRAINT empleados_sede1_cod_dpto_fkey FOREIGN KEY (cod_dpto) REFERENCES departamentos(cod_dpto) ON UPDATE CASCADE ON DELETE RESTRICT;


--
-- TOC entry 2324 (class 2606 OID 27264)
-- Name: empleados_sede1_cod_sede_fkey; Type: FK CONSTRAINT; Schema: public; Owner: grupo_admin
--

ALTER TABLE ONLY empleados_sede1
    ADD CONSTRAINT empleados_sede1_cod_sede_fkey FOREIGN KEY (cod_sede) REFERENCES sedes(cod_sede) ON UPDATE CASCADE ON DELETE RESTRICT;


--
-- TOC entry 2325 (class 2606 OID 27269)
-- Name: empleados_sede2_cod_dpto_fkey; Type: FK CONSTRAINT; Schema: public; Owner: grupo_admin
--

ALTER TABLE ONLY empleados_sede2
    ADD CONSTRAINT empleados_sede2_cod_dpto_fkey FOREIGN KEY (cod_dpto) REFERENCES departamentos(cod_dpto) ON UPDATE CASCADE ON DELETE RESTRICT;


--
-- TOC entry 2326 (class 2606 OID 27274)
-- Name: empleados_sede2_cod_sede_fkey; Type: FK CONSTRAINT; Schema: public; Owner: grupo_admin
--

ALTER TABLE ONLY empleados_sede2
    ADD CONSTRAINT empleados_sede2_cod_sede_fkey FOREIGN KEY (cod_sede) REFERENCES sedes(cod_sede) ON UPDATE CASCADE ON DELETE RESTRICT;


--
-- TOC entry 2327 (class 2606 OID 27279)
-- Name: empleados_sede3_cod_dpto_fkey; Type: FK CONSTRAINT; Schema: public; Owner: grupo_admin
--

ALTER TABLE ONLY empleados_sede3
    ADD CONSTRAINT empleados_sede3_cod_dpto_fkey FOREIGN KEY (cod_dpto) REFERENCES departamentos(cod_dpto) ON UPDATE CASCADE ON DELETE RESTRICT;


--
-- TOC entry 2328 (class 2606 OID 27284)
-- Name: empleados_sede3_cod_sede_fkey; Type: FK CONSTRAINT; Schema: public; Owner: grupo_admin
--

ALTER TABLE ONLY empleados_sede3
    ADD CONSTRAINT empleados_sede3_cod_sede_fkey FOREIGN KEY (cod_sede) REFERENCES sedes(cod_sede) ON UPDATE CASCADE ON DELETE RESTRICT;


--
-- TOC entry 2329 (class 2606 OID 27289)
-- Name: participantes_cod_curso_fkey; Type: FK CONSTRAINT; Schema: public; Owner: grupo_admin
--

ALTER TABLE ONLY participantes
    ADD CONSTRAINT participantes_cod_curso_fkey FOREIGN KEY (cod_curso) REFERENCES cursos(cod_curso) ON UPDATE CASCADE ON DELETE RESTRICT;


--
-- TOC entry 2330 (class 2606 OID 27294)
-- Name: participantes_ficha_fkey; Type: FK CONSTRAINT; Schema: public; Owner: grupo_admin
--

ALTER TABLE ONLY participantes
    ADD CONSTRAINT participantes_ficha_fkey FOREIGN KEY (ficha) REFERENCES empleados(ficha) ON UPDATE CASCADE ON DELETE RESTRICT;


--
-- TOC entry 2331 (class 2606 OID 27299)
-- Name: participantes_sede1_cod_curso_fkey; Type: FK CONSTRAINT; Schema: public; Owner: grupo_admin
--

ALTER TABLE ONLY participantes_sede1
    ADD CONSTRAINT participantes_sede1_cod_curso_fkey FOREIGN KEY (cod_curso) REFERENCES cursos(cod_curso) ON UPDATE CASCADE ON DELETE RESTRICT;


--
-- TOC entry 2332 (class 2606 OID 27304)
-- Name: participantes_sede1_ficha_fkey; Type: FK CONSTRAINT; Schema: public; Owner: grupo_admin
--

ALTER TABLE ONLY participantes_sede1
    ADD CONSTRAINT participantes_sede1_ficha_fkey FOREIGN KEY (ficha) REFERENCES empleados(ficha) ON UPDATE CASCADE ON DELETE RESTRICT;


--
-- TOC entry 2333 (class 2606 OID 27309)
-- Name: participantes_sede2_cod_curso_fkey; Type: FK CONSTRAINT; Schema: public; Owner: grupo_admin
--

ALTER TABLE ONLY participantes_sede2
    ADD CONSTRAINT participantes_sede2_cod_curso_fkey FOREIGN KEY (cod_curso) REFERENCES cursos(cod_curso) ON UPDATE CASCADE ON DELETE RESTRICT;


--
-- TOC entry 2334 (class 2606 OID 27314)
-- Name: participantes_sede2_ficha_fkey; Type: FK CONSTRAINT; Schema: public; Owner: grupo_admin
--

ALTER TABLE ONLY participantes_sede2
    ADD CONSTRAINT participantes_sede2_ficha_fkey FOREIGN KEY (ficha) REFERENCES empleados(ficha) ON UPDATE CASCADE ON DELETE RESTRICT;


--
-- TOC entry 2335 (class 2606 OID 27319)
-- Name: participantes_sede3_cod_curso_fkey; Type: FK CONSTRAINT; Schema: public; Owner: grupo_admin
--

ALTER TABLE ONLY participantes_sede3
    ADD CONSTRAINT participantes_sede3_cod_curso_fkey FOREIGN KEY (cod_curso) REFERENCES cursos(cod_curso) ON UPDATE CASCADE ON DELETE RESTRICT;


--
-- TOC entry 2336 (class 2606 OID 27324)
-- Name: participantes_sede3_ficha_fkey; Type: FK CONSTRAINT; Schema: public; Owner: grupo_admin
--

ALTER TABLE ONLY participantes_sede3
    ADD CONSTRAINT participantes_sede3_ficha_fkey FOREIGN KEY (ficha) REFERENCES empleados(ficha) ON UPDATE CASCADE ON DELETE RESTRICT;


--
-- TOC entry 2337 (class 2606 OID 27329)
-- Name: prelaciones_cod_curso_fkey; Type: FK CONSTRAINT; Schema: public; Owner: grupo_admin
--

ALTER TABLE ONLY prelaciones
    ADD CONSTRAINT prelaciones_cod_curso_fkey FOREIGN KEY (cod_curso) REFERENCES cursos(cod_curso) ON UPDATE CASCADE ON DELETE RESTRICT;


--
-- TOC entry 2338 (class 2606 OID 27334)
-- Name: prelaciones_cod_prelacion_fkey; Type: FK CONSTRAINT; Schema: public; Owner: grupo_admin
--

ALTER TABLE ONLY prelaciones
    ADD CONSTRAINT prelaciones_cod_prelacion_fkey FOREIGN KEY (cod_prelacion) REFERENCES cursos(cod_curso) ON UPDATE CASCADE ON DELETE RESTRICT;


--
-- TOC entry 2630 (class 0 OID 0)
-- Dependencies: 7
-- Name: _slony_cursos; Type: ACL; Schema: -; Owner: postgres
--

REVOKE ALL ON SCHEMA _slony_cursos FROM PUBLIC;
REVOKE ALL ON SCHEMA _slony_cursos FROM postgres;
GRANT ALL ON SCHEMA _slony_cursos TO postgres;
GRANT USAGE ON SCHEMA _slony_cursos TO PUBLIC;


--
-- TOC entry 2632 (class 0 OID 0)
-- Dependencies: 6
-- Name: public; Type: ACL; Schema: -; Owner: postgres
--

REVOKE ALL ON SCHEMA public FROM PUBLIC;
REVOKE ALL ON SCHEMA public FROM postgres;
GRANT ALL ON SCHEMA public TO postgres;
GRANT ALL ON SCHEMA public TO PUBLIC;


SET search_path = _slony_cursos, pg_catalog;

--
-- TOC entry 2667 (class 0 OID 0)
-- Dependencies: 262
-- Name: decode_tgargs(bytea); Type: ACL; Schema: _slony_cursos; Owner: postgres
--

REVOKE ALL ON FUNCTION decode_tgargs(bytea) FROM PUBLIC;
REVOKE ALL ON FUNCTION decode_tgargs(bytea) FROM postgres;
GRANT ALL ON FUNCTION decode_tgargs(bytea) TO postgres;
GRANT ALL ON FUNCTION decode_tgargs(bytea) TO PUBLIC;


--
-- TOC entry 2670 (class 0 OID 0)
-- Dependencies: 253
-- Name: denyaccess(); Type: ACL; Schema: _slony_cursos; Owner: postgres
--

REVOKE ALL ON FUNCTION denyaccess() FROM PUBLIC;
REVOKE ALL ON FUNCTION denyaccess() FROM postgres;
GRANT ALL ON FUNCTION denyaccess() TO postgres;
GRANT ALL ON FUNCTION denyaccess() TO PUBLIC;


--
-- TOC entry 2694 (class 0 OID 0)
-- Dependencies: 255
-- Name: getlocalnodeid(name); Type: ACL; Schema: _slony_cursos; Owner: postgres
--

REVOKE ALL ON FUNCTION getlocalnodeid(p_cluster name) FROM PUBLIC;
REVOKE ALL ON FUNCTION getlocalnodeid(p_cluster name) FROM postgres;
GRANT ALL ON FUNCTION getlocalnodeid(p_cluster name) TO postgres;
GRANT ALL ON FUNCTION getlocalnodeid(p_cluster name) TO PUBLIC;


--
-- TOC entry 2696 (class 0 OID 0)
-- Dependencies: 256
-- Name: getmoduleversion(); Type: ACL; Schema: _slony_cursos; Owner: postgres
--

REVOKE ALL ON FUNCTION getmoduleversion() FROM PUBLIC;
REVOKE ALL ON FUNCTION getmoduleversion() FROM postgres;
GRANT ALL ON FUNCTION getmoduleversion() TO postgres;
GRANT ALL ON FUNCTION getmoduleversion() TO PUBLIC;


--
-- TOC entry 2707 (class 0 OID 0)
-- Dependencies: 233
-- Name: logtrigger(); Type: ACL; Schema: _slony_cursos; Owner: postgres
--

REVOKE ALL ON FUNCTION logtrigger() FROM PUBLIC;
REVOKE ALL ON FUNCTION logtrigger() FROM postgres;
GRANT ALL ON FUNCTION logtrigger() TO postgres;
GRANT ALL ON FUNCTION logtrigger() TO PUBLIC;


SET search_path = public, pg_catalog;

--
-- TOC entry 2908 (class 0 OID 0)
-- Dependencies: 173
-- Name: cursos; Type: ACL; Schema: public; Owner: grupo_admin
--

REVOKE ALL ON TABLE cursos FROM PUBLIC;
REVOKE ALL ON TABLE cursos FROM grupo_admin;
GRANT REFERENCES,TRIGGER,TRUNCATE ON TABLE cursos TO grupo_admin;


--
-- TOC entry 2909 (class 0 OID 0)
-- Dependencies: 174
-- Name: departamentos; Type: ACL; Schema: public; Owner: grupo_admin
--

REVOKE ALL ON TABLE departamentos FROM PUBLIC;
REVOKE ALL ON TABLE departamentos FROM grupo_admin;
GRANT REFERENCES,TRIGGER,TRUNCATE ON TABLE departamentos TO grupo_admin;


--
-- TOC entry 2910 (class 0 OID 0)
-- Dependencies: 175
-- Name: ediciones; Type: ACL; Schema: public; Owner: grupo_admin
--

REVOKE ALL ON TABLE ediciones FROM PUBLIC;
REVOKE ALL ON TABLE ediciones FROM grupo_admin;
GRANT REFERENCES,TRIGGER,TRUNCATE ON TABLE ediciones TO grupo_admin;


--
-- TOC entry 2911 (class 0 OID 0)
-- Dependencies: 176
-- Name: ediciones_sede1; Type: ACL; Schema: public; Owner: grupo_admin
--

REVOKE ALL ON TABLE ediciones_sede1 FROM PUBLIC;
REVOKE ALL ON TABLE ediciones_sede1 FROM grupo_admin;
GRANT REFERENCES,TRIGGER,TRUNCATE ON TABLE ediciones_sede1 TO grupo_admin;


--
-- TOC entry 2912 (class 0 OID 0)
-- Dependencies: 177
-- Name: ediciones_sede2; Type: ACL; Schema: public; Owner: grupo_admin
--

REVOKE ALL ON TABLE ediciones_sede2 FROM PUBLIC;
REVOKE ALL ON TABLE ediciones_sede2 FROM grupo_admin;
GRANT REFERENCES,TRIGGER,TRUNCATE ON TABLE ediciones_sede2 TO grupo_admin;


--
-- TOC entry 2913 (class 0 OID 0)
-- Dependencies: 178
-- Name: ediciones_sede3; Type: ACL; Schema: public; Owner: grupo_admin
--

REVOKE ALL ON TABLE ediciones_sede3 FROM PUBLIC;
REVOKE ALL ON TABLE ediciones_sede3 FROM grupo_admin;
GRANT REFERENCES,TRIGGER,TRUNCATE ON TABLE ediciones_sede3 TO grupo_admin;


--
-- TOC entry 2914 (class 0 OID 0)
-- Dependencies: 179
-- Name: empleados; Type: ACL; Schema: public; Owner: grupo_admin
--

REVOKE ALL ON TABLE empleados FROM PUBLIC;
REVOKE ALL ON TABLE empleados FROM grupo_admin;
GRANT REFERENCES,TRIGGER,TRUNCATE ON TABLE empleados TO grupo_admin;


--
-- TOC entry 2915 (class 0 OID 0)
-- Dependencies: 180
-- Name: empleados_sede1; Type: ACL; Schema: public; Owner: grupo_admin
--

REVOKE ALL ON TABLE empleados_sede1 FROM PUBLIC;
REVOKE ALL ON TABLE empleados_sede1 FROM grupo_admin;
GRANT REFERENCES,TRIGGER,TRUNCATE ON TABLE empleados_sede1 TO grupo_admin;


--
-- TOC entry 2916 (class 0 OID 0)
-- Dependencies: 181
-- Name: empleados_sede2; Type: ACL; Schema: public; Owner: grupo_admin
--

REVOKE ALL ON TABLE empleados_sede2 FROM PUBLIC;
REVOKE ALL ON TABLE empleados_sede2 FROM grupo_admin;
GRANT REFERENCES,TRIGGER,TRUNCATE ON TABLE empleados_sede2 TO grupo_admin;


--
-- TOC entry 2917 (class 0 OID 0)
-- Dependencies: 182
-- Name: empleados_sede3; Type: ACL; Schema: public; Owner: grupo_admin
--

REVOKE ALL ON TABLE empleados_sede3 FROM PUBLIC;
REVOKE ALL ON TABLE empleados_sede3 FROM grupo_admin;
GRANT REFERENCES,TRIGGER,TRUNCATE ON TABLE empleados_sede3 TO grupo_admin;


--
-- TOC entry 2918 (class 0 OID 0)
-- Dependencies: 183
-- Name: participantes; Type: ACL; Schema: public; Owner: grupo_admin
--

REVOKE ALL ON TABLE participantes FROM PUBLIC;
REVOKE ALL ON TABLE participantes FROM grupo_admin;
GRANT REFERENCES,TRIGGER,TRUNCATE ON TABLE participantes TO grupo_admin;


--
-- TOC entry 2919 (class 0 OID 0)
-- Dependencies: 184
-- Name: participantes_sede1; Type: ACL; Schema: public; Owner: grupo_admin
--

REVOKE ALL ON TABLE participantes_sede1 FROM PUBLIC;
REVOKE ALL ON TABLE participantes_sede1 FROM grupo_admin;
GRANT REFERENCES,TRIGGER,TRUNCATE ON TABLE participantes_sede1 TO grupo_admin;


--
-- TOC entry 2920 (class 0 OID 0)
-- Dependencies: 185
-- Name: participantes_sede2; Type: ACL; Schema: public; Owner: grupo_admin
--

REVOKE ALL ON TABLE participantes_sede2 FROM PUBLIC;
REVOKE ALL ON TABLE participantes_sede2 FROM grupo_admin;
GRANT REFERENCES,TRIGGER,TRUNCATE ON TABLE participantes_sede2 TO grupo_admin;


--
-- TOC entry 2921 (class 0 OID 0)
-- Dependencies: 186
-- Name: participantes_sede3; Type: ACL; Schema: public; Owner: grupo_admin
--

REVOKE ALL ON TABLE participantes_sede3 FROM PUBLIC;
REVOKE ALL ON TABLE participantes_sede3 FROM grupo_admin;
GRANT REFERENCES,TRIGGER,TRUNCATE ON TABLE participantes_sede3 TO grupo_admin;


--
-- TOC entry 2922 (class 0 OID 0)
-- Dependencies: 187
-- Name: prelaciones; Type: ACL; Schema: public; Owner: grupo_admin
--

REVOKE ALL ON TABLE prelaciones FROM PUBLIC;
REVOKE ALL ON TABLE prelaciones FROM grupo_admin;
GRANT REFERENCES,TRIGGER,TRUNCATE ON TABLE prelaciones TO grupo_admin;


--
-- TOC entry 2923 (class 0 OID 0)
-- Dependencies: 188
-- Name: sedes; Type: ACL; Schema: public; Owner: grupo_admin
--

REVOKE ALL ON TABLE sedes FROM PUBLIC;
REVOKE ALL ON TABLE sedes FROM grupo_admin;
GRANT REFERENCES,TRIGGER,TRUNCATE ON TABLE sedes TO grupo_admin;


-- Completed on 2015-06-19 21:23:21

--
-- PostgreSQL database dump complete
--

