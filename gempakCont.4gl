{  Program Name   : gempakCont.4gi
   Author         : hcsjbk
   Date           : 09/06/2016
   Description    : To apply contribution after medical reprice

SEQ     DATE        WHO     REMARK
---     ----------  -----   --------------------------------------------
}
DATABASE lifedb

GLOBALS
  DEFINE gr_mcontrol        RECORD LIKE mcontrol.*
  DEFINE gr_mulendor        RECORD LIKE mulendor.*
  DEFINE gr_xreceipt1       RECORD LIKE xreceipt1.*
  DEFINE gr_xreceipt2       RECORD LIKE xreceipt2.*
  DEFINE gr_mpolicy         RECORD LIKE mpolicy.*
  DEFINE g_stmt             CHAR(100)
  DEFINE g_suspamt          LIKE msuspmas.amtappl
  DEFINE g_actual_suspamt   LIKE msuspmas.amtappl
  DEFINE g_noinst           SMALLINT
  DEFINE g_ncdd             DATE
  DEFINE g_psa              LIKE mulpola.calprm
  DEFINE g_amtrcvd          LIKE xreceipt1.amtrcvd
  DEFINE g_inst_psa         LIKE mulpola.calprm
  define g_polno integer  #--minide
END GLOBALS

MAIN

  WHENEVER ERROR CALL uerror
  DEFER INTERRUPT

  CALL STARTLOG("gempakCont.err")

  SELECT * INTO gr_mcontrol.*
  FROM mcontrol
  let g_polno = ARG_VAL(1)  #--minide
  let gr_mcontrol.date_ext=ARG_VAL(2)  #--minide

  CALL crt_temp()

  LET g_stmt = "echo 'start - ", DATE, " ", TIME, " ' >> gempakCont.log"
  RUN g_stmt

  DISPLAY "Applying contribution for Gempak reprice case" AT 10,11

  DECLARE mulendor_cur1 CURSOR FOR
  SELECT *
  FROM mulendor
  WHERE opid = "MEDRep"
  and polno=g_polno #--minide
  ORDER BY polno

  INITIALIZE gr_mulendor.* TO NULL

  FOREACH mulendor_cur1 INTO gr_mulendor.*
     DISPLAY "Processing certificate : ",gr_mulendor.polno AT 11,11
     IF check_money() THEN CONTINUE FOREACH END IF
     CALL get_details()
     CALL gen_xrc1()
     CALL gen_xrc2()
  END FOREACH

  CALL ins_xrc(3)

  DISPLAY "" AT 10,11
  DISPLAY "Process complete" AT 10,11

  LET g_stmt = "echo 'end - ", DATE, " ", TIME, " ' >> gempakCont.log"
  RUN g_stmt
END MAIN

FUNCTION crt_temp()
#*********************************************************************
  SELECT * FROM xreceipt1 WHERE 1=2 INTO TEMP temp_xreceipt1
  SELECT * FROM xreceipt2 WHERE 1=2 INTO TEMP temp_xreceipt2
END FUNCTION

FUNCTION check_money()
#*********************************************************************
  LET g_suspamt = 0
  LET g_actual_suspamt = 0 
  LET g_noinst = 0
  LET g_amtrcvd = 0

  SELECT SUM(amtappl) INTO g_suspamt
  FROM msuspmas
  WHERE refno = gr_mulendor.polno
  AND tcode = "905"
  AND remark = "MEDReprice"

  SELECT SUM(amtappl) INTO g_actual_suspamt
  FROM msuspmas
  WHERE refno = gr_mulendor.polno

  IF g_actual_suspamt IS NULL THEN
     LET g_actual_suspamt = 0
  END IF

  IF g_suspamt IS NULL OR g_suspamt = 0 THEN
     RETURN TRUE
  ELSE
     IF g_suspamt >= g_actual_suspamt THEN
        LET g_suspamt = g_actual_suspamt
     END IF

     LET g_noinst = g_suspamt / gr_mulendor.tinsprm
     IF g_noinst < 1 THEN
        RETURN TRUE
     ELSE
        LET g_amtrcvd = gr_mulendor.tinsprm * g_noinst
        RETURN FALSE
     END IF
  END IF
END FUNCTION

FUNCTION get_details()
#*********************************************************************
  DEFINE f_mth              SMALLINT
  DEFINE f_yr               SMALLINT
  DEFINE f_inst             SMALLINT
  DEFINE f_date             CHAR(8)
  DEFINE f_inst_no_psa      LIKE mulpola.calprm
  DEFINE f_inst_all         LIKE mulpola.calprm

  INITIALIZE gr_mpolicy.* TO NULL
  LET f_date = NULL
  LET g_psa = 0
  LET f_inst_all = 0
  LET g_inst_psa = 0
  LET f_inst_no_psa = 0

  -- cert details
  SELECT * INTO gr_mpolicy.*
  FROM mpolicy
  WHERE polno = gr_mulendor.polno

  LET f_mth = MONTH(gr_mpolicy.npdd)
  LET f_yr = YEAR(gr_mpolicy.npdd)
  LET f_inst = g_noinst

  WHILE f_inst > 0
     LET f_mth = f_mth + 1
     IF f_mth = 13 THEN
        LET f_yr = f_yr + 1
    LET f_mth = 1
     END IF

     LET f_inst = f_inst - 1
  END WHILE

  -- next contribution duedt
  LET f_date = "01",f_mth USING "&&",f_yr USING "&&&&"
  LET g_ncdd = f_date

  -- psa all contri amount
  SELECT SUM(nannprem) INTO f_inst_all
  FROM mulendcv
  WHERE polno = gr_mulendor.polno

  CASE gr_mpolicy.frqpay
    WHEN "M" LET f_inst_all = f_inst_all/12
    WHEN "Q" LET f_inst_all = f_inst_all/4
    WHEN "H" LET f_inst_all = f_inst_all/2
    WHEN "A" LET f_inst_all = f_inst_all/1
  
    OTHERWISE
  END CASE

  -- psa all contri without saver
  SELECT SUM(calprm) INTO f_inst_no_psa
  FROM mulpola
  WHERE polno = gr_mulendor.polno
  AND covercd MATCHES "*PSA"

  IF f_inst_all IS NULL THEN
     LET f_inst_all = 0
  END IF

  IF f_inst_no_psa IS NULL THEN
     LET f_inst_no_psa = 0
  END IF

  -- psa contri (include tax if any)
  IF f_inst_all <> 0 AND gr_mulendor.tinsprm <> 0 THEN
      LET g_inst_psa = f_inst_no_psa / f_inst_all * gr_mulendor.tinsprm
  ELSE
      LET g_inst_psa = 0
  END IF

END FUNCTION

FUNCTION gen_xrc1()
#*********************************************************************
  INITIALIZE gr_xreceipt1.* TO NULL

  LET gr_xreceipt1.pfix = 1
  LET gr_xreceipt1.polno = gr_mulendor.polno
  LET gr_xreceipt1.sno = 1
  LET gr_xreceipt1.source = 1
  LET gr_xreceipt1.mode1 = 3
  LET gr_xreceipt1.colldt = gr_mulendor.trndt
  LET gr_xreceipt1.rcttyp = "N"
  LET gr_xreceipt1.rctno = 0
  LET gr_xreceipt1.rctdt = gr_mulendor.trndt
  LET gr_xreceipt1.rctname = gr_mpolicy.laname
  LET gr_xreceipt1.icnos = gr_mpolicy.laicno
  LET gr_xreceipt1.nxtprmdue = g_ncdd
  LET gr_xreceipt1.remark = "MEDReprice"
  LET gr_xreceipt1.prnflg = 0
  LET gr_xreceipt1.pstflg = 0
  LET gr_xreceipt1.amtrcvd = 0
  LET gr_xreceipt1.noinst = g_noinst
  LET gr_xreceipt1.opid = "MEDRep"
  LET gr_xreceipt1.collbk1 = "SCB"
  LET gr_xreceipt1.collbk2 = "JAMP"
  LET gr_xreceipt1.chqamt = 0
  LET gr_xreceipt1.cashamt = g_amtrcvd
  LET gr_xreceipt1.stats = gr_mpolicy.stats
  LET gr_xreceipt1.bps = "N"
  LET gr_xreceipt1.date_ext = gr_mulendor.trndt

  CALL ins_xrc(1)
END FUNCTION

FUNCTION gen_xrc2()
#*********************************************************************
  DEFINE f_mth          SMALLINT
  DEFINE f_yr           SMALLINT
  DEFINE f_inst         SMALLINT
  DEFINE f_appncd       SMALLINT
  DEFINE f_date         CHAR(8)
  DEFINE f_duedt        DATE

  INITIALIZE gr_xreceipt2.* TO NULL
  LET f_inst = g_noinst

  -- Take out amount from suspense (905)
  LET gr_xreceipt2.pfix = gr_xreceipt1.pfix
  LET gr_xreceipt2.polno = gr_xreceipt1.polno
  LET gr_xreceipt2.sno = gr_xreceipt1.sno
  LET gr_xreceipt2.appncd = "905"
  LET gr_xreceipt2.amt = g_amtrcvd * -1
  LET gr_xreceipt2.date_ext = gr_xreceipt1.date_ext
  LET gr_xreceipt2.opid = "MEDRep"
  LET gr_xreceipt2.remark = "MEDReprice"
  LET gr_xreceipt2.mode1 = gr_xreceipt1.mode1
  CALL ins_xrc(2)

  LET f_mth = MONTH(gr_mpolicy.npdd)
  LET f_yr = YEAR(gr_mpolicy.npdd)

  WHILE f_inst > 0
     INITIALIZE gr_xreceipt2.* TO NULL
     LET f_inst = f_inst - 1

     IF f_mth = 13 THEN
        LET f_yr = f_yr + 1
        LET f_mth = 1
     END IF

     LET f_date = "01",f_mth USING "&&",f_yr USING "&&&&"
     LET f_duedt = f_date

     IF (YEAR(gr_mpolicy.dtincp)+1) > YEAR(f_duedt) THEN
        LET f_appncd = "362"
     ELSE
        LET f_appncd = "363"
     END IF

     LET gr_xreceipt2.pfix = gr_xreceipt1.pfix
     LET gr_xreceipt2.polno = gr_xreceipt1.polno
     LET gr_xreceipt2.sno = gr_xreceipt1.sno
     LET gr_xreceipt2.appncd = f_appncd
     LET gr_xreceipt2.duedt = f_duedt
     LET gr_xreceipt2.amt = gr_mulendor.tinsprm - g_inst_psa
     LET gr_xreceipt2.date_ext = gr_xreceipt1.date_ext
     LET gr_xreceipt2.opid = "MEDRep"
     LET gr_xreceipt2.remark = "MEDReprice"
     LET gr_xreceipt2.mode1 = gr_xreceipt1.mode1

     CALL ins_xrc(2)

     IF g_inst_psa IS NOT NULL AND g_inst_psa > 0 THEN
        LET gr_xreceipt2.pfix = gr_xreceipt1.pfix
        LET gr_xreceipt2.polno = gr_xreceipt1.polno
        LET gr_xreceipt2.sno = gr_xreceipt1.sno
        LET gr_xreceipt2.appncd = "367"
        LET gr_xreceipt2.duedt = f_duedt
        LET gr_xreceipt2.amt = g_inst_psa
        LET gr_xreceipt2.date_ext = gr_xreceipt1.date_ext
        LET gr_xreceipt2.opid = "MEDRep"
        LET gr_xreceipt2.remark = "MEDReprice"
        LET gr_xreceipt2.mode1 = gr_xreceipt1.mode1

    CALL ins_xrc(2)
     END IF

     LET f_mth = f_mth + 1
  END WHILE
END FUNCTION


FUNCTION ins_xrc(f_opt)
#*********************************************************************
  DEFINE f_opt          SMALLINT

  CASE f_opt
    WHEN 1
      INSERT INTO temp_xreceipt1 VALUES (gr_xreceipt1.*)
    WHEN 2
      INSERT INTO temp_xreceipt2 VALUES (gr_xreceipt2.*)
    WHEN 3
      INSERT INTO xreceipt1 SELECT * FROM temp_xreceipt1
      INSERT INTO xreceipt2 SELECT * FROM temp_xreceipt2
  END CASE
END FUNCTION


FUNCTION uerror ()
#*********************************************************************
  DEFINE f_prompt       CHAR (1)

  LET f_prompt = "N"
  WHILE f_prompt <> "Y"
     PROMPT "ERROR gempakCont.err: PLEASE CONTACT IT IMMEDIATELY ...(gempakCont)"
        FOR f_prompt
  END WHILE
  EXIT PROGRAM
END FUNCTION

