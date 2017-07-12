             {Program Name ........: uend2x20.4gl
 Written by ..........: Lee Wen Bing
 Description..........: Update PAA endorsement file to work file 
 			extract data from work file (wfmpolicy, wfmpolicya)
			and then update to daily file (xpolicy,
                        xpolicys, xpolicya,xpolicyn,xpolicym,xpolicyv,xpolicyr)

#------------------------------------------------------------------------------
 Seq  Date       Who   Remark
#------------------------------------------------------------------------------
  1  07/06/1999  LPK   To include 2 additional fields in xpolicyt :-Pru-managed
                       fund & bumi fund
  2  06/06/2000  LPK   To take-out the insert xpolicyt function - not used 
  3  29/08/2000  LLK   To unhashed the calculation part for age.
  4  03/05/2001  LTS   Program bomb not unique - put in another criteria.
  5  09/07/2001  CST   Generate Report  For exceptional record.
  6  28/08/2002  TYP   Add in addition tables.
  7  20/05/2003  LLK   W20030066
                       Add in Spouse Nationality field
  8  02/06/2003  LLK   TPR-END-39
  			LA MArital should not be updated to "M"
  9  13/06/2003  LLK   TPR-END-47
  		       Avoid Duplicate code for endorsement

  10 29/09/2003  EKS   L20030803 
                       Set xpolicy.on_movdt = mulendor.effdt when onmovcd = 11
  11 18/02/2004  YMH   Add the statements to calculate the age for DOB changed 
                       in endorsement screen.
  12 10/03/2004  EKS   Set xpolicy.on_movdt to wfmpolicy effdt when onmovcd = 11 
  13 09/08/2004  LLK   APLIP PROJECT-to check SAM with tsendwfypolicyo.indsam field
  14 01/06/2005  ych   ILP Project - cater for new poltype/covercd.
		       Field added in hulendor and mulendor 
  15 15/06/2005  ych   Chanege condition for changing Servicing Agent for 
 			5 Series
  16 05/09/2005  nsc   For existing rider, mpolcvagt.mths and mpolcvagt.npaldt 
  			should follow existing record
  17 01/10/2005  Davis Foreign Fund Project (5PAF)
  18 05/12/2005  nsc   SRF L200508578 - EXC and PDB not capture in endorsement
  19 25/10/2005  Davis SRF 200508648 - Rectify servicing agent rules for 4PAA
  20 07/04/2006  hcsckt Takaful Project
  21 13/06/2006	 hcsgcl	Takaful Project - Auto change of servicing agent only apply for endorsement
  					  on cover code A only
  22 10/07/2009  iamgss Takaful Impian2 Project - To cater for Impian product
  23 01/12/2009  iammsv MMC2 upgrade 
  24 28/01/2010  iammsv MMC2 upgrade(commission)
  25 01/02/2010  hcswdk 6PAP Phase2:  cater for 6PAP, 6PAPJ, 6PAE,
                        6PAPC Phase2: cater for 6PAPC, 6PAPJC, 6PAEC
  26 21/05/2010  hcsock MMC2 Permanent Solution
  27 11/08/2010  hcsock MMC2 Permananet Solution - get oragt1 and oragt2 from mpolcvagt
  28 19/08/2010  hcsock MMC2 Permanent Solution - defect - loadid is null
  29 18/10/2010  hcsock Takaful Heatlh - New table tsendpromo for Promotion Bonus 
  30 27/10/2010  hcsock Takaful Health (upgrade) - defect - 117707 
                        No cater for rider 5UPTH with indicator N or NULL v 
  31 01/11/2010  hcsock	Takaful Heatlh - to retain 2 records in mmc2upgrd
  32 02/02/2011  hcsazi Takaful Heatlh UB
  33 27/05/2011  hcsmi  Partial Lapse CR15 : Add charges during lapse period
  34 18/08/2011  hcsnas RFC#48753: add initialization variable 
  35 17/01/2012  iammfm Fix Wrong agent
  36 14/5/2012   hcsasr Adding jp2 features while endorsment, data transfer 
  37 14/5/2012   hcsasr Include the 7 series (UPTHE/UPTHD) riders codes for pmm 
  38 18/5/2012   hcsasr Transferring datas from history table to endors tables 
  39 29/06/2012  hcsazi To exculde JL2 details if wfmopicy3 dont have jl2 details
  40 08/03/2013  hcsasr CR#35
  41 21/10/2013  iamarh  RFC#64130 Endorsement does not captured UWR change of status
  42 24/04/2014  hcsmi  MMC re-pricing,1) make mpolcvagt inherit prev paid mths, 
                                       2) cater for double riders cases
  43 09/07/2014  hcszho MMC repricing - add more condition to double mmc case checking                                     
  44 28/10/2014  hcslyj RFC#72928 - UmmahLink Phase 3b - Endorsement - religion change
  45 18/12/2014  hcsmi  GST project - to pass tsnbzgst -> xsnbzgst
  46 10/02/2015  hcslyj GST project - set xsnbzgst.date_ext as current date
  47 19/08/2015  hcsgjh GCP project - to cater for new table mulendcv3s to hulendcv3s
  48 09/09/2015  hcslyj GCP project - fix SIT issue
  49 12/08/2015  hcsgjh GCP endorsement phase 1a - to cater for new table tendtad to xsendta
  50 15/11/2015  hcslyj GCP endorsement phase 1a - SIT defect fixing
  51 13/01/2015	 hcsssn	RFC#71744 - Partial Lapse & Auto Drop Enhancement (7 Series)
  52 29/02/2016  hcsnrh New CI definition
  53 03/03/2016  hcslyj GCP phase1c - fixing wrong data update in table mpolcvagt for MTA payment reversal
  54 27/06/2016  hcszhk Gempak ph2
#******************************************************************************
}

DATABASE lifedb 

GLOBALS
DEFINE	p_wfpol		RECORD LIKE wfmpolicy.*,
	p_wfmpolicy	RECORD LIKE wfmpolicy.*,
	g_wfulpola	RECORD LIKE wfmulpola.*,
        p_mendreqh      RECORD LIKE mendreqh.*,
        p_mendreqd      RECORD LIKE mendreqd.*, 
	p_acmth		CHAR(8),
         p_hwfpol    RECORD LIKE hwfmpolicy.*,
         p_wfpola    RECORD LIKE wfmpolicya.*,
         p_wfpolt    RECORD LIKE wfmpolicyt.*,
         p_mpol      RECORD LIKE mpolicy.*,
         p_mpols     RECORD LIKE mpolicys.*,
         p_mpolm     RECORD LIKE mpolicym.*,
         p_mpoln     RECORD LIKE mpolicyn.*,
         p_mpolns    RECORD LIKE mpolicyns.*,
	g_mulpola	RECORD LIKE mulpola.*,
         p_mpolv     RECORD LIKE mpolicyv.*,
	g_endor		RECORD LIKE mulendor.*,
	g_endcv		RECORD LIKE mulendcv.*,
	g_endcv3s	RECORD LIKE mulendcv3s.*,     -- seq 47 --
	r_spage, v_year, v_month,
         exi, r_laage, v_year1, v_month1 SMALLINT,
         v_enddt         DATE,
         g_comctga CHAR(1),
         p_planchg       CHAR(1),
	g_date_ext	LIKE mcontrol.date_ext,
	v_ivydate        DATE,     -- seq 10 --
         v_ofdate        DATE,
         v_effdt         DATE,
         v_chgmemo       CHAR(18),
  	g_company	 LIKE mvlctrl.company2,
--seq 36--
        p_wfmpoln2       RECORD LIKE wfmpolicyn2.*,
        p_wfmpol3        RECORD LIKE wfmpolicy3.*
--seq 36*--

  DEFINE p_jlind   CHAR(1),
         p_dtincp  DATE

-- seq 6
  DEFINE p_msendmisc       RECORD LIKE msendmisc.*
  DEFINE p_msendoutrmk     RECORD LIKE msendoutrmk.*
  DEFINE p_msendtextrmk    RECORD LIKE msendtextrmk.*
  DEFINE lt_stm            CHAR(500) -- seq 32 --
  define g_polno integer  --kit--
END GLOBALS

MAIN
    SELECT company2 into g_company from mvlctrl
    
    DEFER INTERRUPT

    CALL STARTLOG ("/u/errlog/djs.log")
    WHENEVER ERROR CALL uerror

    DISPLAY "(uend2x20) 080799 Updating to daily file (ENDORSEMENT)"
    SELECT date_ext INTO g_date_ext FROM mcontrol 
    let g_polno = ARG_VAL(1) --kit--
    let g_date_ext = ARG_VAL(2) --kit
    #CALL pro_wfm()   -- seq 5 --
    CALL pro_xpol() 
    CALL create_tmp() --seq 26--
    CALL trans_endor()
  #  CALL trans_mulendor2()    --seq 36--
    CALL trans_mendxtra()     --seq 38--
    CALL chk_mmc2upgrd() --seq 31-- 
    CALL f_unload()   --seq 26-- 
    CALL ub_prosess() -- seq 32 --
    DISPLAY "(uend2x20) Updating Completed."
END MAIN

FUNCTION pro_wfm()
DEFINE	m_factor	SMALLINT

    DECLARE c_endor CURSOR FOR
	SELECT * FROM mulendor 
	    WHERE postflg = 'A'
	    and polno = g_polno --kit--
   
#    START REPORT   err_endos TO "/u/opr2/dlyrpt/uend2x20z.rpt" --kit
START REPORT   err_endos TO "uend2x20z.rpt"--kit
    FOREACH c_endor INTO g_endor.*

        SELECT *
        INTO p_wfmpolicy.*
        FROM wfmpolicy 
        WHERE  polno = g_endor.polno  
     
        IF STATUS <> NOTFOUND THEN
           OUTPUT TO REPORT err_endos (g_endor.polno, p_wfmpolicy.movcd,
                                       p_wfmpolicy.ofmovcd)
           CONTINUE FOREACH
        END IF        --  seq 5 --

	INITIALIZE p_wfpol.* TO NULL
	LET p_wfpol.opid     = g_endor.opid
	LET p_wfpol.polno    = g_endor.polno
	LET p_wfpol.poltype  = g_endor.poltype		#'4PAA'
	LET p_wfpol.movcd    = g_endor.movcd
	LET p_wfpol.stats    = g_endor.stat1
	LET p_wfpol.dtincp   = g_endor.dtincp
	LET p_wfpol.endcd    = g_endor.endtno

	IF g_endor.movcd = '11' THEN
	        LET p_wfpol.npd      = g_endor.measdt
		LET p_wfpol.effdt    = g_endor.measdt
	ELSE
	        LET p_wfpol.npd      = g_endor.npd
		LET p_wfpol.effdt    = g_endor.effdt
	END IF
	LET p_wfpol.lasex    = g_endor.nsexla
	LET p_wfpol.term     = g_endor.term
	LET p_wfpol.sumas    = g_endor.sumas
	LET p_wfpol.tinstprm = g_endor.tinsprm
	LET p_wfpol.tannprm  = g_endor.tannprm
	LET p_wfpol.frqpay   = g_endor.nfreq
	LET p_wfpol.modpay   = g_endor.nmode
	LET p_wfpol.movdt    = g_endor.movdt
	LET p_wfpol.annex1   = g_endor.annex1
	LET p_wfpol.annex2   = g_endor.annex2
	LET p_wfpol.annex3   = g_endor.annex3
	LET p_wfpol.annex4   = g_endor.annex4
	LET p_wfpol.annex5   = g_endor.annex5
	LET p_wfpol.annex6   = g_endor.annex6
	LET p_wfpol.annex7   = g_endor.annex7
	LET p_wfpol.annex8   = g_endor.annex8
	LET p_wfpol.annex9   = g_endor.annex9
	LET p_wfpol.annex10  = g_endor.annex10
	LET p_wfpol.annex11  = g_endor.annex11
	LET p_wfpol.annex12  = g_endor.annex12
	LET p_wfpol.annex13  = g_endor.annex13
	LET p_wfpol.annex14  = g_endor.annex14
	LET p_wfpol.annex15  = g_endor.annex15
	LET p_wfpol.annex16  = g_endor.annex16
	LET p_wfpol.annex17  = g_endor.annex17
	LET p_wfpol.annex18  = g_endor.annex18
	LET p_wfpol.annex19  = g_endor.annex19
	LET p_wfpol.annex20  = g_endor.annex20
	LET p_wfpol.laocpcls = g_endor.nocclsla
	LET p_wfpol.ladob    = g_endor.ndobla
	LET p_wfpol.laocp    = g_endor.noccpla
	LET p_wfpol.lasmind  = g_endor.nsmklaid
	LET p_wfpol.uwla     = g_endor.uwla
	LET p_wfpol.upldind  = 'E'

         #{ laage will not change forever - according to sally Chong.	
							-- seq 3 --
        IF g_endor.dobchid = "Y" THEN
    	    LET v_year1 = YEAR(g_endor.dtincp) - YEAR(g_endor.ndobla)
    	    LET v_month1 = MONTH(g_endor.dtincp) - MONTH(g_endor.ndobla)
    	    IF v_month1 <= 0 THEN
        	    LET r_laage = v_year1
    	    ELSE
        	    LET r_laage = v_year1 + 1
    	    END IF
    	    IF v_month1 = 0 AND v_year1 = 0 THEN
         	    LET r_laage = 1
    	    END IF
            LET p_wfpol.laage = r_laage
            LET p_mpol.laage  = r_laage
         END IF
	#}						-- seq 3 --
#spouse policy
	IF g_endor.spouseid = 'Y' AND g_endor.spname IS NOT NULL THEN
    	    LET v_year = YEAR(g_endor.dtincp) - YEAR(g_endor.ndobsp) 
    	    LET v_month = MONTH(g_endor.dtincp) - MONTH(g_endor.ndobsp)
    	    IF v_month <= 0 THEN
        	    LET r_spage = v_year
    	    ELSE
        	    LET r_spage = v_year + 1
    	    END IF
    	    IF v_month = 0 AND v_year = 0 THEN
         	    LET r_spage = 1
    	    END IF
	    LET p_wfpol.jlind     = 'S'
	    LET p_wfpol.jlname    = g_endor.spname
            LET p_wfpol.jlsex     = g_endor.nsexsp
            LET p_wfpol.jldob     = g_endor.ndobsp
            LET p_wfpol.jlocp     = g_endor.noccpsp
            LET p_wfpol.jlocpcls  = g_endor.nocclssp
	    LET p_wfpol.jlsmind   = g_endor.nsmkspid
	    LET p_wfpol.jlicno    = g_endor.spicno
	    LET p_wfpol.jlnewicno = g_endor.spnicno
	    IF g_endor.spmarital IS NULL THEN
	        LET p_wfpol.jlmarital = 'M'
	    ELSE
	        LET p_wfpol.jlmarital = g_endor.spmarital
	    END IF
	    LET p_wfpol.jlrace    = g_endor.sprace
	    LET p_wfpol.jlage     = r_spage
	ELSE
	    IF g_endor.poltype = '4PAAJ' OR g_endor.poltype = '4PAE' OR
	      g_endor.poltype = '5PAFJ' OR g_endor.poltype = '5PAEF' OR --seq 17
              #g_endor.poltype = '5PAEC' OR                               -- seq 22 -- -- seq 25 --
	      #g_endor.poltype = '5PAPJ' OR g_endor.poltype = '5PAE' THEN -- seq 25 --
               g_endor.poltype MATCHES '?PAEC' OR   -- seq 25 --
	       g_endor.poltype MATCHES '?PAPJ' OR   -- seq 25 --
	       g_endor.poltype MATCHES '?PAE' THEN  -- seq 25 --
    	        LET v_year = YEAR(g_endor.dtincp) - YEAR(g_endor.ndobsp)
    	        LET v_month = MONTH(g_endor.dtincp) - MONTH(g_endor.ndobsp)
    	        IF v_month <= 0 THEN
        	        LET r_spage = v_year
    	        ELSE
        	        LET r_spage = v_year + 1
    	        END IF
    	        IF v_month = 0 AND v_year = 0 THEN
         	        LET r_spage = 1
    	        END IF
	    	LET p_wfpol.jlind     = 'P'
	        LET p_wfpol.jlname    = g_endor.spname
                LET p_wfpol.jlsex     = g_endor.nsexsp
                LET p_wfpol.jldob     = g_endor.ndobsp
                LET p_wfpol.jlocp     = g_endor.noccpsp
                LET p_wfpol.jlocpcls  = g_endor.nocclssp
	        LET p_wfpol.jlsmind   = g_endor.nsmkspid
	        LET p_wfpol.jlicno    = g_endor.spicno
	        LET p_wfpol.jlnewicno = g_endor.spnicno
	        LET p_wfpol.jlmarital = g_endor.spmarital
	        LET p_wfpol.jlrace    = g_endor.sprace
	        LET p_wfpol.jlage     = r_spage
	    ELSE
	    	LET p_wfpol.jlind     = 'N'
	    END IF
	END IF

	INSERT INTO wfmpolicy VALUES (p_wfpol.*) 

	DECLARE c_endcv CURSOR FOR
	    SELECT * FROM mulendcv
		WHERE polno = g_endor.polno
	FOREACH c_endcv INTO g_endcv.*
	    IF g_endcv.nannprem = 0 THEN
		CONTINUE FOREACH
	    END IF
	    INITIALIZE g_wfulpola.* TO NULL
	    CASE
        	WHEN g_endor.nfreq = "M" LET m_factor = 12
        	WHEN g_endor.nfreq = "Q" LET m_factor = 4
        	WHEN g_endor.nfreq = "H" LET m_factor = 2
        	WHEN g_endor.nfreq = "A" LET m_factor = 1
      	    END CASE
	    LET g_wfulpola.calprm = g_endcv.nannprem / m_factor
	    LET g_wfulpola.polno   = g_endcv.polno
	    LET g_wfulpola.covercd = g_endcv.covercd
	    LET g_wfulpola.benterm = g_endcv.benterm
	    LET g_wfulpola.class   = g_endcv.occls
	    LET g_wfulpola.xmorind = g_endcv.loadid
	    LET g_wfulpola.xmorrat = g_endcv.lodrate
	    LET g_wfulpola.sa      = g_endcv.nsa
	    LET g_wfulpola.annprm  = g_endcv.nannprem
	    LET g_wfulpola.effect  = g_endcv.incepdt
	    LET g_wfulpola.dtorginc= g_endcv.incepdt #??
	    LET g_wfulpola.summul  = g_endor.summul
	    LET g_wfulpola.trno    = g_endcv.trno
	    LET g_wfulpola.compyind= g_endcv.compyble
	    LET g_wfulpola.lpdd    = g_endcv.lpdd
	    IF g_endcv.osa = 0 THEN
	        LET g_wfulpola.trndt   = g_date_ext 
	    ELSE
	        LET g_wfulpola.trndt   = g_endcv.trndt
            END IF
	    SELECT prodprd, date_ext INTO g_wfulpola.prodprd, g_wfulpola.lenddt
		FROM mulpola
		WHERE polno = g_endcv.polno
		AND  effect = g_endcv.incepdt
		AND covercd = g_endcv.covercd
		AND xmorind = g_endcv.loadid 
                AND trno    = g_endcv.trno -- seq 4 --
	    IF STATUS = NOTFOUND THEN
		LET g_wfulpola.lenddt = NULL
		SELECT acmth INTO p_acmth FROM mcontrol
		LET p_acmth = '01', p_acmth CLIPPED
		LET g_wfulpola.prodprd = p_acmth
	    END IF

	    INSERT INTO wfmulpola VALUES (g_wfulpola.*)
	END FOREACH
	UPDATE mulendor
	    SET postflg = 'P'
	    WHERE polno = g_endor.polno
    END FOREACH
    FINISH REPORT   err_endos
END FUNCTION

FUNCTION pro_xpol()
DEFINE	v_class		CHAR(1)
DEFINE	l_count		SMALLINT, 		   	-- seq 14 --
	l_comcat2	LIKE magent.comctg 		-- seq 14 --
DEFINE  p_mths		LIKE mpolcvagt.mths,		-- seq 16 --
	p_npaldt	LIKE mpolcvagt.npaldt		-- seq 16 --
DEFINE  p_oragt1        LIKE mpolcvagt.oragt1,          --seq 27--
        p_oragt2        LIKE mpolcvagt.oragt2           --seq 27--
DEFINE  f_class     LIKE mulpola.class    -- seq 42 --
DEFINE  f_npaldt    LIKE mpolcvagt.npaldt -- seq 42 --
DEFINE  f_mths      LIKE mpolcvagt.mths   -- seq 42 --
DEFINE f_prmterm    SMALLINT    -- seq 48 --
--* seq 50 --
DEFINE f_gcp_plan_got_changed_ind SMALLINT
DEFINE f_new_gcp_plan_mths        SMALLINT
DEFINE f_new_gcp_plan_npaldt      DATE
-- seq 50 *--
--* seq 53 --
DEFINE f_xmorind                  CHAR(3)
-- seq 53 *--

    START REPORT alt_post TO "/u/report/uend2x20.rpt"

    DECLARE c_wfpolicy CURSOR FOR
	SELECT * FROM wfmpolicy 
	    WHERE upldind = 'E'
	    and polno = g_polno --kit
    FOREACH c_wfpolicy INTO p_wfpol.*

	SELECT * FROM xpolicy
	    WHERE polno = p_wfpol.polno

          IF STATUS <> NOTFOUND THEN
	     CONTINUE FOREACH
          END IF

	SELECT m.*, s.* INTO p_mpol.*, p_mpols.* FROM mpolicy m, mpolicys s
	    WHERE m.polno = p_wfpol.polno
	    AND   m.polno = s.polno 

	CALL ins_xpol()
	CALL ins_xpols()
	LET v_ofdate = p_mpol.dtofmov
	CALL ins_xpolm()
	CALL ins_xpoln()
        CALL ins_xpoln2()                     --seq 36--
        CALL ins_xsendx()                     --seq 38--
        CALL ins_xsnbzgst()                   --seq 45--
        CALL ins_xsendtad()                    --seq 49--
	CALL ins_xpolns()
	CALL ins_xpolv()
	CALL upd_mulunit()
        CALL ci_tag() -- seq 52

	DECLARE c_hwfpol CURSOR FOR
	    SELECT * FROM hwfmpolicy
		WHERE polno = p_wfpol.polno
		AND movdt = p_wfpol.movdt
	LET exi = 0
	FOREACH c_hwfpol INTO p_hwfpol.*
	    LET exi = 1
	END FOREACH
	IF exi = 0   THEN
	    INSERT INTO hwfmpolicy VALUES (p_wfpol.*)
	END IF


# amended by lhs
         #IF (p_wfpol.movcd = "11" AND p_wfpol.rev IS NOT NULL)
          IF (p_wfpol.movcd = "11")
	     OR p_wfpol.movcd = "52" --seq 40--
	     OR p_wfpol.movcd = "53" --seq 40--
	     OR p_wfpol.movcd = "47" -- seq 51--
	     OR p_wfpol.movcd = "41"    THEN

            LET g_comctga = p_mpols.comcata 
##paa
#	  IF p_wfpol.poltype = '4PAA' THEN
	  IF p_wfpol.polno > 32800000 AND p_wfpol.frqpay <> 'S' THEN
            DECLARE c_wula CURSOR FOR
              SELECT * FROM wfmulpola 
                 WHERE wfmulpola.polno = p_wfpol.polno
              ORDER BY seqno

						-- seq 14 --
	    SELECT MAX(class) INTO l_count
	    FROM mulpola
	    WHERE polno = p_wfpol.polno

	    IF l_count IS NULL THEN
                LET l_count = 0 	
	    END IF	
						-- end seq 14 -- 

            FOREACH c_wula INTO g_wfulpola.*
	      INITIALIZE p_oragt1, p_oragt2	TO NULL  -- seq 34 --

	      IF exi = 0   THEN
                 INSERT INTO hwfmulpola VALUES (g_wfulpola.* )
              END IF

	   #IF p_mpol.poltype MATCHES "5PA*" THEN  -- seq 14 --  -- seq 25 --
       --* seq 48 --
#             IF p_mpol.poltype MATCHES "?PA*" THEN  -- seq 25 --
       IF  p_mpol.poltype MATCHES "?PA*" 
           OR is_3s_uend2x20(p_mpol.poltype) THEN
       -- seq 48 *--
                                                	-- seq 14 --
	      IF g_wfulpola.class IS NULL THEN
	          LET l_count = l_count + 1
		  LET g_wfulpola.class = l_count

		  SELECT agt1, agt2, agtshr1, agtshr2
		  INTO g_wfulpola.agt1, g_wfulpola.agt2, g_wfulpola.comshr1,
		       g_wfulpola.comshr2
		  FROM mulendor
		  WHERE polno = g_wfulpola.polno

		  IF g_wfulpola.agt1 IS NULL OR g_wfulpola.agt1 = " " THEN
		      SELECT magtcd, sagtcd, magtshr, sagtshr
		      INTO g_wfulpola.agt1, g_wfulpola.agt2, 
			   g_wfulpola.comshr1, g_wfulpola.comshr2
		      FROM mpolicy
		      WHERE polno = g_wfulpola.polno
		  END IF

		  LET p_oragt1 = g_wfulpola.agt1	--seq 35--
		  LET p_oragt2 = g_wfulpola.agt2	--seq 35--

	          SELECT comctg INTO g_wfulpola.comcat
	          FROM magent
	          WHERE agtcd = g_wfulpola.agt1

	          IF g_wfulpola.agt1[1,3] = "D09" THEN
	              LET g_wfulpola.comcat = "E"
	          END IF

              --* seq 50 --
              INITIALIZE l_comcat2 TO NULL 
              -- seq 50 *--
	          SELECT comctg INTO l_comcat2
	          FROM magent
	          WHERE agtcd = g_wfulpola.agt2

	          IF g_wfulpola.agt2[1,3] = "D09" THEN
	              LET l_comcat2 = "E"
	          END IF
		  LET p_mths = 0			-- seq 16 --
		  LET p_npaldt = g_wfulpola.effect	-- seq 16 --

          --* seq 50 --
          LET f_gcp_plan_got_changed_ind = FALSE
          LET f_new_gcp_plan_mths = 0
          INITIALIZE f_new_gcp_plan_npaldt TO NULL 
          CALL is_gcp_plan_got_changed_by_endt(g_wfulpola.polno,
                                               g_wfulpola.covercd)
                                    RETURNING f_gcp_plan_got_changed_ind
                                              ,f_new_gcp_plan_mths
                                              ,f_new_gcp_plan_npaldt
                 
          IF f_gcp_plan_got_changed_ind THEN
              #special case
              #got previous mth & npdldt of cancelled gcp plan to the new one
              LET p_mths = f_new_gcp_plan_mths
              LET p_npaldt = f_new_gcp_plan_npaldt
          END IF
          -- seq 50 *--
	      ELSE

		  SELECT agt1, agt2, agtshr1, agtshr2, comcat1, comcat2, 
		#  	 mths, npaldt			-- seq 16 --    --seq 27--
		         mths, npaldt, oragt1, oragt2                   --seq 27--
		  INTO g_wfulpola.agt1, g_wfulpola.agt2, g_wfulpola.comshr1,
		       g_wfulpola.comshr2, g_wfulpola.comcat, l_comcat2,
		      # p_mths, p_npaldt			-- seq 16 --  --seq 27--
		      p_mths, p_npaldt, p_oragt1, p_oragt2     --seq 27--
		  FROM mpolcvagt
		  WHERE polno = g_wfulpola.polno
		  AND benefitno = g_wfulpola.class

	      END IF  -- seq 14 --

            --* seq 48 --
            LET f_prmterm = g_wfulpola.benterm
            IF p_wfpol.movcd = "41" THEN
                IF is_3s_uend2x20(p_mpol.poltype) AND g_wfulpola.benterm > p_wfpol.term THEN
                    #for 3 series, its prmterm may be different from the benefit term
                    LET f_prmterm = p_wfpol.term
                END IF
            END IF
            -- seq 48 *--
	    END IF						-- end seq 14 --

        --* seq 53 --
        IF p_wfpol.movcd = "41"                      AND
           is_3s_uend2x20(p_mpol.poltype)     = TRUE AND 
           is_endt_post_case(g_wfulpola.polno, g_date_ext)= TRUE AND 
           is_occls_null(g_wfulpola.polno
                         ,g_wfulpola.covercd
                         ,g_wfulpola.trno
                         ,g_wfulpola.xmorind) = TRUE THEN

            INITIALIZE f_xmorind TO NULL
            
            LET f_xmorind = g_wfulpola.xmorind
            IF g_wfulpola.xmorind IS NULL THEN
                LET f_xmorind = "0"
            END IF
            UPDATE mulendcv
               SET occls = g_wfulpola.class
             WHERE polno         = g_wfulpola.polno
               AND covercd       = g_wfulpola.covercd
               AND trno          = g_wfulpola.trno
               AND NVL(loadid,"0") = f_xmorind
               AND nsa > 0
               AND occls IS NULL
        END IF
        -- seq 53 *--
              INSERT INTO xulpola VALUES (g_wfulpola.polno,  g_wfulpola.covercd,
                  #NULL, g_wfulpola.xmorind, g_wfulpola.benterm, g_wfulpola.sa,		-- seq 14 --
                   --* seq 48 --
#                    g_wfulpola.class, g_wfulpola.xmorind, g_wfulpola.benterm, g_wfulpola.sa,	-- seq 14 --
                   g_wfulpola.class, g_wfulpola.xmorind, f_prmterm, g_wfulpola.sa,  
                   -- seq 48 *--
		   g_wfulpola.xmorper, g_wfulpola.xmorrat,  g_wfulpola.instprm,
		   g_wfulpola.calprm,  g_wfulpola.compyind, g_wfulpola.lpdd,
		   g_wfulpola.delind,  NULL, 		    g_wfulpola.benterm,
		   g_wfulpola.annprm,  g_date_ext,   	    g_wfulpola.seqno,
		   g_wfulpola.effect,  g_wfulpola.prodprd,  g_wfulpola.dtorginc,
		   g_wfulpola.comcat,  g_wfulpola.agt1,     g_wfulpola.agt2,
	           g_wfulpola.comshr1, g_wfulpola.comshr2,  g_wfulpola.summul,
		   g_wfulpola.lenddt,  g_wfulpola.trno,     g_wfulpola.trndt)

               IF STATUS < 0   THEN
                  OUTPUT TO REPORT alt_post (p_mpol.polno)
               END IF

	 
             #IF p_mpol.poltype MATCHES "5PA*" THEN    -- seq 25 --
        --* seq 48 --
#               IF p_mpol.poltype MATCHES "?PA*" THEN    -- seq 25 --
        IF  p_mpol.poltype MATCHES "?PA*" 
            OR is_3s_uend2x20(p_mpol.poltype) THEN 
        -- seq 48 *--
               					-- seq 14 --
		IF p_oragt1 IS NULL THEN		--seq 27--
		   LET p_oragt1 = g_wfulpola.agt1	--seq 27--
		   LET p_oragt2 = g_wfulpola.agt2	--seq 27--
		END IF 

	       --* seq 42 --
	       SELECT DISTINCT "X"
	       FROM mulendor
	       WHERE polno = p_mpol.polno
	       #AND opid = "MMCR"  -- seq 54 --
	       AND opid in ("MMCR", "MEDRep")  #-- seq 54 --

	       LET f_class = NULL
	       LET f_mths = NULL
	       LET f_npaldt = NULL

	       IF STATUS <> NOTFOUND THEN
	          SELECT DISTINCT "X"
	          FROM mtnbzchkcode
	          WHERE poltype = g_wfulpola.covercd
	          #AND syscode = "MMCR"  -- seq 54 --
	          AND syscode in ("MMCR", "GEMPAK") #-- seq 54 -- how about the contributors?
	          AND usercd8 = "MEDICAL"

		  IF STATUS <> NOTFOUND THEN
		     IF g_wfulpola.xmorind IS NOT NULL THEN
		        SELECT class INTO f_class
			FROM mulpola
			WHERE polno = g_wfulpola.polno
			AND covercd = g_wfulpola.covercd
			AND xmorind = g_wfulpola.xmorind
		     ELSE
		        SELECT class INTO f_class
			FROM mulpola
			WHERE polno = g_wfulpola.polno
			AND covercd = g_wfulpola.covercd
			AND xmorind IS NULL
		     END IF

		     SELECT mths,npaldt INTO f_mths,f_npaldt
		     FROM mpolcvagt
		     WHERE polno = g_wfulpola.polno
		     AND benefitno = f_class

		     LET p_mths = f_mths
		     LET p_npaldt = f_npaldt
		  END IF
	       END IF
	       -- seq 42 *--

	       INSERT INTO xpolcvagt 
	       VALUES (g_wfulpola.polno, g_wfulpola.class, g_wfulpola.agt1, 
		       g_wfulpola.agt2, g_wfulpola.comshr1, g_wfulpola.comshr2,
  		       #g_wfulpola.comcat, l_comcat2, g_wfulpola.agt1, --seq 27--
		       g_wfulpola.comcat, l_comcat2, p_oragt1, --seq 27--
		       #g_wfulpola.agt2, 0, g_wfulpola.effect)	-- seq 16 --
		       #g_wfulpola.agt2, p_mths, p_npaldt)	-- seq 16 --  --seq 27--
		       p_oragt2, p_mths, p_npaldt)
	    					-- end seq 14 --

               END IF
            END FOREACH   
	  ELSE
#paa
            DECLARE c_wfa CURSOR FOR
              SELECT * FROM wfmpolicya 
                 WHERE wfmpolicya.polno = p_wfpol.polno
              ORDER BY seqno

            FOREACH c_wfa INTO p_wfpola.*
	      IF exi = 0   THEN
                 INSERT INTO hwfmpolicya VALUES (p_wfpola.* )
              END IF

              INSERT INTO xpolicya VALUES (p_wfpola.polno,  p_wfpola.covercd,
                      NULL,     p_wfpola.xmorind, p_wfpola.benterm,
                      p_wfpola.sa,        p_wfpola.xmorper, p_wfpola.xmorrat,
                      p_wfpola.instprm,   p_wfpola.calprm,  p_wfpola.compyind,
                      p_wfpola.lpdd,      p_wfpola.delind, null, 
                      null, p_wfpola.benterm, null, g_date_ext, p_wfpola.seqno,
		      p_wfpola.effect,    p_wfpola.prodprd, p_wfpola.dtorginc,
		      p_wfpola.comcat )

               IF STATUS < 0   THEN
                  OUTPUT TO REPORT alt_post (p_mpol.polno)
               END IF

            END FOREACH   
	  END IF 	#4PAA		paa
	     --*seq 40-- 
	     IF p_wfpol.movcd = "52" OR p_wfpol.movcd = "53" THEN 
	        LET v_ofdate = p_wfpol.effdt
                CALL upd_xpol1()
                CALL upd_xpols1()
	     ELSE --seq 40*--
                CALL upd_xpol()
		CALL upd_xpols()
                CALL upd_xpoln()
	     END IF --seq 40--
           # CALL upd_xpoln2()           --seq 36--
          ELSE
   	    IF p_wfpol.movcd = "39" OR p_wfpol.movcd = "40" OR
	       p_wfpol.movcd = "09" OR
	       p_wfpol.movcd = "48" OR p_wfpol.movcd = "18"   THEN 

##paa
#	     IF p_wfpol.poltype = '4PAA' THEN
	     IF p_wfpol.polno > 32800000 AND p_wfpol.frqpay <> 'S' THEN
              DECLARE c_wfmulpola CURSOR FOR
                SELECT * FROM wfmulpola 
                   WHERE wfmulpola.polno = p_wfpol.polno
                ORDER BY seqno

              FOREACH c_wfmulpola INTO g_wfulpola.*
	        IF exi = 0   THEN
                   INSERT INTO hwfmulpola VALUES (g_wfulpola.* )
                END IF

                INSERT INTO xulpola VALUES (g_wfulpola.polno,g_wfulpola.covercd,
                     g_wfulpola.class,  g_wfulpola.xmorind, g_wfulpola.benterm,
                     g_wfulpola.sa,     g_wfulpola.xmorper, g_wfulpola.xmorrat,
                     g_wfulpola.instprm,g_wfulpola.calprm,  g_wfulpola.compyind,
                     g_wfulpola.lpdd,   g_wfulpola.delind,  NULL,
		     g_wfulpola.benterm, g_wfulpola.annprm, g_date_ext,
		     g_wfulpola.seqno,   g_wfulpola.effect, g_wfulpola.prodprd,
		     g_wfulpola.dtorginc, g_wfulpola.comcat, g_wfulpola.agt1,
		     g_wfulpola.agt2,   g_wfulpola.comshr1, g_wfulpola.comshr2,
		     g_wfulpola.summul, g_wfulpola.lenddt,  g_wfulpola.trno,
		     g_wfulpola.trndt)

                 IF STATUS < 0   THEN
                    OUTPUT TO REPORT alt_post (p_mpol.polno)
                 END IF

              END FOREACH
	     END IF	#paa
   	    ELSE
##paa
#	     IF p_wfpol.poltype = '4PAA' THEN
	     IF p_wfpol.polno > 32800000 AND p_wfpol.frqpay <> 'S' THEN
              DECLARE c_pola1 CURSOR FOR
                SELECT * FROM mulpola 
                   WHERE polno = p_wfpol.polno
                   ORDER BY seq

              FOREACH c_pola1 INTO g_mulpola.*
	        LET v_class = g_mulpola.class USING "#"

                INSERT INTO xulpola VALUES (g_mulpola.polno,  g_mulpola.covercd,
                        v_class,          g_mulpola.xmorind,  g_mulpola.prmterm,
                        g_mulpola.sa,     g_mulpola.xmorper,  g_mulpola.xmorrat,
                        NULL, 		  g_mulpola.calprm,  g_mulpola.compyind,
                        g_mulpola.lpdd,   g_mulpola.delind,   NULL, 
                        g_mulpola.benterm, g_mulpola.annprm,  g_date_ext,
			g_mulpola.seq,    g_mulpola.effect,   g_mulpola.prodprd,
			g_mulpola.dtorginc, NULL, NULL, NULL, NULL, NULL,
			g_mulpola.summul,  g_mulpola.lenddt,  g_mulpola.trno,
		        g_mulpola.trndt)

                 IF STATUS < 0   THEN
                    OUTPUT TO REPORT alt_post (p_mpol.polno)
                 END IF
              END FOREACH 
#paa
	     END IF	#4PAA	paa
   	    END IF

# amended by lhs
           # IF p_wfpol.movcd = "11" OR p_wfpol.ofmovcd = "35" OR 
             IF p_wfpol.ofmovcd = "35" OR 
	       p_wfpol.ofmovcd = "32" OR p_wfpol.ofmovcd = "38" OR
	       p_wfpol.ofmovcd = "51" OR p_wfpol.movcd = "17" OR
	       p_wfpol.ofmovcd = "88" OR p_wfpol.ofmovcd = "43" OR # L19980762 
	       p_wfpol.movcd = "20" OR p_wfpol.movcd = "81" OR
	       p_wfpol.movcd = "86" OR p_wfpol.movcd = "85" OR
	       p_wfpol.movcd = "91" OR
	       p_wfpol.movcd = "92" OR p_wfpol.movcd = "93" OR
	       p_wfpol.movcd = "94" THEN
              CALL upd_xpol1()
              CALL upd_xpols1()
	      IF p_wfpol.movcd="81" OR p_wfpol.movcd="93" THEN
         #	 CALL ins_xpolt()         --seq 2--
              END IF
            ELSE
              CALL upd_xpol2()
              CALL upd_xpols2()
            END IF

          END IF

          UPDATE wfmpolicy
            SET upldind = "U"
              WHERE polno = p_mpol.polno

       END FOREACH 

       FINISH REPORT alt_post
       RUN "lp -dp03 /u/report/uend2x20.rpt"

END FUNCTION
           
--* seq 50 --
FUNCTION is_gcp_plan_got_changed_by_endt(fi_polno, fi_covercd)
DEFINE fi_polno              INTEGER
DEFINE fi_covercd            CHAR(8)

DEFINE fo_new_gcp_plan_mths   SMALLINT
DEFINE fo_new_gcp_plan_npaldt DATE

DEFINE f_covercd_cancelled   CHAR(8)
DEFINE f_covercd_included    CHAR(8)

    LET f_covercd_cancelled = " "
    LET f_covercd_included = " "

    LET fo_new_gcp_plan_mths = 0
    INITIALIZE fo_new_gcp_plan_npaldt TO NULL 
    
    IF fi_covercd MATCHES "3PUGCP[CVP]" THEN
    ELSE
        RETURN FALSE, "", ""
    END IF

    SQL
    SELECT FIRST 1 covercd
      INTO $f_covercd_cancelled
      FROM mulendcv
     WHERE polno = $fi_polno
       AND covercd MATCHES "3PUGCP[CVP]"
       AND osa > 0
       AND nsa = 0
    END SQL

    SQL
    SELECT FIRST 1 covercd
      INTO $f_covercd_included
      FROM mulendcv
     WHERE polno = $fi_polno
       AND covercd MATCHES "3PUGCP[CVP]"
       AND osa = 0
       AND nsa > 0
    END SQL

    IF f_covercd_cancelled IS NULL THEN
        LET f_covercd_cancelled = " "
    END IF

    IF f_covercd_included IS NULL THEN
        LET f_covercd_included = " "
    END IF

    IF f_covercd_cancelled <> f_covercd_included THEN
        #GCP plan got upgrade or downgrade
        
        SELECT mths, npaldt
          INTO fo_new_gcp_plan_mths, fo_new_gcp_plan_npaldt
          FROM mpolcvagt a, mulpola b
         WHERE a.polno = fi_polno
           AND a.polno = b.polno
           AND b.covercd = f_covercd_cancelled
           AND a.benefitno = b.class
        
        RETURN TRUE, fo_new_gcp_plan_mths, fo_new_gcp_plan_npaldt
    END IF

    RETURN FALSE, "", ""
END FUNCTION
-- seq 50 *--
FUNCTION ins_xpol()

    DEFINE  l_dobchid LIKE mulendor.dobchid,
            l_odobla  LIKE mulendor.odobla,
            l_ndobla  LIKE mulendor.ndobla
    DEFINE  l_excind1 LIKE mulendor.excind1,    --* seq 18 --
            l_excind2 LIKE mulendor.excind2,
            l_ooccpas LIKE mulendor.ooccpas,
            l_noccpas LIKE mulendor.noccpas,
            l_uwla    LIKE mulendor.uwla,
            l_uwjl    LIKE mulendor.uwjl        -- seq 18 *--

    LET l_dobchid = NULL
    LET l_odobla = NULL
    LET  l_ndobla = NULL
    LET l_excind1 = NULL        --* seq 18 --
    LET l_excind2 = NULL
    LET l_ooccpas = NULL
    LET l_noccpas = NULL
    LET l_uwla    = NULL
    LET l_uwjl    = NULL-- seq 18 *--

{   -- seq 18 --
    SELECT dobchid, odobla, ndobla, dtincp, ndobla, spouseid , spname , ndobsp
    INTO  l_dobchid, l_odobla, l_ndobla, g_endor.dtincp, g_endor.ndobla,
          g_endor.spouseid, g_endor.spname, g_endor.ndobsp
    FROM mulendor
    WHERE polno = p_mpol.polno
}   -- seq 18 --
    --* seq 18 --
    SELECT dobchid, odobla, ndobla, dtincp, ndobla, spouseid , spname , ndobsp,
           uwla, uwjl, excind1, excind2, ooccpas, noccpas
    INTO  l_dobchid, l_odobla, l_ndobla, g_endor.dtincp, g_endor.ndobla,
          g_endor.spouseid, g_endor.spname, g_endor.ndobsp, l_uwla, l_uwjl,
          l_excind1, l_excind2, l_ooccpas, l_noccpas
    FROM mulendor
    WHERE polno = p_mpol.polno

    LET p_mpol.excind1 = l_excind1
    LET p_mpol.excind2 = l_excind2
    LET p_mpol.pdbelg = l_ooccpas
    LET p_mpol.pdb22 = l_noccpas
    -- seq 18 *--

    IF l_dobchid = "Y" AND l_ndobla IS NOT NULL THEN
-- *seq 11 --    
       	 LET v_year1 = YEAR(g_endor.dtincp) - YEAR(g_endor.ndobla)
	 LET v_month1 = MONTH(g_endor.dtincp) - MONTH(g_endor.ndobla)
	IF v_month1 <= 0 THEN
	 LET r_laage = v_year1
	ELSE
	 LET r_laage = v_year1 + 1
	END IF
	IF v_month1 = 0 AND v_year1 = 0 THEN
	 LET r_laage = 1
	END IF
	LET p_wfpol.laage = r_laage  -- seq 11* --
	LET p_mpol.laage = p_wfpol.laage
    END IF   -- seq 3  --
   
       LET p_jlind = "N"

       SELECT jlind INTO p_jlind
       FROM mpolicy
       WHERE polno = p_mpol.polno


       IF l_dobchid = "Y" AND p_jlind = "S" THEN
          
	  DECLARE l_cur CURSOR FOR
	  SELECT incepdt
	  FROM mulendcv
	  WHERE polno = p_mpol.polno
	  AND covercd = "USYR"
	  AND loadid IS NULL 

	  FOREACH l_cur INTO p_dtincp
	  END FOREACH

          LET v_year = YEAR(p_dtincp) - YEAR(g_endor.ndobsp)
	  LET v_month = MONTH(p_dtincp) - MONTH(g_endor.ndobsp)
	  IF v_month <= 0 THEN
	     LET r_spage = v_year
	  ELSE
	     LET r_spage = v_year + 1
	  END IF
	  IF v_month = 0 AND v_year = 0 THEN
	   LET r_spage = 1
	  END IF
           LET p_wfpol.jlage     = r_spage
       END IF	   -- seq 11 --

    INSERT INTO xpolicy VALUES (p_mpol.polno,    p_mpol.prono,
              p_mpol.magtcd,    p_mpol.poltype,  p_mpol.term,
         #    p_mpol.stats,     p_mpol.dtincp,   p_mpol.frqpay, 
              p_wfpol.stats,     p_mpol.dtincp,   p_mpol.frqpay, 
              p_mpol.modpay,    p_mpol.dtonmov,  NULL, 
         #    p_mpol.onmovcd,  NULL, NULL,  
              p_wfpol.movcd,  NULL, NULL,  
              p_wfpol.movcd,     p_mpol.dtmat,    p_mpol.dtorgcom, 
              p_mpol.dtaccp,    p_mpol.laname,   p_mpol.lasex,  
              p_mpol.ladob,     p_mpol.laage,    p_mpol.laageadm, 
              p_mpol.laicno,    p_mpol.salute,   p_mpol.laocp,  
              p_mpol.laocpcls,  p_mpol.laageld,  p_mpol.lanationcd,
              p_mpol.lamarital, p_mpol.larace,   p_mpol.atelno,
              p_mpol.jlind,     p_mpol.adind,    p_mpol.sumas,
              p_mpol.tinstprm,  p_mpol.tannprm,  p_mpol.partip,
              p_mpol.snelg,     p_mpol.pdbelg,   p_mpol.aplinx,
              p_mpol.bonus,     p_mpol.sarisk,   p_mpol.spvr1,
              p_mpol.or1,       p_mpol.spvr2,    p_mpol.or2,
              p_mpol.uwla,      p_mpol.collat,   
              p_mpol.sect23,    p_mpol.assign,   p_mpol.neccd, NULL, NULL, 
	      g_date_ext,
	      p_mpol.ontrandt,  p_mpol.smkind,   p_mpol.spvr3,
	      p_mpol.or3,       p_mpol.accu,
	      p_mpol.rider,     p_mpol.supp,
	      p_mpol.excind1,   p_mpol.wpbind1,  p_mpol.uwjl,
	      p_mpol.excind2,   p_mpol.wpbind2,  p_mpol.pdb22,
	      p_mpol.sn22,      p_mpol.load1,    p_mpol.load2,
	      p_mpol.load3,     p_mpol.load4,    p_mpol.jlsar,
	      p_mpol.jlsartot,  p_mpol.benind,   p_mpol.exc1,
	      p_mpol.exc2,      p_mpol.exc3,     p_mpol.exc4,
	      p_mpol.exc5,      p_mpol.exc6,     p_mpol.exc7,
	      p_mpol.exc8,      p_mpol.exc9,     p_mpol.exc10,
	      p_mpol.excwpb,    p_mpol.excoth,   p_mpol.ccyear,
	      p_mpol.cchalf,    p_mpol.ccquar,   p_mpol.ccmonth,
	      p_mpol.boyear,    p_mpol.bohalf,   p_mpol.boquar,
	      p_mpol.bomonth,   p_mpol.dtreassi, p_mpol.sagtcd,
	      p_mpol.magtshr,   p_mpol.sagtshr,  p_mpol.or1y2,
	      p_mpol.or1y3,     p_mpol.or1y4,    p_mpol.or2y2,
	      p_mpol.or2y3,     p_mpol.or2y4,    p_mpol.or3y2,
	      p_mpol.or3y3,     p_mpol.or3y4,    p_mpol.origapi,
	      p_mpol.persper,   p_mpol.prodper,  p_mpol.rgbper,
	      p_mpol.lanewicno, p_mpol.laanninc)

    IF STATUS < 0   THEN
        OUTPUT TO REPORT alt_post (p_mpol.polno)
    END IF
END FUNCTION

FUNCTION ins_xpols()

    IF p_wfpol.movcd = "11" THEN
       LET p_mpols.revind = "Y"
    END IF 

    INSERT INTO xpolicys VALUES (p_mpols.polno,   p_mpols.bankac,
	    p_mpols.acumbon,    p_mpols.yrbon,    p_mpols.intrmbon,
            p_mpols.boncsh,     p_mpols.bonpdag,  p_mpols.bonpfag,
            p_mpols.bonrtag,
            p_mpols.lasartot,   p_mpols.risa,     p_mpols.orgsa,
            p_mpols.redult,     p_mpols.dedult,   p_mpols.laaddr1,
            p_mpols.laaddr2,    p_mpols.laaddr3,  p_mpols.annex1,
            p_mpols.annex2,     p_mpols.annex3,   p_mpols.annex4, 
            p_mpols.annex5,     p_mpols.annex6,   p_mpols.annex7, 
            p_mpols.annex8,     p_mpols.annex9,   p_mpols.annex10, 
            p_mpols.sprov1,     p_mpols.sprov2,   p_mpols.sprov3,
            p_mpols.sprov4,     p_mpols.height1,  p_mpols.weight1,
            p_mpols.dev,        p_mpols.mdtint1,  p_mpols.mdtint2, 
            p_mpols.factrty,    p_mpols.facstats, p_mpols.riskyr,   
            p_mpols.dtpol,      NULL,              p_mpols.dtpropent,
            p_mpol.dtorgcom,    p_mpols.medind,   p_mpols.dtmed,
            p_mpols.dtlendt,    p_mpols.lendtcd,
            p_mpols.dtsub,      p_mpols.polnosub, p_mpols.dttrnf,
            p_mpols.agttrnf,    p_mpols.airmail,  p_mpols.notcind,
            p_mpols.spcmark,    p_mpols.chgind,   p_mpols.prtcind,
            p_mpols.frcpind,    p_mpols.valcls,   p_mpols.gbind,
            p_mpols.dupind,     p_mpols.secg,     p_mpols.chxray,
            p_mpols.volind,     p_mpols.thdprty,  p_mpols.comuind,
            p_mpols.astind,     p_mpols.revind,   p_mpols.filrgf,
            p_mpols.comcata,    p_mpols.comyra,   p_mpols.comcatb,
            p_mpols.comyrb,     NULL, NULL, NULL,   NULL, NULL,
            NULL,   NULL, NULL, NULL,   NULL,	  p_mpols.comamtb1,
            p_mpols.comamtb2,   p_mpols.comamtb3, p_mpols.comamtb4,
            p_mpols.comamtb5,   p_mpols.comamtb6, p_mpols.comamtb7,
            p_mpols.comamtb8,   p_mpols.comamtb9, p_mpols.comamtb10,
            NULL, g_date_ext,       p_mpols.laaddr4,  p_mpols.oriagt,
	    p_mpols.notrnf,     p_mpols.dupdocdt, p_mpols.svind,
	    p_mpols.surval,     p_mpols.prjvoiddt,p_mpols.dtaccp,
	    NULL,    	        p_mpols.eqlage,   p_mpols.offaddr1,
	    p_mpols.offaddr2,   p_mpols.offaddr3, p_mpols.offaddr4,
	    p_mpols.offtelno,   p_mpols.postind,  p_mpols.annex11,
	    p_mpols.annex12,    p_mpols.annex13,  p_mpols.annex14,
	    p_mpols.annex15,    p_mpols.annex16,  p_mpols.annex17,
	    p_mpols.annex18,    p_mpols.annex19,  p_mpols.annex20)

    IF STATUS < 0   THEN
        OUTPUT TO REPORT alt_post (p_mpol.polno)
    END IF
END FUNCTION

FUNCTION ins_xpolm()
    DECLARE c_zmpolm CURSOR FOR
       SELECT * FROM mpolicym
          WHERE mpolicym.polno = p_mpol.polno

    FOREACH c_zmpolm INTO p_mpolm.*
       LET v_chgmemo = p_mpolm.chgmemo[1,18]
       INSERT INTO xpolicym VALUES (p_mpolm.rectype,  p_mpolm.polno,    
               p_mpolm.chgdt,       v_chgmemo,        p_mpolm.chgstats,
               g_date_ext )

       IF STATUS < 0   THEN
           OUTPUT TO REPORT alt_post (p_mpol.polno)
       END IF
    END FOREACH
END FUNCTION

FUNCTION ins_xpoln()
    DECLARE c_zmpoln CURSOR FOR
       SELECT * FROM mpolicyn
          WHERE mpolicyn.polno = p_mpol.polno

    FOREACH c_zmpoln INTO p_mpoln.*
      ----* seq 39 start ---
      IF p_mpoln.mtype ="X" THEN
         SELECT DISTINCT "X"
	 FROM wfmpolicy3 a
	 WHERE a.polno = p_mpoln.polno
	 AND a.mtype ="X"

	 IF STATUS = NOTFOUND THEN
           CONTINUE FOREACH
	 END IF
      END IF
      
      ---- seq 39 end ---
       INSERT INTO xpolicyn VALUES (p_mpoln.polno,    p_mpoln.mtype, 
               p_mpoln.mname,      p_mpoln.icno,     p_mpoln.sex,
               p_mpoln.rel,        p_mpoln.agenb,    p_mpoln.dob,
               p_mpoln.ageadm,     p_mpoln.indic,    g_date_ext,
	       p_mpoln.smkind,     p_mpoln.race,     p_mpoln.nation,
	       p_mpoln.occpcls,    p_mpoln.occup,    p_mpoln.percent,
	       p_mpoln.lamarital,  p_mpoln.newicno )

       IF STATUS < 0   THEN
           OUTPUT TO REPORT alt_post (p_mpol.polno)
       END IF
    END FOREACH
END FUNCTION

-- for nomination details 270297
FUNCTION ins_xpolns()
    DECLARE c_xpolns CURSOR FOR
       SELECT opid,polno,name,mtype,addr1,addr2,addr3,addr4,upd_ind,reldesc
	 FROM mpolicyns
        WHERE mpolicyns.polno = p_mpol.polno

    FOREACH c_xpolns INTO p_mpolns.opid, p_mpolns.polno,
			  p_mpolns.name, p_mpolns.mtype,
			  p_mpolns.addr1, p_mpolns.addr2,
			  p_mpolns.addr3, p_mpolns.addr4,
			  p_mpolns.upd_ind, p_mpolns.reldesc

       INSERT INTO xpolicyns VALUES (p_mpolns.opid, p_mpolns.polno, 
				     p_mpolns.name, p_mpolns.mtype, 
				     p_mpolns.addr1, p_mpolns.addr2,
				     p_mpolns.addr3, p_mpolns.addr4,
				     p_mpolns.upd_ind, g_date_ext,
				     p_mpolns.reldesc)
       IF STATUS < 0   THEN
           OUTPUT TO REPORT alt_post (p_mpol.polno)
       END IF
    END FOREACH
END FUNCTION

FUNCTION ins_xpolv()
    DECLARE c_zmpolv CURSOR FOR
       SELECT * FROM mpolicyv
          WHERE mpolicyv.polno = p_mpol.polno

    FOREACH c_zmpolv INTO p_mpolv.*
       INSERT INTO xpolicyv VALUES (p_mpolv.polno, 
               p_mpolv.trmov,      p_mpolv.dtval,    p_mpolv.dteval,
               p_mpolv.sv,         p_mpolv.bonsv,    p_mpolv.lonval,
               p_mpolv.pdupval,    p_mpolv.stsv,     p_mpolv.stpdup,
               p_mpolv.stbon,      p_mpolv.termbon,  p_mpolv.movrom,
               p_mpolv.prmdue,     p_mpolv.dtnpd,    p_mpolv.pvno,
               p_mpolv.vquot,      p_mpolv.vamtpay,  g_date_ext )

       IF STATUS < 0   THEN
           OUTPUT TO REPORT alt_post (p_mpol.polno)
       END IF

    END FOREACH
END FUNCTION


{--seq 2--
FUNCTION ins_xpolt()

   DECLARE c_wft CURSOR FOR
     SELECT * FROM wfmpolicyt 
       WHERE wfmpolicyt.polno = p_wfpol.polno
     ORDER BY seqno

   FOREACH c_wft INTO p_wfpolt.*
      IF exi = 0   THEN
        INSERT INTO hwfmpolicyt VALUES (p_wfpolt.* )
      END IF

      INSERT INTO xpolicyt VALUES (p_wfpolt.polno,  p_wfpolt.covercd,
                      NULL,     p_wfpolt.xmorind, p_wfpolt.benterm,
                      p_wfpolt.sa,        p_wfpolt.xmorper, p_wfpolt.xmorrat,
                      p_wfpolt.instprm,   p_wfpolt.calprm,  p_wfpolt.compyind,
                      p_wfpolt.lpdd,      p_wfpolt.delind, null, 
                      null, p_wfpolt.benterm, null, g_date_ext, p_wfpolt.seqno,
		      p_wfpolt.effect,    p_wfpolt.prodprd, p_wfpolt.dtorginc,
		      p_wfpolt.comcat, p_wfpolt.eqtfund,
                      p_wfpolt.bondfund, p_wfpolt.admchg,
                      p_wfpolt.agtcd)
                  #   p_wfpolt.mngfund, p_wfpolt.bumifund)     --seq 1--
   END FOREACH
END FUNCTION
}



FUNCTION upd_xpol()
DEFINE p_sar	LIKE mulendor.lasar,
	p_jlsar LIKE mulendor.jlsar,
        p_stprem LIKE mulendor.stprem,
        p_anprmdif LIKE mulendor.anprmdif,
        p_psrprmchg LIKE mulendor.psrprmchg,
        p_agt1 LIKE mulendor.agt1,
        p_summul LIKE mulendor.summul,
	f_indsam LIKE tsendwfmpolicyo.indsam -- seq 13 --

DEFINE	p_premchg,				-- seq 15 --
	p_pprprmchg,
	p_psaprmchg,				--seq 17--
	ann_premchg,
	ann_psaprmchg,				--seq 17--
	ann_pprprmchg	LIKE mulendor.premchg,	
	p_prem_number	LIKE payment_mode.prem_number,
	p_change,			
	p_pruterm	CHAR(1)			-- end seq 15 --

DEFINE p_uwjl LIKE mulendor.uwjl --seq 41 --

# amended by lhs
  #  IF p_wfpol.movcd = "11" AND p_wfpol.rev = "Y"   THEN
     IF p_wfpol.movcd = "11" THEN
       LET v_effdt = p_wfpol.npd
    ELSE
       LET v_effdt = p_wfpol.effdt
    END IF
    IF p_wfpol.jlind IS NULL THEN
	LET p_wfpol.jlind = p_mpol.jlind
    END IF

    LET p_sar = NULL
    LET p_jlsar = NULL
    LET p_stprem = NULL
    SELECT lasar, jlsar, stprem, anprmdif, summul, agt1, psrprmchg,
	   premchg, pprprmchg,		-- seq 15 --
	   psaprmchg	,		--seq 17--
	   uwjl --seq 41 --
	 INTO p_sar, p_jlsar, p_stprem, p_anprmdif, p_summul, p_agt1, p_psrprmchg,
         p_premchg, p_pprprmchg,		-- seq 15
	 p_psaprmchg,				--seq 17--
	 p_uwjl --seq 41 --
	 FROM mulendor
	WHERE polno = p_wfpol.polno
    IF p_psrprmchg IS NULL THEN LET p_psrprmchg = 0 END IF

    LET p_anprmdif = p_anprmdif - p_psrprmchg

    --* seq 13 --
     INITIALIZE f_indsam TO NULL
     LET f_indsam = 0

     SELECT indsam into f_indsam
     FROM tsendwfmpolicyo
     where polno = p_wfpol.polno
    -- seqllk *--

   
     #--IF p_wfpol.poltype MATCHES "5PA*" THEN 		-- seq 15 --seq 19--
     --* seq 48 --
#      IF p_wfpol.poltype MATCHES "?PA*" THEN 		--seq 19--
     IF p_wfpol.poltype MATCHES "?PA*" 
     OR is_3s_uend2x20(p_wfpol.poltype) THEN
     -- seq 48 *--
        IF p_premchg IS NULL THEN LET p_premchg = 0 END IF
        IF p_pprprmchg IS NULL THEN LET p_pprprmchg = 0 END IF
        IF p_psaprmchg IS NULL THEN LET p_psaprmchg = 0 END IF	--seq 17--

	SELECT prem_number INTO p_prem_number
	FROM payment_mode
	WHERE prem_mode = p_wfpol.frqpay

	LET ann_premchg = p_premchg * p_prem_number
	LET ann_pprprmchg = p_pprprmchg * p_prem_number
	LET ann_psaprmchg = p_psaprmchg * p_prem_number	--seq 17--

	--seq 19--
	# Cater for 4th series PAA policy on servicing agent rules
	IF p_wfpol.poltype MATCHES "4PA*" THEN
	   LET ann_premchg   = p_anprmdif	--PAA portion
	   LET ann_psaprmchg = p_psrprmchg	--PSA portion
        END IF
	--end seq 19--

	LET p_change = "N"
	LET p_pruterm = "N"
	
        {--seq 17--
	IF ann_pprprmchg <> 0 THEN
	   SELECT UNIQUE 'X' 
	   FROM mulendcv
	   WHERE polno = p_wfpol.polno
	   AND covercd = "5PUPT"
	   AND osa = 0

	   IF STATUS <> NOTFOUND THEN 
	      LET p_pruterm = "Y"
	   END IF
 	END IF

	CASE
	   WHEN ann_premchg <> 0 AND ann_pprprmchg =  0
	      IF ann_premchg >= 900 AND f_indsam >= 20 THEN
		 LET p_change = "Y"
	      END IF

	   WHEN ann_premchg =  0 AND ann_pprprmchg <> 0
	      IF ann_pprprmchg >= 900 AND p_pruterm = "Y" THEN
		 LET p_change = "Y"
	      END IF
		
	   WHEN ann_premchg <> 0 AND ann_pprprmchg <> 0
	      IF (ann_premchg + ann_pprprmchg) >= 900 AND f_indsam >= 20 THEN
		 LET p_change = "Y"
	      END IF
	END CASE
	}--seq 17--

	{ --*seq 21--
	CASE
	WHEN ann_premchg   >= 900 LET p_change = "Y"
	WHEN ann_psaprmchg >= 900 LET p_change = "Y"
	WHEN ann_pprprmchg >= 900 LET p_change = "Y"
	WHEN (ann_premchg + ann_pprprmchg) >= 900   	
				  LET p_change = "Y" 
	END CASE
	} --seq 21*--
	
	--*seq 21--
	IF ann_premchg >= 900 THEN
		LET p_change = "Y"
	END IF
	--seq 21*--
	
	--end seq 17--

        IF p_change = "Y" THEN
           UPDATE xpolicy
           SET xpolicy.agtcd = p_agt1,	#assign new service agent
       	       xpolicy.sagtcd = NULL,
       	       xpolicy.magtshr = "100", 
       	       xpolicy.sagtshr = NULL
           WHERE polno = p_wfpol.polno 
        END IF
     ELSE							-- end seq 15 --

        #IF p_anprmdif >= 900 AND p_summul >= 20 THEN -- seq 13 --
        IF p_anprmdif >= 900 AND f_indsam >= 20 THEN -- seq 13 --
           UPDATE xpolicy
           SET xpolicy.agtcd = p_agt1,	#assign new service agent
       	       xpolicy.sagtcd = NULL,
       	       xpolicy.magtshr = "100", 
       	       xpolicy.sagtshr = NULL
           WHERE polno = p_wfpol.polno 
        END IF
     END IF							-- seq 15 --


    IF p_stprem IS NOT NULL THEN
       UPDATE xpolicy
       SET xpolicy.origapi = p_stprem
       WHERE polno = p_wfpol.polno 
   END IF  

   { IF p_wfpol.jlind = 'S' THEN		--* seq 8 --
       UPDATE xpolicy
       SET xpolicy.marital = 'M'
       WHERE polno = p_wfpol.polno 
   END IF  }					-- seq 8 *--
 

    UPDATE xpolicy
       SET xpolicy.poltyp = p_wfpol.poltype,
           xpolicy.poltm = p_wfpol.term,
         # xpolicy.polstat = p_wfpol.stats,
           xpolicy.incpdt = p_wfpol.dtincp,
           xpolicy.movcd = p_wfpol.movcd,
           xpolicy.frqpy = p_wfpol.frqpay,
           xpolicy.modepy = p_wfpol.modpay,
           xpolicy.jlife = p_wfpol.jlind,
           xpolicy.on_movdt = v_effdt,
         # xpolicy.on_movcd = p_wfpol.movcd,
           xpolicy.off_movdt = NULL,
           xpolicy.off_movcd = NULL,
           xpolicy.off_trn_dt = NULL,
           xpolicy.matudt = p_wfpol.dtmat,
           xpolicy.sex = p_wfpol.lasex,
           xpolicy.dob = p_wfpol.ladob,
           xpolicy.occp = p_wfpol.laocp,
           xpolicy.occp_cls = p_wfpol.laocpcls,
           xpolicy.sumas = p_wfpol.sumas,
           xpolicy.instprm = p_wfpol.tinstprm,
           xpolicy.annprm = p_wfpol.tannprm,
           xpolicy.uwres = p_wfpol.uwla,
           xpolicy.on_trn_dt = p_wfpol.movdt,
           xpolicy.smkind = p_wfpol.lasmind,
           xpolicy.sar = p_sar,
	   xpolicy.jlsar = p_jlsar
         WHERE polno = p_wfpol.polno
  IF STATUS < 0   THEN
     OUTPUT TO REPORT alt_post (p_mpol.polno)
  END IF

 -- seq 10 --
  IF p_wfpol.movcd= "11" THEN 
         #   SELECT effdt INTO v_ivydate  -- seq 12 --
         #   FROM mulendor                -- seq 12 --
	 #   WHERE polno = p_wfpol.polno  -- seq 12 --

         SELECT effdt INTO v_ivydate  -- seq 12 --
	 FROM wfmpolicy               -- seq 12 --
	 WHERE polno = p_wfpol.polno  -- seq 12 --
	 
         IF STATUS <> NOTFOUND THEN  
	         UPDATE xpolicy
                 SET xpolicy.on_movdt = v_ivydate    
                 WHERE polno = p_wfpol.polno
	 END IF                     
  END IF
 -- seq 10 --

 -- seq 41 start --
# IF p_wfpol.movcd == "41" AND (p_uwjl IS NOT NULL OR p_uwjl <> " " ) THEN				-- seq 51hash out
  IF (p_wfpol.movcd == "41" OR p_wfpol.movcd == "47") AND (p_uwjl IS NOT NULL OR p_uwjl <> " " ) THEN	-- seq 51
       UPDATE xpolicy
        SET xpolicy.uwjl = p_uwjl
        WHERE polno = p_wfpol.polno
  END IF
-- seq 41 end --
END FUNCTION

FUNCTION upd_xpol1()

  IF p_wfpol.ofmovcd IS NOT NULL    THEN
    UPDATE xpolicy
       SET xpolicy.polstat = p_wfpol.stats,
           xpolicy.movcd = p_wfpol.ofmovcd,
           xpolicy.off_movcd = p_wfpol.ofmovcd,
           xpolicy.off_movdt = p_wfpol.effdt,
           xpolicy.off_trn_dt = p_wfpol.ofmovdt
         WHERE polno = p_wfpol.polno

     IF STATUS < 0   THEN
        OUTPUT TO REPORT alt_post (p_mpol.polno)
     END IF
  ELSE 
    IF p_wfpol.movcd = "81" OR
       p_wfpol.movcd = "85" OR
       p_wfpol.movcd = "86" OR
       p_wfpol.movcd = "93" OR
       p_wfpol.movcd = "94" THEN
      UPDATE xpolicy
         SET xpolicy.polstat = p_wfpol.stats,
             xpolicy.movcd = p_wfpol.movcd,
             xpolicy.on_movdt = p_wfpol.effdt, 
             xpolicy.on_movcd = p_wfpol.movcd,
             xpolicy.off_movcd = NULL,
             xpolicy.off_movdt = NULL,
             xpolicy.off_trn_dt = NULL,
             xpolicy.on_trn_dt =  p_wfpol.movdt
           WHERE polno = p_wfpol.polno

       IF STATUS < 0   THEN
          OUTPUT TO REPORT alt_post (p_mpol.polno)
       END IF
    ELSE    
      #   REVIVAL MOVEMENT(11), REINSTATEMENT (17,20)

      UPDATE xpolicy
         SET xpolicy.polstat = p_wfpol.stats,
             xpolicy.movcd = p_wfpol.movcd,
             xpolicy.on_movdt = v_ofdate, 
             xpolicy.on_movcd = p_wfpol.movcd,
             xpolicy.off_movcd = NULL,
             xpolicy.off_movdt = NULL,
             xpolicy.off_trn_dt = NULL,
             xpolicy.on_trn_dt =  p_wfpol.movdt
           WHERE polno = p_wfpol.polno

       IF STATUS < 0   THEN
          OUTPUT TO REPORT alt_post (p_mpol.polno)
       END IF
    END IF 
  END IF 

END FUNCTION

FUNCTION upd_xpol2()

  IF p_wfpol.ofmovcd IS NOT NULL    THEN
    UPDATE xpolicy
       SET xpolicy.polstat = p_wfpol.stats,
           xpolicy.movcd = p_wfpol.ofmovcd,
           xpolicy.off_movcd = p_wfpol.ofmovcd,
           xpolicy.off_movdt = p_wfpol.effdt,
           xpolicy.off_trn_dt = p_wfpol.ofmovdt,
           xpolicy.bnsindx = p_wfpol.bonus,
           xpolicy.sumas = p_wfpol.sumas,
           xpolicy.frqpy = p_wfpol.frqpay,
           xpolicy.instprm = p_wfpol.tinstprm,
           xpolicy.annprm = p_wfpol.tannprm
         WHERE polno = p_wfpol.polno

     IF STATUS < 0   THEN
        OUTPUT TO REPORT alt_post (p_mpol.polno)
     END IF
  ELSE 
    IF p_wfpol.movcd = "50" OR p_wfpol.movcd = "60" OR
       p_wfpol.movcd = "71" OR p_wfpol.movcd = "70" OR
       p_wfpol.movcd = "39" OR p_wfpol.movcd = "40" OR 
       p_wfpol.movcd = "09" OR
       p_wfpol.movcd = "48" OR p_wfpol.movcd = "18" OR
       p_wfpol.movcd = "86"    THEN
       UPDATE xpolicy
         SET xpolicy.polstat = p_wfpol.stats,
             xpolicy.movcd = p_wfpol.movcd,
             xpolicy.on_movdt = p_wfpol.effdt, 
             xpolicy.on_movcd = p_wfpol.movcd,
             xpolicy.off_movcd = NULL,
             xpolicy.off_movdt = NULL,
             xpolicy.off_trn_dt = NULL,
             xpolicy.on_trn_dt =  p_wfpol.movdt,
             xpolicy.bnsindx = p_wfpol.bonus,
             xpolicy.sumas = p_wfpol.sumas,
             xpolicy.frqpy = p_wfpol.frqpay,
             xpolicy.instprm = p_wfpol.tinstprm,
             xpolicy.annprm = p_wfpol.tannprm
           WHERE polno = p_wfpol.polno
  
       IF STATUS < 0   THEN
          OUTPUT TO REPORT alt_post (p_mpol.polno)
       END IF
    ELSE
      UPDATE xpolicy
         SET xpolicy.polstat = p_wfpol.stats,
             xpolicy.movcd = p_wfpol.movcd,
             xpolicy.on_movdt = v_ofdate, 
             xpolicy.on_movcd = p_wfpol.movcd,
             xpolicy.off_movcd = NULL,
             xpolicy.off_movdt = NULL,
             xpolicy.off_trn_dt = NULL,
             xpolicy.on_trn_dt =  p_wfpol.movdt,
             xpolicy.bnsindx = p_wfpol.bonus,
             xpolicy.sumas = p_wfpol.sumas,
             xpolicy.frqpy = p_wfpol.frqpay,
             xpolicy.instprm = p_wfpol.tinstprm,
             xpolicy.annprm = p_wfpol.tannprm
           WHERE polno = p_wfpol.polno
  
       IF STATUS < 0   THEN
          OUTPUT TO REPORT alt_post (p_mpol.polno)
       END IF
    END IF 
  END IF 

END FUNCTION

FUNCTION upd_xpols()
    IF p_wfpol.endcd IS NULL  THEN
      LET p_wfpol.endcd = "00"
      LET v_enddt = NULL
    ELSE
      LET v_enddt = p_wfpol.movdt
#      IF p_wfpol.endcd = "02" AND p_wfpol.poltype <> '4PAA'  THEN	#paa
      IF p_wfpol.endcd = "02" AND NOT
	     (p_wfpol.polno > 32800000 AND p_wfpol.frqpay <> 'S') THEN
         LET p_planchg = "Y"
      ELSE
         LET p_planchg = p_mpols.chgind
      END IF
    END IF
       
    UPDATE xpolicys
       SET xpolicys.bankac = p_wfpol.bankac,
           xpolicys.anx1  = p_wfpol.annex1,
           xpolicys.anx2  = p_wfpol.annex2,
           xpolicys.anx3  = p_wfpol.annex3,
           xpolicys.anx4  = p_wfpol.annex4,
           xpolicys.anx5  = p_wfpol.annex5,
           xpolicys.anx6  = p_wfpol.annex6,
           xpolicys.anx7  = p_wfpol.annex7,
           xpolicys.anx8  = p_wfpol.annex8,
           xpolicys.anx9  = p_wfpol.annex9,
           xpolicys.anx10 = p_wfpol.annex10,
           xpolicys.anx11 = p_wfpol.annex11,
           xpolicys.anx12 = p_wfpol.annex12,
           xpolicys.anx13 = p_wfpol.annex13,
           xpolicys.anx14 = p_wfpol.annex14,
           xpolicys.anx15 = p_wfpol.annex15,
           xpolicys.anx16 = p_wfpol.annex16,
           xpolicys.anx17 = p_wfpol.annex17,
           xpolicys.anx18 = p_wfpol.annex18,
           xpolicys.anx19 = p_wfpol.annex19,
           xpolicys.anx20 = p_wfpol.annex20,
           planchg  = p_planchg,
           commctg1 = g_comctga,
           commyrs1 = 8,		#p_comyra,
           lst_entdt = v_enddt,
           lst_entcd = p_wfpol.endcd
        WHERE polno = p_wfpol.polno

  IF STATUS < 0   THEN
     OUTPUT TO REPORT alt_post (p_mpol.polno)
  END IF
END FUNCTION

FUNCTION upd_xpols1()

    UPDATE xpolicys
       SET lst_entdt = NULL, 
           lst_entcd = NULL 
        WHERE polno = p_wfpol.polno

  IF STATUS < 0   THEN
     OUTPUT TO REPORT alt_post (p_mpol.polno)
  END IF
END FUNCTION

FUNCTION upd_xpols2()

  IF p_wfpol.movcd <> "40" AND p_wfpol.movcd <> "39" AND 
     p_wfpol.movcd <> "09" AND
     p_wfpol.movcd <> "48" AND p_wfpol.movcd <> "18"   THEN
    UPDATE xpolicys
       SET accbns = p_wfpol.acumbon, 
           cash = p_wfpol.boncsh, 
           revind = p_wfpol.rev 
        WHERE polno = p_wfpol.polno
  END IF

  IF STATUS < 0   THEN
     OUTPUT TO REPORT alt_post (p_mpol.polno)
  END IF
END FUNCTION

FUNCTION upd_xpoln()
DEFINE	p_rel	LIKE mulendor.sprel
DEFINE	p_spnation LIKE tsendwfmpolicyo.spnation	-- seq 7 --

  IF p_wfpol.jlind <> "N"   THEN
    # LET p_rel = NULL 			 -- seq 7 --
    INITIALIZE p_rel, p_spnation TO NULL -- seq 7 --

     IF p_wfpol.jlind MATCHES '[PS]' THEN
        SELECT sprel INTO p_rel FROM mulendor WHERE polno = p_wfpol.polno
     END IF
     
     IF p_wfpol.jlind MATCHES '[Ss]' THEN 	--* seq 7 --
        SELECT spnation INTO p_spnation FROM tsendwfmpolicyo WHERE polno = p_wfpol.polno
     END IF					-- seq 7 *--

     SELECT * FROM xpolicyn
       WHERE polno = p_wfpol.polno
	  AND rectyp2 = p_wfpol.jlind 

     IF STATUS = NOTFOUND   THEN
       INSERT INTO xpolicyn VALUES (p_wfpol.polno,    p_wfpol.jlind, 
               p_wfpol.jlname,      p_wfpol.jlicno,   p_wfpol.jlsex,
               p_rel,                p_wfpol.jlage,    p_wfpol.jldob,
               NULL,                NULL,             g_date_ext,
              # p_wfpol.jlsmind,     p_wfpol.jlrace,   NULL, 	-- seq 7 --
               p_wfpol.jlsmind,     p_wfpol.jlrace,   p_spnation, 	-- seq 7 --
               p_wfpol.jlocpcls,    p_wfpol.jlocp,    NULL,
               p_wfpol.jlmarital,   p_wfpol.jlnewicno )
     ELSE
       UPDATE xpolicyn
          SET xpolicyn.name1 = p_wfpol.jlname,
              xpolicyn.icno = p_wfpol.jlicno,
              xpolicyn.newicno = p_wfpol.jlnewicno,
              xpolicyn.sex = p_wfpol.jlsex,
              xpolicyn.agenb = p_wfpol.jlage,
              xpolicyn.race = p_wfpol.jlrace,
              xpolicyn.smkind = p_wfpol.jlsmind,
              xpolicyn.occpcls = p_wfpol.jlocpcls,
              xpolicyn.occup = p_wfpol.jlocp,
              xpolicyn.lamarital = p_wfpol.jlmarital,
              xpolicyn.rellife = p_rel,
              xpolicyn.dob = p_wfpol.jldob
          WHERE xpolicyn.polno = p_wfpol.polno AND 
               xpolicyn.rectyp2 = p_wfpol.jlind 

          IF p_wfpol.jlind MATCHES '[Ss]' THEN 	--* seq 7 --
             UPDATE xpolicyn
	     SET xpolicyn.nation = p_spnation 
	     WHERE xpolicyn.polno = p_wfpol.polno AND
	           xpolicyn.rectyp2 = p_wfpol.jlind
          END IF				-- seq 7 *--
     END IF
  ELSE
    SELECT * FROM xpolicyn
      WHERE polno = p_wfpol.polno
        AND rectyp2 IN ("S","P")

    IF STATUS <> NOTFOUND   THEN
       DELETE FROM xpolicyn
	 WHERE polno = p_wfpol.polno
	   AND rectyp2 IN ("S","P")
    END IF
  END IF
END FUNCTION

FUNCTION upd_mulunit()
#paa revival
-- seq 33 starts --
{
    IF p_wfpol.polno > 32800000 AND p_wfpol.frqpay <> 'S' AND
	p_wfpol.movcd = '11' THEN
		UPDATE mulunits SET ncd = p_wfpol.effdt
			WHERE polno = p_wfpol.polno
    END IF
}
-- seq 33 ends --
END FUNCTION

FUNCTION uerror ()
DEFINE l_prompt   CHAR (1)

    OPTIONS PROMPT LINE 15
    LET l_prompt = "N"
    WHILE l_prompt <> "Y"
	PROMPT "(uend2x20)  Program ERROR !" FOR l_prompt
    END WHILE
    EXIT PROGRAM
END FUNCTION

FUNCTION trans_endor()
# DEFINE	f_endno		LIKE hulendor.endtno
DEFINE l_amtappl    LIKE msuspmas.amtappl,
       l_amt        LIKE xreceipt2.amt,
       l_frqpay     LIKE mpolicy.frqpay, 
       l_modpay     LIKE mpolicy.modpay, 
       l_collat     LIKE mpolicy.collat, 
       l_tinstprm   LIKE mpolicy.tinstprm, 
       l_laname     LIKE mpolicy.laname,
       l_indsam     LIKE tsendwfmpolicyo.indsam
DEFINE l_promo	   RECORD LIKE tsendpromo.*   --seq 29--

INITIALIZE l_promo.* TO NULL   --seq 29--

    DECLARE c_endor1 CURSOR FOR
	SELECT * FROM mulendor 
	    WHERE postflg = 'P'
	    AND movdt = g_date_ext -- seq 9 --
	    and polno = g_polno --kit

    FOREACH c_endor1 INTO g_endor.*
         -- seq 5 --
       #INSERT INTO hulendor VALUES (g_endor.*, g_date_ext)	#, f_endno)							-- seq 14 --
	LET g_endor.date_ext = g_date_ext	-- seq 14 --
	INSERT INTO hulendor VALUES (g_endor.*)	-- seq 14 --

	DECLARE c_endcv1 CURSOR FOR
	    SELECT * FROM mulendcv
		WHERE polno = g_endor.polno
	    ORDER BY covercd  --seq 30--

	FOREACH c_endcv1 INTO g_endcv.*
            -- seq 5 --
	    INSERT INTO hulendcv VALUES (g_endcv.*, g_date_ext)	#, f_endno)

           SELECT DISTINCT covercd	--seq 26--
	     FROM mtnbzchkcode3_tmp	--seq 26--
	     WHERE covercd = g_endcv.covercd
           IF STATUS <> NOTFOUND THEN	--seq 26--
	    IF g_endcv.loadid IS NULL OR g_endcv.loadid = "" THEN --seq 28--
	      CALL mmc2upgrd()     --seq 23--
	    END IF --seq 28--
	   END IF			--seq 26--
        #--* seq 54 --
        IF g_endor.opid ="MEDRep" THEN 
            IF chk_rept() THEN
                CALL rept_upd()
            END IF
        END IF
        #-- seq 54 *--
	END FOREACH
	
	    --* seq 47 --
    #Transfer data from mulendcv3s to hulendcv3s
    DECLARE c_endcv3s CURSOR FOR
	    SELECT * FROM mulendcv3s
		WHERE polno = g_endor.polno
	    ORDER BY covercd

	FOREACH c_endcv3s INTO g_endcv3s.*
	    INSERT INTO hulendcv3s VALUES (g_endcv3s.*, g_date_ext)
	END FOREACH	
        -- seq 47 *--
	--seq 29 start--
	IF g_endor.endtype = 'Q' THEN  
		LET l_promo.polno = g_endor.polno
		LET l_promo.covercd = "5UPTH"
		LET l_promo.movdt = g_endor.movdt
		LET l_promo.stats = "A"
		LET l_promo.tran_grp = "PMM"
		LET l_promo.tran_cd = "UB"
		LET l_promo.ind1 = "Y"

	      SELECT UNIQUE "X" FROM tsendpromo
	             WHERE polno = g_endor.polno
	      IF STATUS = NOTFOUND THEN
		INSERT INTO tsendpromo VALUES (l_promo.*)
	      ELSE
		UPDATE tsendpromo
		  SET movdt_org = g_date_ext,
		      stats = l_promo.stats,
		      movdt = g_endor.movdt
		WHERE polno = g_endor.polno
	      END IF
	END IF
	--seq 29 end--
	-- seq 6 --
	-- outstanding program screen
        SELECT mendreqh.* INTO p_mendreqh.*
        FROM mendreqh
        WHERE polno = g_endor.polno

       -- seq 5 -- 
       -- seq 6 --

       SELECT sum(amtappl) INTO l_amtappl 
       FROM msuspmas
       WHERE msuspmas.refno = g_endor.polno

       IF STATUS = NOTFOUND OR l_amtappl IS NULL THEN
          LET l_amtappl = 0
       END IF

       SELECT sum(xreceipt2.amt) INTO l_amt 
       FROM xreceipt2
       WHERE xreceipt2.polno = g_endor.polno
       AND xreceipt2.appncd[1,1] = "9"

       IF STATUS = NOTFOUND OR l_amt IS NULL THEN
          LET l_amt = 0
       END IF

       LET l_amtappl = l_amtappl + l_amt

       SELECT frqpay,modpay,collat,tinstprm,laname
         INTO l_frqpay, l_modpay, l_collat, l_tinstprm, l_laname
       FROM mpolicy
       WHERE polno = g_endor.polno
       
       SELECT indsam into l_indsam
       FROM tsendwfmpolicyo
       where polno = g_endor.polno

       
      INSERT INTO hendreqh (polno, remark, amtreq, 
                            trandt, endtno, opid, 
			    laname, suspamt, tinstprm, 
			    npdd, effdt, estatus, 
			    uwid,collat,frqpay,
			    modpay,summul,indsam,
			    remark2,statcase,uwopid,
                            cddate,endtype,curr_tinstprm)

       VALUES (g_endor.polno, p_mendreqh.remark, p_mendreqh.amtreq, 
               g_date_ext, g_endor.endtno,  g_endor.opid, 
	       l_laname, l_amtappl, g_endor.tinsprm,
	       g_endor.npd, g_endor.effdt, g_endor.estatus, 
	       g_endor.uwid,l_collat, l_frqpay,
	       l_modpay,g_endor.summul, l_indsam,
	       p_mendreqh.remark2,g_endor.estatus, g_endor.uwid,
	       p_mendreqh.cddate,g_endor.endtype, l_tinstprm) 

        {INSERT INTO hendreqh VALUES 
	   (p_mendreqh.polno,
	    p_mendreqh.remark,
	    p_mendreqh.amtreq,
	    g_date_ext, 
	    g_endor.endtno,
	    p_mendreqh.remark2,
	    p_mendreqh.remark3,
	    p_mendreqh.statcase,
	    p_mendreqh.uwopid,
	    p_mendreqh.cddate
	    )
	    }

        DECLARE c_reqd CURSOR FOR

        SELECT * FROM mendreqd
        WHERE polno = g_endor.polno

        FOREACH c_reqd INTO p_mendreqd.*
           INSERT INTO hendreqd VALUES 
	   (p_mendreqd.polno,
	    p_mendreqd.outcd,
	    p_mendreqd.recind,
	    p_mendreqd.dtrec,
	    p_mendreqd.type,
	    p_mendreqd.descrpt,
	    g_endor.endtno,
	    p_mendreqd.opid,
	    p_mendreqd.upddt,
	    p_mendreqd.rmkind,
	    p_mendreqd.live_type,
	    p_mendreqd.paycd
	    )

        END FOREACH

	-- seq 6  --
        DECLARE cursor_msendmisc CURSOR FOR
        Select *
        FROM msendmisc
        WHERE polno = g_endor.polno

        FOREACH cursor_msendmisc INTO p_msendmisc.*

           INSERT INTO mshendmisc
           VALUES(p_msendmisc.polno,
                  p_msendmisc.type1,
                  p_msendmisc.type2,
                  p_msendmisc.type3,
                  p_msendmisc.type4,
                  p_msendmisc.type5,
                  p_msendmisc.ind,
                  p_msendmisc.opid,
                  p_msendmisc.outcd,
                  p_msendmisc.laname,
                  p_msendmisc.laicno,
                  p_msendmisc.live_type,
                  g_endor.endtno,g_date_ext)

         END FOREACH 

        DECLARE cursor_msendoutrmk CURSOR FOR
        SELECT *
        FROM msendoutrmk
        WHERE polno = g_endor.polno

        FOREACH cursor_msendoutrmk INTO p_msendoutrmk.*

           INSERT INTO mshendoutrmk VALUES (p_msendoutrmk.polno,
                                            p_msendoutrmk.outcd,
                                            p_msendoutrmk.num,
                                            p_msendoutrmk.rmk,
                                            p_msendoutrmk.live_type,
                                            g_endor.endtno,g_date_ext)

        END FOREACH

        DECLARE cur_msendtextrmk CURSOR FOR
        SELECT *
        FROM msendtextrmk
        WHERE polno = g_endor.polno

        FOREACH cur_msendtextrmk INTO p_msendtextrmk.*

           INSERT INTO mshendtextrmk VALUES ( p_msendtextrmk.polno,
                                              p_msendtextrmk.outcd,
                                              p_msendtextrmk.rmk0,
                                              p_msendtextrmk.rmk1,
                                              p_msendtextrmk.rmk2,
                                              p_msendtextrmk.rmk3,
                                              p_msendtextrmk.rmk4,
                                              p_msendtextrmk.rmk5,
                                              p_msendtextrmk.rmk6,
                                              p_msendtextrmk.rmk7,
                                              p_msendtextrmk.rmk8,
                                              p_msendtextrmk.rmk9,
                                              p_msendtextrmk.rmk10,
                                              p_msendtextrmk.rmk11,
                                              p_msendtextrmk.rmk12,
                                              p_msendtextrmk.rmk13,
                                              p_msendtextrmk.rmk14,
                                              p_msendtextrmk.rmk15,
                                              p_msendtextrmk.rmk16,
                                              p_msendtextrmk.live_type,
                                              g_endor.endtno,g_date_ext) 

        END FOREACH	

   END FOREACH 


END FUNCTION

REPORT alt_post (r_polno)
DEFINE	r_polno		INTEGER

   OUTPUT
      LEFT MARGIN 0
 
   FORMAT

      PAGE HEADER
         PRINT "Date : ",TODAY,
              
	       -- seq 20 --
	       #COLUMN 28,"PRUDENTIAL ASSURANCE MALAYSIA BHD",
               #COLUMN 28,"PRUDENTIAL BSN TAKAFUL BERHAD", --seq 21--
               COLUMN 28,g_company,
               COLUMN 69,"PAGE : ",PAGENO USING"###"
         SKIP 1 LINE
         PRINT "ERROR ALTERATION POSTING (uend2x20)"
         SKIP 1 LINE
         -- seq 20 --
	 #PRINT "      POLICY NO. " 
         PRINT "      CERTIFICATE NO. " 
         PRINT "________________"
         SKIP 1 LINE

      ON EVERY ROW 
         PRINT COLUMN 6, r_polno USING "##########"
 
      ON LAST ROW
         SKIP 1 LINE
         PRINT "_______________________________________________________________"
         PRINT "GRAND TOTAL # OF ERROR FOUND  = ", COUNT(*)
 
END REPORT


REPORT     err_endos(r_polno, r_movcd, r_ofmovcd )

  DEFINE      r_polno   LIKE  mpolicy.polno ,
              r_movcd   LIKE  wfmpolicy.movcd,
              r_ofmovcd LIKE  wfmpolicy.ofmovcd,
              r_amt     INTEGER

    OUTPUT
      LEFT MARGIN 0

   FORMAT


      PAGE HEADER
         PRINT "Date : ",TODAY,
               -- seq 20 --
	       #COLUMN 28,"PRUDENTIAL ASSURANCE MALAYSIA BHD",
               COLUMN 28,"PRUDENTIAL BSN TAKAFUL BERHAD",
               COLUMN 69,"PAGE : ",PAGENO USING"###"
         SKIP 1 LINE
         PRINT "ERROR ALTERATION POSTING (uend2x20)"
         SKIP 1 LINE
         -- seq 20 --
	 #PRINT "      POLICY NO.           MOVCD           OFF MOVCD "
         PRINT "      CERT NO.           MOVCD           OFF MOVCD "
         PRINT "_____________________________________________________"
         SKIP 1 LINE
         LET r_amt = 0 

      ON EVERY ROW
         LET r_amt = r_amt + 1 
         PRINT COLUMN 6, r_polno USING "##########" , "               ", 
                         r_movcd, "           ", r_ofmovcd

      ON LAST ROW
         SKIP 1 LINE
         PRINT "_______________________________________________________________"
         PRINT "GRAND TOTAL # OF ERROR FOUND  = ", r_amt

END REPORT

--seq 26 start--
FUNCTION create_tmp()
#***************************************************
CREATE TEMP TABLE tmp_xmmc2upgrd
  (
    polno	INTEGER,
    endtno	SMALLINT,
    covercd	CHAR(8),
    loadid	CHAR(3),
    incepdt	DATE,
    osa		INTEGER,
    nsa		INTEGER,
    oannprem	DECIMAL(10,2),
    nannprem	DECIMAL(10,2),
    trno	SMALLINT,
    premdiff	DECIMAL(10,2),
    date_ext	DATE,
    time_ext    DATETIME  hour to second,
    category	CHAR(1),
    chr1	VARCHAR(20),
    chr2	VARCHAR(20),
    dt1		DATE,
    dt2		DATE,
    int1	INTEGER,
    int2	INTEGER
  ) WITH NO LOG;

CREATE TEMP TABLE mtnbzchkcode3_tmp
( covercd    CHAR(8)
 ) WITH NO LOG;

 INSERT INTO mtnbzchkcode3_tmp
 SELECT DISTINCT usercd1
   FROM mtnbzchkcode3
   WHERE systype = 'MMC2UPGRD'
     AND syscode = 'COVERCD' ;

 INSERT INTO mtnbzchkcode3_tmp
  SELECT DISTINCT usercd2
   FROM mtnbzchkcode3
   WHERE systype = 'MMC2UPGRD'
     AND syscode = 'COVERCD' 
     AND usercd2 NOT IN ( SELECT DISTINCT usercd1
			    FROM mtnbzchkcode3
			   WHERE systype = 'MMC2UPGRD'
			     AND syscode = 'COVERCD')
     AND (usercd2 IS NOT NULL 
         OR usercd2 <> " ");

END FUNCTION

#--* seq 54 --
FUNCTION chk_rept()
DEFINE f_rec_cnt INTEGER
    LET f_rec_cnt = 0
    IF g_endcv.loadid is NULL OR g_endcv.loadid = " " THEN
        SELECT count(*) INTO f_rec_cnt
          FROM xreptrand
          WHERE polno = g_endor.polno
            AND endtno = g_endcv.endtno
            AND trno = g_endcv.trno
            AND covercd = g_endcv.covercd
            AND loadid IS NULL
            AND category IS NOT NULL
    ELSE
        SELECT count(*) INTO f_rec_cnt
          FROM xreptrand
          WHERE polno = g_endor.polno
            AND endtno = g_endcv.endtno
            AND trno = g_endcv.trno
            AND covercd = g_endcv.covercd
            AND loadid = g_endcv.loadid
            AND category IS NOT NULL
           
    END IF
    
    IF f_rec_cnt <> 1 THEN
        RETURN FALSE
    ELSE
        RETURN TRUE
    END IF
END FUNCTION

FUNCTION rept_upd()
DEFINE f_xreptrand RECORD LIKE xreptrand.*
DEFINE f_reptrand RECORD LIKE reptrand.*
DEFINE f_histseq  INTEGER

    INITIALIZE f_xreptrand.* TO NULL
    LET f_histseq = 1
    IF g_endcv.loadid is NULL OR g_endcv.loadid = " " THEN
        SELECT * INTO f_xreptrand.*
          FROM xreptrand
          WHERE polno = g_endor.polno
            AND endtno = g_endcv.endtno
            AND trno = g_endcv.trno
            AND covercd = g_endcv.covercd
            AND loadid IS NULL
            AND category IS NOT NULL
            
        SELECT * INTO f_reptrand.*
          FROM reptrand
         WHERE polno = f_xreptrand.polno
           AND covercd = f_xreptrand.covercd
           AND endtno = f_xreptrand.endtno
           AND trno = f_xreptrand.trno
           AND loadid IS NULL
    ELSE
        SELECT * INTO f_xreptrand.*
          FROM xreptrand
          WHERE polno = g_endor.polno
            AND endtno = g_endcv.endtno
            AND trno = g_endcv.trno
            AND covercd = g_endcv.covercd
            AND loadid = g_endcv.loadid
            AND category IS NOT NULL
            
        SELECT * INTO f_reptrand.*
          FROM reptrand
         WHERE polno = f_xreptrand.polno
           AND covercd = f_xreptrand.covercd
           AND endtno = f_xreptrand.endtno
           AND trno = f_xreptrand.trno
           AND loadid = f_xreptrand.loadid
    END IF
       
    IF STATUS = NOTFOUND THEN
    #Insert new record
        INSERT INTO reptrand(polno, endtno, covercd, loadid, lodrate, benterm, 
               occls, incepdt, osa, nsa, oannprem, nannprem, 
              compyble, lpdd, trno, premdiff, slkrem, prmred,
              trndt, date_ext, chr1) 
        VALUES(g_endcv.*, g_date_ext, f_xreptrand.category);
        
        INSERT INTO reptrandh(polno, endtno, covercd, loadid, lodrate, benterm, 
                              occls, incepdt, osa, nsa, oannprem, nannprem, 
                              compyble, lpdd, trno, premdiff, slkrem, prmred,
                              trndt, date_ext, chr1, histseq) 
            VALUES(g_endcv.*, g_date_ext, f_xreptrand.category, f_histseq ); 
    ELSE
    #Update existing
        IF f_xreptrand.loadid is NULL OR f_xreptrand.loadid = " " THEN
           UPDATE reptrand
           SET lodrate = g_endcv.lodrate, 
               benterm = g_endcv.benterm , 
               occls   = g_endcv.occls, 
               incepdt = g_endcv.incepdt, 
               osa     = g_endcv.osa, 
               nsa     = g_endcv.nsa, 
               oannprem = g_endcv.oannprem, 
               nannprem = g_endcv.nannprem , 
               compyble = g_endcv.compyble, 
               lpdd   = g_endcv.lpdd, 
               premdiff = g_endcv.premdiff, 
               slkrem = g_endcv.slkrem, 
               prmred = g_endcv.prmred,
               trndt = g_endcv.trndt, 
               date_ext = g_date_ext, 
               chr1 = f_xreptrand.category
             WHERE polno = f_xreptrand.polno
               AND covercd = f_xreptrand.covercd
               AND endtno = f_xreptrand.endtno
               AND trno = f_xreptrand.trno
               AND loadid IS NULL
               
           SELECT MAX(histseq) INTO f_histseq FROM reptrandh
            WHERE polno = f_xreptrand.polno
              AND covercd = f_xreptrand.covercd
              AND endtno = f_xreptrand.endtno
              AND trno = f_xreptrand.trno
              AND loadid IS NULL
            
            
        ELSE
           UPDATE reptrand
           SET lodrate = g_endcv.lodrate, 
               benterm = g_endcv.benterm , 
               occls   = g_endcv.occls, 
               incepdt = g_endcv.incepdt, 
               osa     = g_endcv.osa, 
               nsa     = g_endcv.nsa, 
               oannprem = g_endcv.oannprem, 
               nannprem = g_endcv.nannprem , 
               compyble = g_endcv.compyble, 
               lpdd   = g_endcv.lpdd, 
               premdiff = g_endcv.premdiff, 
               slkrem = g_endcv.slkrem, 
               prmred = g_endcv.prmred,
               trndt = g_endcv.trndt, 
               date_ext = g_date_ext , 
               chr1 = f_xreptrand.category
             WHERE polno = f_xreptrand.polno
               AND covercd = f_xreptrand.covercd
               AND endtno = f_xreptrand.endtno
               AND trno = f_xreptrand.trno
               AND loadid  = f_xreptrand.loadid
               
               
           SELECT MAX(histseq) INTO f_histseq FROM reptrandh
            WHERE polno = f_xreptrand.polno
               AND covercd = f_xreptrand.covercd
               AND endtno = f_xreptrand.endtno
               AND trno = f_xreptrand.trno
               AND loadid  = f_xreptrand.loadid
        
        END IF
        
        IF f_histseq IS NULL THEN  LET f_histseq = 0  END IF
        
        LET f_histseq = f_histseq + 1
        INSERT INTO reptrandh(polno, endtno, covercd, loadid, lodrate, benterm, 
                              occls, incepdt, osa, nsa, oannprem, nannprem, 
                              compyble, lpdd, trno, premdiff, slkrem, prmred,
                              trndt, date_ext, chr1, histseq)
            VALUES(g_endcv.*, g_date_ext, f_xreptrand.category, f_histseq);
    END IF

END FUNCTION
#-- seq 54 *--

FUNCTION mmc2upgrd()
#***************************************************
DEFINE f_xmmc2upgrd  RECORD LIKE xmmc2upgrd.*,
       f_mmc2upgrd   RECORD LIKE mmc2upgrd.*,
       f_delmmc      RECORD LIKE mmc2upgrd.*,
       bak_xmmc2upgrd RECORD LIKE xmmc2upgrd.*,
       l_mmccnt      SMALLINT,
       f_xmmc2cover  CHAR(5),
       l_rowid       INTEGER
DEFINE l_stmt        CHAR(300) --seq 30--

       INITIALIZE f_xmmc2upgrd.* TO NULL
	SELECT * INTO f_xmmc2upgrd.*
	  FROM xmmc2upgrd
	  WHERE polno = g_endor.polno
	    AND endtno = g_endcv.endtno
	    AND trno = g_endcv.trno
	    AND covercd = g_endcv.covercd
	    AND loadid IS NULL
	    AND category IS NOT NULL --seq 30--
    
	IF STATUS <> NOTFOUND THEN
	  --seq 30 start--
	  IF  f_xmmc2upgrd.covercd = "5UPTH" AND f_xmmc2upgrd.category = "N" THEN
	  ELSE 
	  --seq 30 end--
	    SELECT * INTO f_mmc2upgrd.*
	      FROM mmc2upgrd
	     WHERE polno = f_xmmc2upgrd.polno
	       AND covercd = f_xmmc2upgrd.covercd
	       AND endtno = f_xmmc2upgrd.endtno
	       AND trno = f_xmmc2upgrd.trno
	     
	     IF STATUS = NOTFOUND THEN
	     	--insert into mmc2upgrd
	        INSERT INTO mmc2upgrd(polno, endtno, covercd, loadid, lodrate, benterm, 
		                      occls, incepdt, osa, nsa, oannprem, nannprem, 
				      compyble, lpdd, trno, premdiff, slkrem, prmred,
				       trndt, date_ext, chr1) 
		VALUES(g_endcv.*, g_date_ext, f_xmmc2upgrd.category);

		--insert into history table mmc2upgrdh
	        INSERT INTO mmc2upgrdh(polno, endtno, covercd, loadid, lodrate, benterm, 
		                      occls, incepdt, osa, nsa, oannprem, nannprem, 
				      compyble, lpdd, trno, premdiff, slkrem, prmred,
				       trndt, date_ext, chr1) 
		VALUES(g_endcv.*, g_date_ext, f_xmmc2upgrd.category); 
     
	     END IF

	         INSERT INTO tmp_xmmc2upgrd VALUES (f_xmmc2upgrd.*);

		 DELETE FROM xmmc2upgrd
	          WHERE polno = f_xmmc2upgrd.polno
	            AND covercd = f_xmmc2upgrd.covercd
	            AND endtno = f_xmmc2upgrd.endtno
	            AND trno = f_xmmc2upgrd.trno

	     --if more than 2 records, must delete min--

                 LET l_mmccnt = 0
		 LET f_xmmc2cover = f_xmmc2upgrd.covercd[2,5] CLIPPED
		--seq 30 start--
	    
		IF f_xmmc2upgrd.covercd = "5UPTH" OR f_xmmc2upgrd.covercd MATCHES '*PMM*' THEN  
		   LET l_stmt = " SELECT * ,ROWID ",
		              " FROM mmc2upgrd ",
			      " WHERE polno = ", f_xmmc2upgrd.polno ,  
			     # " AND covercd[2,4] in ('UPTH','UPMM') ",  --seq 31--
			      " AND ( covercd = '5UPTH'  ",  --seq 31--
			      " OR covercd[2,5] in ('UPMM')) ", --seq 31--
			      "  ORDER BY date_ext DESC, rowid DESC "
		ELSE 
		   LET l_stmt = " SELECT * ,ROWID ",
		              " FROM mmc2upgrd ",
			      " WHERE polno = ", f_xmmc2upgrd.polno ,  
			      " AND covercd[2,5] = '", f_xmmc2cover, "'",
			      "  ORDER BY date_ext DESC, rowid DESC "
		END IF

		--* seq 42 --
		IF check_got2mmc() THEN
		   LET l_stmt = " SELECT * ,ROWID ",
		              " FROM mmc2upgrd ",
			      " WHERE polno = ", f_xmmc2upgrd.polno ,  
			      " AND covercd[2,5] = '", f_xmmc2cover, "'",
			      "  ORDER BY date_ext DESC, rowid DESC "
		END IF
		-- seq 42 *--
		
		INITIALIZE f_delmmc.* TO NULL
		PREPARE c_mmcupth FROM l_stmt
		DECLARE c_mmccnt CURSOR FOR c_mmcupth
		 --seq 30 end--
		{ DECLARE c_mmccnt CURSOR FOR
		   SELECT * ,ROWID
	             FROM mmc2upgrd
	            WHERE polno = f_xmmc2upgrd.polno
	              AND covercd[2,5] = f_xmmc2cover
		    ORDER BY date_ext DESC, rowid DESC} --seq 30--

		 FOREACH c_mmccnt INTO  f_delmmc.*,l_rowid

		       LET l_mmccnt = l_mmccnt + 1   

		       IF l_mmccnt > 2 THEN
		         --before delete make sure have backup data in mmc2upgrdh

			 SELECT * FROM mmc2upgrdh
			 WHERE polno = f_delmmc.polno
			   AND covercd = f_delmmc.covercd
			   AND endtno = f_delmmc.endtno
			   AND trno =  f_delmmc.trno
			   AND date_ext = f_delmmc.date_ext 
			
			  IF STATUS <> NOTFOUND THEN
		             DELETE FROM mmc2upgrd
			      WHERE polno = f_delmmc.polno
			        AND covercd = f_delmmc.covercd
			        AND endtno = f_delmmc.endtno
			        AND trno = f_delmmc.trno
			        AND date_ext = f_delmmc.date_ext
			  ELSE
				INSERT INTO mmc2upgrdh VALUES (f_delmmc.*);
		               
				DELETE FROM mmc2upgrd
			         WHERE polno = f_delmmc.polno
			           AND covercd = f_delmmc.covercd
			           AND endtno = f_delmmc.endtno
			           AND trno = f_delmmc.trno
			           AND date_ext = f_delmmc.date_ext
			  END IF 
		       END IF --l_mmccnt > 2
		 END FOREACH
	  END IF --seq 30--
	END IF --status <> notfound
        

END FUNCTION

FUNCTION f_unload()
#**********************************************************
 DEFINE f_textfile   CHAR(50)
   

   LET f_textfile = '/data/dl/xmmc2upgrd.',g_date_ext USING "ddmm"
    UNLOAD TO f_textfile
    SELECT * FROM tmp_xmmc2upgrd;

END FUNCTION
--seq 26 end--

{--seq 26 start--
--seq 23--
FUNCTION mmc2upgrd()
DEFINE f_mmc2upgrd RECORD LIKE mmc2upgrd.*, 
       f_premdiff LIKE hulendcv.premdiff, 
       f_count, f_ins, f_endtno, f_status, f_cnt  SMALLINT, 
       f_null  CHAR(1), 
       f_hulendcv RECORD LIKE hulendcv.*, 
       f_cnt1  SMALLINT  --seq 24--  

#IF g_endcv.covercd = "5UPMM5"  AND g_endcv.loadid IS NULL THEN ---5upmm5 --seq 24-- 
IF g_endcv.covercd = "5UPMM5"  AND g_endcv.loadid IS NULL  		  --seq 24-- 
   AND g_endcv.premdiff <> 0 THEN 					  --seq 24-- 
   LET f_ins = 0
   LET f_count = 0
   LET f_null = NULL 
   SELECT count(*) 
     INTO f_cnt 
     FROM hulpola 
    WHERE polno = g_endcv.polno 
      AND cover IN ("5UPMM3", "5UPMM4") 
   
   IF f_cnt > 0 AND f_cnt IS NOT NULL THEN --shld have a previous pmm 
      IF g_endcv.covercd = "5UPMM5" AND g_endcv.nannprem > 0 THEN --add pmm5 

         SELECT * 
	   INTO f_mmc2upgrd.* 
   	   FROM mmc2upgrd
	  WHERE polno = g_endcv.polno 
	    AND covercd = "5UPMM5" 

         IF STATUS <> NOTFOUND THEN 
   	    INSERT INTO mmc2upgrdh
	    VALUES(f_mmc2upgrd.*)
	    LET f_count = 1 
	 END IF 

	 IF f_count = 1  THEN 	  --existing record, backup the data  	 
	    DELETE FROM mmc2upgrd
            WHERE polno =  g_endcv.polno 
	    AND covercd = "5UPMM5" 
	 ELSE 
  	    LET f_ins = 1     --record not found, so have to insert in mmc2proc 
         END IF 

	 INSERT INTO mmc2upgrd(polno, endtno, covercd, loadid, lodrate, benterm, 
				occls, incepdt, osa, nsa, oannprem, nannprem, 
				compyble, lpdd, trno, premdiff, slkrem, prmred,
				trndt, date_ext) 
	 VALUES(g_endcv.*, g_date_ext)  --for pmm5 only 

	 IF f_ins = 1 THEN    --insert the old pmm 

	    SELECT polno, endtno, covercd, loadid, lodrate, benterm, 
		   occls, incepdt, osa, nsa, oannprem, nannprem, 
		   compyble, lpdd, trno, premdiff, slkrem, prmred,
		   trndt
	      INTO f_hulendcv.polno, f_hulendcv.endtno, f_hulendcv.covercd, 
	           f_hulendcv.loadid, f_hulendcv.lodrate, f_hulendcv.benterm, 
		   f_hulendcv.occls, f_hulendcv.incepdt, f_hulendcv.osa, f_hulendcv.nsa, 
		   f_hulendcv.oannprem, f_hulendcv.nannprem, f_hulendcv.compyble, 
		   f_hulendcv.lpdd, f_hulendcv.trno, f_hulendcv.premdiff, f_hulendcv.slkrem, 
		   f_hulendcv.prmred, f_hulendcv.trndt  
	      FROM mulendcv 
  	     WHERE polno = g_endcv.polno 
	       AND covercd IN ("5UPMM3", "5UPMM4") 
	       AND premdiff < 0 

	    LET f_hulendcv.date_ext = g_date_ext 
	    IF f_hulendcv.polno IS NULL OR f_hulendcv.polno = 0 THEN 
	       SELECT max(endtno) 
	         INTO f_endtno  
	         FROM hulendcv 
  	        WHERE polno = g_endcv.polno 
  	          AND covercd IN ("5UPMM3", "5UPMM4") 
	          AND nannprem = 0 
	       SELECT * 
	         INTO f_hulendcv.*  
	         FROM hulendcv 
     	        WHERE polno = g_endcv.polno 
  	          AND covercd IN ("5UPMM3", "5UPMM4") 
	          AND nannprem = 0 
		  AND endtno = f_endtno 
	    END IF 

	    IF f_hulendcv.polno IS NOT NULL THEN 
	       INSERT INTO mmc2upgrd(polno, endtno, covercd, loadid, lodrate, benterm, 
				occls, incepdt, osa, nsa, oannprem, nannprem, 
				compyble, lpdd, trno, premdiff, slkrem, prmred,
				trndt, date_ext) 
	       VALUES(f_hulendcv.*)   
	    END IF 
	 END IF 

      ELSE 		   --nannprem = 0 (dropped)
         #DECLARE c_curmmc2 CURSOR FOR 
         #SELECT * 
	 #FROM mmc2upgrd
	 #WHERE polno = g_endcv.polno 
	 #  AND covercd matches '*PMM*' 

         #FOREACH c_curmmc2 INTO f_mmc2upgrd.* 
	 #   INSERT INTO mmc2upgrdh
	 #    VALUES(f_mmc2upgrd.*)

         #END FOREACH
         --seq 24-- 
         SELECT count(*) 
           INTO f_cnt1 
           FROM mmc2upgrd
          WHERE polno = g_endcv.polno

         IF f_cnt1 > 0 THEN 
         --seq 24-- 
            UPDATE mmc2upgrd
	       SET datedrop = g_date_ext, 
	           effect = g_endor.effdt, 
	           int1 = g_endcv.endtno   --get the endorsement number when it was dropped 
	     WHERE polno =  g_endcv.polno 	   
         END IF  --seq 24-- 

     END IF 

   END IF --f_status 
   IF f_ins = 1 THEN 
      --alloc 
      SELECT DISTINCT polno 
      FROM mmc2proc 
      WHERE polno = g_endcv.polno
        AND proctype IN  ("ALLOC") 
      IF STATUS <> NOTFOUND THEN 
         #UPDATE mmc2proc
	 #SET date_ext = NULL, dt1 = NULL, int1 = NULL 
	 #WHERE polno = g_endcv.polno
	 #AND proctype IN ("ALLOC") 
      ELSE 
         INSERT INTO mmc2proc(polno, proctype)
	 VALUES(g_endcv.polno, "ALLOC")
      END IF 
      --persistency 
      SELECT DISTINCT polno 
      FROM mmc2proc 
      WHERE polno = g_endcv.polno
        AND proctype IN  ("PC") 
      IF STATUS <> NOTFOUND THEN 
         #UPDATE mmc2proc
	 #SET date_ext = NULL, dt1 = NULL, int1 = NULL 
	 #WHERE polno = g_endcv.polno
	 #AND proctype IN ("PC") 
      ELSE 
         INSERT INTO mmc2proc(polno, proctype)
	 VALUES(g_endcv.polno, "PC")
      END IF 
      --aci 
      SELECT DISTINCT polno 
      FROM mmc2proc 
      WHERE polno = g_endcv.polno
        AND proctype IN  ("ACI") 
      IF STATUS <> NOTFOUND THEN 
         #UPDATE mmc2proc
	 #SET date_ext = NULL, dt1 = NULL, int1 = NULL 
	 #WHERE polno = g_endcv.polno
	 #AND proctype IN ("ACI") 
      ELSE 
         INSERT INTO mmc2proc(polno, proctype)
	 VALUES(g_endcv.polno, "ACI")
      END IF 
      --aci3 
      SELECT DISTINCT polno 
      FROM mmc2proc 
      WHERE polno = g_endcv.polno
        AND proctype IN  ("ACI3") 
      IF STATUS <> NOTFOUND THEN 
         #UPDATE mmc2proc
	 #SET date_ext = NULL, dt1 = NULL, int1 = NULL 
	 #WHERE polno = g_endcv.polno
	 #AND proctype IN ("ACI3") 
      ELSE 
         INSERT INTO mmc2proc(polno, proctype)
	 VALUES(g_endcv.polno, "ACI3")
      END IF 
      --comm 
      SELECT DISTINCT polno 
      FROM mmc2proc 
      WHERE polno = g_endcv.polno
        AND proctype IN  ("COMM") 
      IF STATUS <> NOTFOUND THEN 
         #UPDATE mmc2proc
	 #SET date_ext = NULL, dt1 = NULL, int1 = NULL 
	 #WHERE polno = g_endcv.polno
	 #AND proctype IN ("COMM")
      ELSE 
         INSERT INTO mmc2proc(polno, proctype)
	 VALUES(g_endcv.polno, "COMM")
      END IF 

   END IF  --f_ins = 1 

 END IF  --main 

END FUNCTION 
--seq 23--
}--seq 26 end--

--seq 31 start--
FUNCTION chk_mmc2upgrd ()
#-----------------------
 DEFINE l_polno INTEGER,
	l_stmt  CHAR(500),
	l_mmc   RECORD LIKE mmc2upgrd.*,
	l_chkcnt SMALLINT

 LET l_polno = NULL
 DECLARE cur_chk CURSOR FOR
    SELECT polno
     FROM mmc2upgrd
    GROUP BY polno
    HAVING count(polno) > 2
 FOREACH cur_chk INTO l_polno
   # for rider other than pmm and upth
   LET l_chkcnt = 0
   DECLARE cur_chk1 CURSOR FOR

--seq 37--
  {   SELECT *, rowid
      FROM mmc2upgrd
      WHERE polno = l_polno
      AND covercd[2,5] NOT IN ('UPMM','UPTH')
      ORDER BY date_ext DESC, rowid DESC
  }
      SELECT *, rowid
      FROM mmc2upgrd
      WHERE polno = l_polno
      AND
      ( covercd not matches '*UPMM'   and
        covercd not matches '*UPTH'   and
        covercd not matches '*UPTHD*' and
        covercd not matches '*UPTHE' )
      ORDER BY date_ext DESC, rowid DESC

--seq 37*--

    INITIALIZE l_mmc.* TO NULL
   FOREACH cur_chk1 INTO l_mmc.*
      LET l_chkcnt = l_chkcnt + 1
      
      IF l_chkcnt > 2 THEN
	  SELECT * FROM mmc2upgrdh
	   WHERE polno = l_mmc.polno
	     AND covercd = l_mmc.covercd
	     AND endtno = l_mmc.endtno
	     AND trno =  l_mmc.trno
	     AND date_ext = l_mmc.date_ext 
			
	    IF STATUS <> NOTFOUND THEN
	        DELETE FROM mmc2upgrd
	         WHERE polno = l_mmc.polno
	           AND covercd = l_mmc.covercd
	           AND endtno = l_mmc.endtno
	           AND trno = l_mmc.trno
	           AND date_ext = l_mmc.date_ext
	     ELSE
	 	INSERT INTO mmc2upgrdh VALUES (l_mmc.*);
		               
		DELETE FROM mmc2upgrd
	         WHERE polno = l_mmc.polno
	           AND covercd = l_mmc.covercd
	           AND endtno = l_mmc.endtno
	           AND trno = l_mmc.trno
	           AND date_ext = l_mmc.date_ext
	  END IF 
      END IF

   END FOREACH --cur_chk1


   # for rider  pmm and upth
   LET l_chkcnt = 0
   DECLARE cur_chk2 CURSOR FOR
--seq 37--

 {    SELECT *, rowid
      FROM mmc2upgrd
      WHERE polno = l_polno
      AND covercd[2,5] IN ('UPMM','UPTH')
     ORDER BY date_ext DESC, rowid DESC
 }
      SELECT *, rowid
      FROM mmc2upgrd
      WHERE polno = l_polno
      AND
      ( covercd matches '*UPMM'   or
        covercd matches '*UPTH'   or
        covercd matches '*UPTHD*' or
        covercd matches '*UPTHE' )
      ORDER BY date_ext DESC, rowid DESC

--seq 37*--

    INITIALIZE l_mmc.* TO NULL
   FOREACH cur_chk2 INTO l_mmc.*
      LET l_chkcnt = l_chkcnt + 1
      
      IF l_chkcnt > 2 THEN
	  SELECT * FROM mmc2upgrdh
	   WHERE polno = l_mmc.polno
	     AND covercd = l_mmc.covercd
	     AND endtno = l_mmc.endtno
	     AND trno =  l_mmc.trno
	     AND date_ext = l_mmc.date_ext 
			
	    IF STATUS <> NOTFOUND THEN
	        DELETE FROM mmc2upgrd
	         WHERE polno = l_mmc.polno
	           AND covercd = l_mmc.covercd
	           AND endtno = l_mmc.endtno
	           AND trno = l_mmc.trno
	           AND date_ext = l_mmc.date_ext
	     ELSE
	 	INSERT INTO mmc2upgrdh VALUES (l_mmc.*);
		               
		DELETE FROM mmc2upgrd
	         WHERE polno = l_mmc.polno
	           AND covercd = l_mmc.covercd
	           AND endtno = l_mmc.endtno
	           AND trno = l_mmc.trno
	           AND date_ext = l_mmc.date_ext
	  END IF 
      END IF

   END FOREACH --cur_chk2

 END FOREACH --cur_chk

END FUNCTION
--seq 31 end--
-- start seq 32 --
FUNCTION ub_prosess ()
DEFINE dt_first DATE

  SELECT DISTINCT "X"
  FROM mtnbzchkcode
  WHERE poltype = "UBONUS"
  AND   usercd1 IS NOT NULL
  AND   usercd6 IS NOT NULL

  IF STATUS = NOTFOUND THEN
      
      LET lt_stm = "SELECT polno, movdt ",
		   " FROM tsendpromo ",
	           " WHERE tran_grp = 'PMM' AND tran_cd = 'UB' ",
	           " AND spchar1 IS NULL ",
	           " ORDER BY polno "

      CALL f_allocate_ub(g_date_ext,lt_stm)
      
      UPDATE mtnbzchkcode
      SET usercd1 ="Y",
          usercd6 = g_date_ext
      WHERE poltype = "UBONUS"
   
  ELSE

      LET lt_stm = "SELECT polno, movdt ",
	           " FROM tsendpromo ",
	           " WHERE tran_grp = 'PMM' AND tran_cd = 'UB' ",
	           " AND movdt BETWEEN  '"CLIPPED,g_date_ext CLIPPED,"' AND '"CLIPPED,g_date_ext CLIPPED,"' ",
	           " AND spchar1 IS NULL ",
	           " ORDER BY polno "

      CALL f_allocate_ub(g_date_ext,lt_stm)
  END IF

END FUNCTION
-- end seq 32 --

--*seq 36 --
FUNCTION ins_xpoln2()

    DECLARE c_wfmpol3 CURSOR FOR
        SELECT * FROM wfmpolicy3
          WHERE polno = p_mpol.polno  AND
                mtype = 'X'

    FOREACH c_wfmpol3 INTO p_wfmpol3.*

        SELECT "X" FROM xpolicyn 
          WHERE polno = p_mpol.polno AND  
                rectyp2 = 'X'

        IF STATUS = NOTFOUND THEN
            INSERT INTO xpolicyn(polno,rectyp2,name1,
                                 icno,sex,rellife,
                                 agenb,dob,ageadm,
                                 indic,date_ext,smkind,
                                 race,nation,occpcls,
                                 occup,percent,lamarital,
                                 newicno)
               VALUES (p_wfmpol3.polno, p_wfmpol3.mtype,p_wfmpol3.mname,
                       p_wfmpol3.icno,  p_wfmpol3.sex,  p_wfmpol3.rel,
                       p_wfmpol3.agenb, p_wfmpol3.dob,  p_wfmpol3.ageadm,
                       p_wfmpol3.indic, p_wfmpol3.date_ext, p_wfmpol3.smkind,
                       p_wfmpol3.race,  p_wfmpol3.nation, p_wfmpol3.occpcls,
                       p_wfmpol3.occup, p_wfmpol3.percent,p_wfmpol3.lamarital,
                       p_wfmpol3.newicno )

            IF STATUS < 0   THEN
               OUTPUT TO REPORT alt_post (p_mpol.polno)
            END IF
        ELSE
           UPDATE xpolicyn SET name1 = p_wfmpol3.mname,icno = p_wfmpol3.icno,
                       sex   = p_wfmpol3.sex, rellife = p_wfmpol3.rel,
                       agenb = p_wfmpol3.agenb, dob =  p_wfmpol3.dob,
                       ageadm = p_wfmpol3.ageadm,indic = p_wfmpol3.indic,
                       date_ext = p_wfmpol3.date_ext,smkind = p_wfmpol3.smkind,
                       race = p_wfmpol3.race,nation  = p_wfmpol3.nation,
                       occpcls =p_wfmpol3.occpcls, occup = p_wfmpol3.occup,
                       percent= p_wfmpol3.percent,lamarital=p_wfmpol3.lamarital,
                       newicno = p_wfmpol3.newicno
           WHERE polno = p_wfmpol3.polno and rectyp2 ='X'
        END IF
    END FOREACH

    DECLARE c_wfmpoln2 CURSOR FOR
        SELECT * FROM wfmpolicyn2
          WHERE polno = p_mpol.polno  

    FOREACH c_wfmpoln2 INTO p_wfmpoln2.*
        SELECT * FROM xpolicyn2 
          WHERE polno = p_mpol.polno 

        IF STATUS = NOTFOUND THEN
               INSERT INTO xpolicyn2 VALUES( p_wfmpoln2.* )
        ELSE
               DELETE FROM xpolicyn2 WHERE polno = p_mpol.polno 
               INSERT INTO xpolicyn2 VALUES( p_wfmpoln2.* )
               IF STATUS < 0   THEN
                   OUTPUT TO REPORT alt_post (p_mpol.polno)
               END IF
         END IF
    END FOREACH

END FUNCTION
--seq 36 *--

--seq 36--
FUNCTION upd_xpoln2()

      DEFINE lr_mul2 RECORD LIKE mulendor2.*
      
      DECLARE c_mul2 CURSOR FOR
        SELECT * FROM mulendor2
          WHERE polno = p_mpol.polno
      FOREACH c_mul2 INTO lr_mul2.*
          UPDATE xpolicyn2 SET annex21 = lr_mul2.annex1,
                               annex22 = lr_mul2.annex2,
                               annex23 = lr_mul2.annex3,
                               annex24 = lr_mul2.annex4,
                               annex25 = lr_mul2.annex5,  
                               annex26 = lr_mul2.annex6,
                               annex27 = lr_mul2.annex7,
                               annex28 = lr_mul2.annex8,
                               annex29 = lr_mul2.annex9,
                               annex30 = lr_mul2.annex10
                 WHERE polno = p_mpol.polno
      END FOREACH 
             
END FUNCTION 

FUNCTION trans_mulendor2()

       INSERT INTO hulendor2 SELECT * FROM mulendor2

END FUNCTION
--seq 36*--

--*seq 38 --
FUNCTION ins_xsendx()
  
    DEFINE p_mendxtra RECORD LIKE mendxtra.*
    DEFINE p_hulendor2 RECORD LIKE hulendor2.*

    DECLARE c_mendxtra CURSOR FOR
        SELECT * FROM mendxtra
          WHERE polno = p_mpol.polno 

    FOREACH c_mendxtra INTO p_mendxtra.*
        SELECT * FROM xsendxtra 
        WHERE polno = p_mpol.polno 

        IF STATUS = NOTFOUND THEN
            INSERT INTO xsendxtra 
                   VALUES (p_mendxtra.polno,   p_mendxtra.date_ext, 
                           p_mendxtra.lasmkyr, p_mendxtra.jlsmkyr,
                           p_mendxtra.jl2smkyr,p_mendxtra.lacigno,
                           p_mendxtra.jlcigno, p_mendxtra.jl2cigno,
                           p_mendxtra.larelgn, p_mendxtra.jlrelgn,
                           p_mendxtra.jl2relgn
                           --* seq 44 --
                           ,p_mendxtra.parelgn
                           -- seq 44 *--
                          )
                                  
            IF STATUS < 0   THEN
               OUTPUT TO REPORT alt_post (p_mpol.polno)
            END IF
        ELSE
            UPDATE xsendxtra SET polno    = p_mendxtra.polno,
                                 date_ext = p_mendxtra.date_ext,
                                 lasmkyr  = p_mendxtra.lasmkyr,
                                 jlsmkyr  = p_mendxtra.jlsmkyr,
                                 jl2smkyr = p_mendxtra.jl2smkyr,
                                 lacigno  = p_mendxtra.lacigno,
                                 jlcigno  = p_mendxtra.jlcigno,
                                 jl2cigno = p_mendxtra.jl2cigno,
                                 larelgn  = p_mendxtra.larelgn,
                                 jlrelgn  = p_mendxtra.jlrelgn,
                                 jl2relgn = p_mendxtra.jl2relgn
                                 --* seq 44 --
                                 ,parelgn = p_mendxtra.parelgn
                                 -- seq 44 *--
              WHERE polno = p_mpol.polno   
        END IF
    END FOREACH
 
END FUNCTION

FUNCTION trans_mendxtra()

       INSERT INTO hendxtra SELECT * FROM mendxtra
       where polno = g_polno --kit

END FUNCTION
--seq 38 *--


--* seq 42 --
FUNCTION check_got2mmc()
  DEFINE f_cnt			INTEGER
  DEFINE f_chk			CHAR(1)
  DEFINE f_date_ext		DATE

  LET f_cnt = 0
  LET f_chk = "N"
  LET f_date_ext = NULL

  SELECT COUNT(*) INTO f_cnt
  FROM mulpola
  WHERE polno = g_endor.polno
  AND (covercd MATCHES "*UPTH"
  OR covercd MATCHES "*PMM*")
  AND xmorind IS NULL

  IF f_cnt = 2 THEN
     LET f_chk = "Y"
  END IF

  IF f_chk = "N" THEN
     LET f_cnt = 0

     DECLARE cur1_hulpola CURSOR FOR
     SELECT DISTINCT date_ext
     FROM hulpola
     WHERE polno = g_endor.polno
     ORDER BY date_ext DESC

     FOREACH cur1_hulpola INTO f_date_ext
        LET f_cnt = 0

        SELECT COUNT(*) INTO f_cnt
	FROM hulpola
	WHERE polno = g_endor.polno
	AND (cover MATCHES "*UPTH"
        OR cover MATCHES "*PMM*")
        AND extracd IS NULL
        AND date_ext = f_date_ext -- seq 43 --

	IF f_cnt = 2 THEN
	   LET f_chk = "Y"
	   EXIT FOREACH
	END IF
     END FOREACH
  END IF

  IF f_chk = "Y" THEN
     RETURN TRUE
  ELSE
     RETURN FALSE
  END IF
END FUNCTION
-- seq 42 *--


-- seq 45 starts --
FUNCTION ins_xsnbzgst()
  DEFINE fr_tsnbzgst		RECORD LIKE tsnbzgst.*

  INITIALIZE fr_tsnbzgst.* TO NULL

  DECLARE c_xsnbzgst CURSOR FOR
  SELECT * FROM tsnbzgst
  WHERE polno = p_mpol.polno
    AND gst_upd = "ENDORSEMENT" -- seq 46 --

  FOREACH c_xsnbzgst INTO fr_tsnbzgst.*
     --* seq 46 --
     LET fr_tsnbzgst.date_ext = g_date_ext
     -- seq 46 *--
     INSERT INTO xsnbzgst VALUES (fr_tsnbzgst.*)
  END FOREACH
END FUNCTION
-- seq 45 ends --

--* seq 48 --
FUNCTION is_3s_uend2x20(fi_poltype)
DEFINE fi_poltype CHAR(8)

    IF fi_poltype MATCHES "3GCP[ABCD]" THEN
        RETURN TRUE
    END IF
    RETURN FALSE
END FUNCTION
-- seq 48 *--
--* seq 49 --
#Function to migrate the data from table tendtad to xsendtad
FUNCTION ins_xsendtad()
  DEFINE fr_tendtad     RECORD LIKE tendtad.*

  INITIALIZE fr_tendtad.* TO NULL

  DECLARE c_xsendtad CURSOR FOR
  SELECT * FROM tendtad
  WHERE polno = p_mpol.polno

  FOREACH c_xsendtad INTO fr_tendtad.*
     LET fr_tendtad.date_ext = g_date_ext
     INSERT INTO xsendtad VALUES (fr_tendtad.*)
     INSERT INTO hendtad VALUES (fr_tendtad.*)
  END FOREACH
END FUNCTION
-- seq 49 *--

--seq 52 starts 
FUNCTION ci_tag()
DEFINE lv_cnt, lv_cnt1 smallint
DEFINE lr_xulcitag RECORD LIKE xulcitag.*
DEFINE lr_mulcitag RECORD LIKE mulcitag.*

   LET lv_cnt = 0
   SELECT count(*) INTO lv_cnt FROM xulcitag 
   WHERE polno = p_wfpol.polno

   IF lv_cnt > 0 THEN 

      DECLARE ci_cur CURSOR FOR 
         SELECT * from xulcitag
         WHERE polno = p_wfpol.polno

      FOREACH ci_cur into lr_xulcitag.*

         INSERT INTO hulcitag values (p_wfpol.polno,lr_xulcitag.endtno,
                    lr_xulcitag.covercd , lr_xulcitag.loadid , 
                    lr_xulcitag.lodrate,lr_xulcitag.trno,lr_xulcitag.ci_ind,
                    lr_xulcitag.trndt, today ) 

         INITIALIZE lr_xulcitag.* TO NULL 

      END FOREACH 

      DELETE from xulcitag
      WHERE polno = p_wfpol.polno

   END IF
   
END FUNCTION 
--seq 52 ends 

--* seq 53 --
FUNCTION is_endt_post_case(fi_polno, fi_movdt)
DEFINE fi_polno INTEGER
DEFINE fi_movdt DATE

DEFINE f_cnt INTEGER

    LET f_cnt = 0
    SELECT COUNT(*)
      INTO f_cnt
      FROM mulendor 
     WHERE polno = fi_polno
       AND postflg = 'P'
       AND movdt = fi_movdt
      
    IF f_cnt > 0 THEN
        RETURN TRUE
    END IF
    
    RETURN FALSE
END FUNCTION

FUNCTION is_occls_null(fi_polno
                       ,fi_covercd
                       ,fi_trno
                       ,fi_xmorind)
DEFINE fi_polno   INTEGER
DEFINE fi_covercd CHAR(8)
DEFINE fi_trno    SMALLINT
DEFINE fi_xmorind CHAR(3)
                      
DEFINE f_chk      INTEGER
DEFINE f_xmorind  CHAR(3)
    
    INITIALIZE f_xmorind TO NULL
    
    LET f_xmorind = fi_xmorind
    IF fi_xmorind IS NULL THEN
        LET f_xmorind = "0"
    END IF
    
    LET f_chk = 0 
    SELECT COUNT(*) 
      INTO f_chk
      FROM mulendcv
     WHERE polno           = fi_polno    #g_wfulpola.polno
       AND covercd         = fi_covercd  #g_wfulpola.covercd
       AND trno            = fi_trno     #g_wfulpola.trno
       AND NVL(loadid,"0") = f_xmorind   #g_wfulpola.xmorind
       AND nsa > 0
       AND occls IS NOT NULL
       
    IF f_chk > 0 THEN
        #occls is not null
        RETURN FALSE
    END IF
    
    RETURN TRUE 
END FUNCTION
-- seq 53 *--

