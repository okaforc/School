	switch (t->back) {
	default: Uerror("bad return move");
	case  0: goto R999; /* nothing to undo */

		 /* PROC :init: */
;
		
	case 3: // STATE 1
		goto R999;

	case 4: // STATE 3
		;
		now.in = trpt->bup.oval;
		;
		goto R999;
;
		;
		
	case 6: // STATE 5
		;
		now.buffer[ Index(now.in, 4) ] = trpt->bup.oval;
		;
		goto R999;

	case 7: // STATE 6
		;
		now.in = trpt->bup.oval;
		;
		goto R999;

	case 8: // STATE 8
		;
		now.in = trpt->bup.oval;
		;
		goto R999;

	case 9: // STATE 13
		;
		now.out = trpt->bup.oval;
		;
		goto R999;

	case 10: // STATE 14
		;
		now.bfull = trpt->bup.oval;
		;
		goto R999;

	case 11: // STATE 15
		;
		now.bempty = trpt->bup.oval;
		;
		goto R999;
;
		;
		
	case 13: // STATE 19
		;
		now.six = trpt->bup.oval;
		;
		goto R999;

	case 14: // STATE 22
		;
		now.six = trpt->bup.oval;
		;
		goto R999;
;
		
	case 15: // STATE 24
		goto R999;

	case 16: // STATE 31
		;
		now.mtx.mstate = trpt->bup.oval;
		;
		goto R999;

	case 17: // STATE 32
		;
		now.cvars[0].cvstate = trpt->bup.oval;
		;
		goto R999;

	case 18: // STATE 33
		;
		now.cvars[1].cvstate = trpt->bup.oval;
		;
		goto R999;

	case 19: // STATE 35
		;
		;
		delproc(0, now._nr_pr-1);
		;
		goto R999;

	case 20: // STATE 36
		;
		;
		delproc(0, now._nr_pr-1);
		;
		goto R999;

	case 21: // STATE 37
		;
		;
		delproc(0, now._nr_pr-1);
		;
		goto R999;

	case 22: // STATE 38
		;
		p_restor(II);
		;
		;
		goto R999;

		 /* PROC consumer */
;
		;
		
	case 24: // STATE 4
		;
		now.mtx.mid = trpt->bup.ovals[1];
		now.mtx.mstate = trpt->bup.ovals[0];
		;
		ungrab_ints(trpt->bup.ovals, 2);
		goto R999;
;
		;
		;
		;
		;
		;
		;
		;
		;
		;
		;
		;
		;
		;
		
	case 32: // STATE 21
		;
		now.out = trpt->bup.oval;
		;
		goto R999;

	case 33: // STATE 22
		;
		cout[ Index(((P1 *)_this)->cno, 2) ] = trpt->bup.oval;
		;
		goto R999;

	case 34: // STATE 23
		;
		now.buffer[ Index(now.out, 4) ] = trpt->bup.oval;
		;
		goto R999;
;
		;
		
	case 36: // STATE 25
		;
		now.bfull = trpt->bup.oval;
		;
		goto R999;

	case 37: // STATE 26
		;
		now.bempty = trpt->bup.oval;
		;
		goto R999;

	case 38: // STATE 28
		;
		now.six = trpt->bup.oval;
		;
		goto R999;

	case 39: // STATE 31
		;
		now.six = trpt->bup.oval;
		;
		goto R999;
;
		
	case 40: // STATE 33
		goto R999;
;
		;
		;
		;
		;
		;
		
	case 44: // STATE 47
		;
		now.mtx.mid = trpt->bup.ovals[1];
		now.mtx.mstate = trpt->bup.ovals[0];
		;
		ungrab_ints(trpt->bup.ovals, 2);
		goto R999;
;
		;
		
	case 46: // STATE 55
		;
		p_restor(II);
		;
		;
		goto R999;

		 /* PROC producer */
;
		;
		
	case 48: // STATE 4
		;
		now.mtx.mid = trpt->bup.ovals[1];
		now.mtx.mstate = trpt->bup.ovals[0];
		;
		ungrab_ints(trpt->bup.ovals, 2);
		goto R999;
;
		;
		;
		;
		;
		;
		;
		;
		;
		;
		;
		;
		;
		;
		
	case 56: // STATE 21
		;
		now.buffer[ Index(now.in, 4) ] = trpt->bup.oval;
		;
		goto R999;
;
		;
		
	case 58: // STATE 23
		;
		now.bempty = trpt->bup.oval;
		;
		goto R999;

	case 59: // STATE 24
		;
		now.bfull = trpt->bup.oval;
		;
		goto R999;

	case 60: // STATE 25
		;
		now.in = trpt->bup.oval;
		;
		goto R999;

	case 61: // STATE 27
		;
		now.six = trpt->bup.oval;
		;
		goto R999;

	case 62: // STATE 30
		;
		now.six = trpt->bup.oval;
		;
		goto R999;
;
		
	case 63: // STATE 32
		goto R999;
;
		;
		;
		;
		;
		;
		
	case 67: // STATE 46
		;
		now.mtx.mid = trpt->bup.ovals[1];
		now.mtx.mstate = trpt->bup.ovals[0];
		;
		ungrab_ints(trpt->bup.ovals, 2);
		goto R999;
;
		;
		
	case 69: // STATE 51
		;
		((P0 *)_this)->p = trpt->bup.oval;
		;
		goto R999;

	case 70: // STATE 55
		;
		p_restor(II);
		;
		;
		goto R999;
	}

