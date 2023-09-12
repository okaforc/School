#define rand	pan_rand
#define pthread_equal(a,b)	((a)==(b))
#if defined(HAS_CODE) && defined(VERBOSE)
	#ifdef BFS_PAR
		bfs_printf("Pr: %d Tr: %d\n", II, t->forw);
	#else
		cpu_printf("Pr: %d Tr: %d\n", II, t->forw);
	#endif
#endif
	switch (t->forw) {
	default: Uerror("bad forward move");
	case 0:	/* if without executable clauses */
		continue;
	case 1: /* generic 'goto' or 'skip' */
		IfNotBlocked
		_m = 3; goto P999;
	case 2: /* generic 'else' */
		IfNotBlocked
		if (trpt->o_pm&1) continue;
		_m = 3; goto P999;

		 /* PROC :init: */
	case 3: // STATE 1 - C:\\Users\\user\\Desktop\\Code\\C\\CS_OS\\practical_3\\writer-and-readers.pml:123 - [printf('A Model of pthreads\\n')] (0:17:0 - 1)
		IfNotBlocked
		reached[2][1] = 1;
		Printf("A Model of pthreads\n");
		/* merge: printf('\\n Producer-Consumer example\\n')(17, 2, 17) */
		reached[2][2] = 1;
		Printf("\n Producer-Consumer example\n");
		_m = 3; goto P999; /* 1 */
	case 4: // STATE 3 - C:\\Users\\user\\Desktop\\Code\\C\\CS_OS\\practical_3\\writer-and-readers.pml:24 - [in = 0] (0:0:1 - 1)
		IfNotBlocked
		reached[2][3] = 1;
		(trpt+1)->bup.oval = ((int)now.in);
		now.in = 0;
#ifdef VAR_RANGES
		logval("in", ((int)now.in));
#endif
		;
		_m = 3; goto P999; /* 0 */
	case 5: // STATE 4 - C:\\Users\\user\\Desktop\\Code\\C\\CS_OS\\practical_3\\writer-and-readers.pml:26 - [((in<4))] (0:0:0 - 1)
		IfNotBlocked
		reached[2][4] = 1;
		if (!((((int)now.in)<4)))
			continue;
		_m = 3; goto P999; /* 0 */
	case 6: // STATE 5 - C:\\Users\\user\\Desktop\\Code\\C\\CS_OS\\practical_3\\writer-and-readers.pml:26 - [buffer[in] = 0] (0:0:1 - 1)
		IfNotBlocked
		reached[2][5] = 1;
		(trpt+1)->bup.oval = ((int)now.buffer[ Index(((int)now.in), 4) ]);
		now.buffer[ Index(now.in, 4) ] = 0;
#ifdef VAR_RANGES
		logval("buffer[in]", ((int)now.buffer[ Index(((int)now.in), 4) ]));
#endif
		;
		_m = 3; goto P999; /* 0 */
	case 7: // STATE 6 - C:\\Users\\user\\Desktop\\Code\\C\\CS_OS\\practical_3\\writer-and-readers.pml:26 - [in = (in+1)] (0:0:1 - 1)
		IfNotBlocked
		reached[2][6] = 1;
		(trpt+1)->bup.oval = ((int)now.in);
		now.in = (((int)now.in)+1);
#ifdef VAR_RANGES
		logval("in", ((int)now.in));
#endif
		;
		_m = 3; goto P999; /* 0 */
	case 8: // STATE 8 - C:\\Users\\user\\Desktop\\Code\\C\\CS_OS\\practical_3\\writer-and-readers.pml:27 - [in = 0] (0:0:1 - 1)
		IfNotBlocked
		reached[2][8] = 1;
		(trpt+1)->bup.oval = ((int)now.in);
		now.in = 0;
#ifdef VAR_RANGES
		logval("in", ((int)now.in));
#endif
		;
		_m = 3; goto P999; /* 0 */
	case 9: // STATE 13 - C:\\Users\\user\\Desktop\\Code\\C\\CS_OS\\practical_3\\writer-and-readers.pml:29 - [out = (4-1)] (0:0:1 - 3)
		IfNotBlocked
		reached[2][13] = 1;
		(trpt+1)->bup.oval = ((int)now.out);
		now.out = (4-1);
#ifdef VAR_RANGES
		logval("out", ((int)now.out));
#endif
		;
		_m = 3; goto P999; /* 0 */
	case 10: // STATE 14 - C:\\Users\\user\\Desktop\\Code\\C\\CS_OS\\practical_3\\writer-and-readers.pml:30 - [bfull = 0] (0:0:1 - 1)
		IfNotBlocked
		reached[2][14] = 1;
		(trpt+1)->bup.oval = ((int)now.bfull);
		now.bfull = 0;
#ifdef VAR_RANGES
		logval("bfull", ((int)now.bfull));
#endif
		;
		_m = 3; goto P999; /* 0 */
	case 11: // STATE 15 - C:\\Users\\user\\Desktop\\Code\\C\\CS_OS\\practical_3\\writer-and-readers.pml:30 - [bempty = 1] (0:0:1 - 1)
		IfNotBlocked
		reached[2][15] = 1;
		(trpt+1)->bup.oval = ((int)now.bempty);
		now.bempty = 1;
#ifdef VAR_RANGES
		logval("bempty", ((int)now.bempty));
#endif
		;
		_m = 3; goto P999; /* 0 */
	case 12: // STATE 16 - C:\\Users\\user\\Desktop\\Code\\C\\CS_OS\\practical_3\\writer-and-readers.pml:31 - [printf('buffer zeroed\\n')] (0:0:0 - 1)
		IfNotBlocked
		reached[2][16] = 1;
		Printf("buffer zeroed\n");
		_m = 3; goto P999; /* 0 */
	case 13: // STATE 18 - C:\\Users\\user\\Desktop\\Code\\C\\CS_OS\\practical_3\\writer-and-readers.pml:38 - [printf('@@@ %d BUFFER in:%d, out:%d, empty:%d, full:%d [|',_pid,in,out,bempty,bfull)] (0:26:1 - 1)
		IfNotBlocked
		reached[2][18] = 1;
		Printf("@@@ %d BUFFER in:%d, out:%d, empty:%d, full:%d [|", ((int)((P2 *)_this)->_pid), ((int)now.in), ((int)now.out), ((int)now.bempty), ((int)now.bfull));
		/* merge: six = 0(26, 19, 26) */
		reached[2][19] = 1;
		(trpt+1)->bup.oval = ((int)now.six);
		now.six = 0;
#ifdef VAR_RANGES
		logval("six", ((int)now.six));
#endif
		;
		/* merge: .(goto)(0, 27, 26) */
		reached[2][27] = 1;
		;
		_m = 3; goto P999; /* 2 */
	case 14: // STATE 20 - C:\\Users\\user\\Desktop\\Code\\C\\CS_OS\\practical_3\\writer-and-readers.pml:41 - [((six<4))] (26:0:1 - 1)
		IfNotBlocked
		reached[2][20] = 1;
		if (!((((int)now.six)<4)))
			continue;
		/* merge: printf(' %d |',buffer[six])(26, 21, 26) */
		reached[2][21] = 1;
		Printf(" %d |", ((int)now.buffer[ Index(((int)now.six), 4) ]));
		/* merge: six = (six+1)(26, 22, 26) */
		reached[2][22] = 1;
		(trpt+1)->bup.oval = ((int)now.six);
		now.six = (((int)now.six)+1);
#ifdef VAR_RANGES
		logval("six", ((int)now.six));
#endif
		;
		/* merge: .(goto)(0, 27, 26) */
		reached[2][27] = 1;
		;
		_m = 3; goto P999; /* 3 */
	case 15: // STATE 24 - C:\\Users\\user\\Desktop\\Code\\C\\CS_OS\\practical_3\\writer-and-readers.pml:42 - [printf(']\\n')] (0:28:0 - 1)
		IfNotBlocked
		reached[2][24] = 1;
		Printf("]\n");
		/* merge: goto :b7(28, 25, 28) */
		reached[2][25] = 1;
		;
		_m = 3; goto P999; /* 1 */
	case 16: // STATE 31 - C:\\Users\\user\\Desktop\\Code\\C\\CS_OS\\practical_3\\pthreads.pml:29 - [mtx.mstate = unlocked] (0:0:1 - 1)
		IfNotBlocked
		reached[2][31] = 1;
		(trpt+1)->bup.oval = now.mtx.mstate;
		now.mtx.mstate = 2;
#ifdef VAR_RANGES
		logval("mtx.mstate", now.mtx.mstate);
#endif
		;
		_m = 3; goto P999; /* 0 */
	case 17: // STATE 32 - C:\\Users\\user\\Desktop\\Code\\C\\CS_OS\\practical_3\\pthreads.pml:30 - [cvars[0].cvstate = not_waiting] (0:0:1 - 1)
		IfNotBlocked
		reached[2][32] = 1;
		(trpt+1)->bup.oval = now.cvars[0].cvstate;
		now.cvars[0].cvstate = 3;
#ifdef VAR_RANGES
		logval("cvars[0].cvstate", now.cvars[0].cvstate);
#endif
		;
		_m = 3; goto P999; /* 0 */
	case 18: // STATE 33 - C:\\Users\\user\\Desktop\\Code\\C\\CS_OS\\practical_3\\pthreads.pml:31 - [cvars[1].cvstate = not_waiting] (0:0:1 - 1)
		IfNotBlocked
		reached[2][33] = 1;
		(trpt+1)->bup.oval = now.cvars[1].cvstate;
		now.cvars[1].cvstate = 3;
#ifdef VAR_RANGES
		logval("cvars[1].cvstate", now.cvars[1].cvstate);
#endif
		;
		_m = 3; goto P999; /* 0 */
	case 19: // STATE 35 - C:\\Users\\user\\Desktop\\Code\\C\\CS_OS\\practical_3\\writer-and-readers.pml:128 - [(run producer())] (0:0:0 - 1)
		IfNotBlocked
		reached[2][35] = 1;
		if (!(addproc(II, 1, 0, 0)))
			continue;
		_m = 3; goto P999; /* 0 */
	case 20: // STATE 36 - C:\\Users\\user\\Desktop\\Code\\C\\CS_OS\\practical_3\\writer-and-readers.pml:129 - [(run consumer(0))] (0:0:0 - 1)
		IfNotBlocked
		reached[2][36] = 1;
		if (!(addproc(II, 1, 1, 0)))
			continue;
		_m = 3; goto P999; /* 0 */
	case 21: // STATE 37 - C:\\Users\\user\\Desktop\\Code\\C\\CS_OS\\practical_3\\writer-and-readers.pml:130 - [(run consumer(1))] (0:0:0 - 1)
		IfNotBlocked
		reached[2][37] = 1;
		if (!(addproc(II, 1, 1, 1)))
			continue;
		_m = 3; goto P999; /* 0 */
	case 22: // STATE 38 - C:\\Users\\user\\Desktop\\Code\\C\\CS_OS\\practical_3\\writer-and-readers.pml:131 - [-end-] (0:0:0 - 1)
		IfNotBlocked
		reached[2][38] = 1;
		if (!delproc(1, II)) continue;
		_m = 3; goto P999; /* 0 */

		 /* PROC consumer */
	case 23: // STATE 1 - C:\\Users\\user\\Desktop\\Code\\C\\CS_OS\\practical_3\\pthreads.pml:37 - [printf('@@@ %d LOCKING : state is %e\\n',_pid,mtx.mstate)] (0:0:0 - 1)
		IfNotBlocked
		reached[1][1] = 1;
		Printf("@@@ %d LOCKING : state is %e\n", ((int)((P1 *)_this)->_pid), now.mtx.mstate);
		_m = 3; goto P999; /* 0 */
	case 24: // STATE 2 - C:\\Users\\user\\Desktop\\Code\\C\\CS_OS\\practical_3\\pthreads.pml:39 - [((mtx.mstate==unlocked))] (6:0:2 - 1)
		IfNotBlocked
		reached[1][2] = 1;
		if (!((now.mtx.mstate==2)))
			continue;
		/* merge: mtx.mstate = locked(6, 3, 6) */
		reached[1][3] = 1;
		(trpt+1)->bup.ovals = grab_ints(2);
		(trpt+1)->bup.ovals[0] = now.mtx.mstate;
		now.mtx.mstate = 1;
#ifdef VAR_RANGES
		logval("mtx.mstate", now.mtx.mstate);
#endif
		;
		/* merge: mtx.mid = _pid(6, 4, 6) */
		reached[1][4] = 1;
		(trpt+1)->bup.ovals[1] = ((int)now.mtx.mid);
		now.mtx.mid = ((int)((P1 *)_this)->_pid);
#ifdef VAR_RANGES
		logval("mtx.mid", ((int)now.mtx.mid));
#endif
		;
		_m = 3; goto P999; /* 2 */
	case 25: // STATE 6 - C:\\Users\\user\\Desktop\\Code\\C\\CS_OS\\practical_3\\pthreads.pml:42 - [printf('@@@ %d LOCKED : state is %e\\n',_pid,mtx.mstate)] (0:0:0 - 1)
		IfNotBlocked
		reached[1][6] = 1;
		Printf("@@@ %d LOCKED : state is %e\n", ((int)((P1 *)_this)->_pid), now.mtx.mstate);
		_m = 3; goto P999; /* 0 */
	case 26: // STATE 8 - C:\\Users\\user\\Desktop\\Code\\C\\CS_OS\\practical_3\\writer-and-readers.pml:101 - [assert((mtx.mid==_pid))] (0:0:0 - 1)
		IfNotBlocked
		reached[1][8] = 1;
		spin_assert((((int)now.mtx.mid)==((int)((P1 *)_this)->_pid)), "(mtx.mid==_pid)", II, tt, t);
		_m = 3; goto P999; /* 0 */
	case 27: // STATE 9 - C:\\Users\\user\\Desktop\\Code\\C\\CS_OS\\practical_3\\writer-and-readers.pml:103 - [(!(bempty))] (0:0:0 - 1)
		IfNotBlocked
		reached[1][9] = 1;
		if (!( !(((int)now.bempty))))
			continue;
		_m = 3; goto P999; /* 0 */
	case 28: // STATE 12 - C:\\Users\\user\\Desktop\\Code\\C\\CS_OS\\practical_3\\pthreads.pml:58 - [printf('@@@ %d WAIT for cond[%d]=%d with mutex=%e\\n',_pid,1,cvars[1].cvstate,mtx.mstate)] (0:0:0 - 1)
		IfNotBlocked
		reached[1][12] = 1;
		Printf("@@@ %d WAIT for cond[%d]=%d with mutex=%e\n", ((int)((P1 *)_this)->_pid), 1, now.cvars[1].cvstate, now.mtx.mstate);
		_m = 3; goto P999; /* 0 */
	case 29: // STATE 13 - C:\\Users\\user\\Desktop\\Code\\C\\CS_OS\\practical_3\\pthreads.pml:62 - [assert((mtx.mstate==locked))] (0:0:0 - 1)
		IfNotBlocked
		reached[1][13] = 1;
		spin_assert((now.mtx.mstate==1), "(mtx.mstate==1)", II, tt, t);
		_m = 3; goto P999; /* 0 */
	case 30: // STATE 15 - C:\\Users\\user\\Desktop\\Code\\C\\CS_OS\\practical_3\\pthreads.pml:66 - [printf('@@@ %d DONE with cond[%d]=%d with mutex=%e\\n',_pid,1,cvars[1].cvstate,mtx.mstate)] (0:0:0 - 1)
		IfNotBlocked
		reached[1][15] = 1;
		Printf("@@@ %d DONE with cond[%d]=%d with mutex=%e\n", ((int)((P1 *)_this)->_pid), 1, now.cvars[1].cvstate, now.mtx.mstate);
		_m = 3; goto P999; /* 0 */
	case 31: // STATE 20 - C:\\Users\\user\\Desktop\\Code\\C\\CS_OS\\practical_3\\writer-and-readers.pml:62 - [assert(!(bempty))] (0:0:0 - 1)
		IfNotBlocked
		reached[1][20] = 1;
		spin_assert( !(((int)now.bempty)), " !(bempty)", II, tt, t);
		_m = 3; goto P999; /* 0 */
	case 32: // STATE 21 - C:\\Users\\user\\Desktop\\Code\\C\\CS_OS\\practical_3\\writer-and-readers.pml:63 - [out = ((out+1)%4)] (0:0:1 - 1)
		IfNotBlocked
		reached[1][21] = 1;
		(trpt+1)->bup.oval = ((int)now.out);
		now.out = ((((int)now.out)+1)%4);
#ifdef VAR_RANGES
		logval("out", ((int)now.out));
#endif
		;
		_m = 3; goto P999; /* 0 */
	case 33: // STATE 22 - C:\\Users\\user\\Desktop\\Code\\C\\CS_OS\\practical_3\\writer-and-readers.pml:64 - [cout[cno] = buffer[out]] (0:0:1 - 1)
		IfNotBlocked
		reached[1][22] = 1;
		(trpt+1)->bup.oval = ((int)cout[ Index(((int)((P1 *)_this)->cno), 2) ]);
		cout[ Index(((P1 *)_this)->cno, 2) ] = ((int)now.buffer[ Index(((int)now.out), 4) ]);
#ifdef VAR_RANGES
		logval("cout[consumer:cno]", ((int)cout[ Index(((int)((P1 *)_this)->cno), 2) ]));
#endif
		;
		_m = 3; goto P999; /* 0 */
	case 34: // STATE 23 - C:\\Users\\user\\Desktop\\Code\\C\\CS_OS\\practical_3\\writer-and-readers.pml:64 - [buffer[out] = 0] (0:0:1 - 1)
		IfNotBlocked
		reached[1][23] = 1;
		(trpt+1)->bup.oval = ((int)now.buffer[ Index(((int)now.out), 4) ]);
		now.buffer[ Index(now.out, 4) ] = 0;
#ifdef VAR_RANGES
		logval("buffer[out]", ((int)now.buffer[ Index(((int)now.out), 4) ]));
#endif
		;
		_m = 3; goto P999; /* 0 */
	case 35: // STATE 24 - C:\\Users\\user\\Desktop\\Code\\C\\CS_OS\\practical_3\\writer-and-readers.pml:65 - [printf('@@@ %d EXTRACT cout[%d] := buf[%d] is %d\\n',_pid,cno,out,cout[cno])] (0:0:0 - 1)
		IfNotBlocked
		reached[1][24] = 1;
		Printf("@@@ %d EXTRACT cout[%d] := buf[%d] is %d\n", ((int)((P1 *)_this)->_pid), ((int)((P1 *)_this)->cno), ((int)now.out), ((int)cout[ Index(((int)((P1 *)_this)->cno), 2) ]));
		_m = 3; goto P999; /* 0 */
	case 36: // STATE 25 - C:\\Users\\user\\Desktop\\Code\\C\\CS_OS\\practical_3\\writer-and-readers.pml:66 - [bfull = 0] (0:0:1 - 1)
		IfNotBlocked
		reached[1][25] = 1;
		(trpt+1)->bup.oval = ((int)now.bfull);
		now.bfull = 0;
#ifdef VAR_RANGES
		logval("bfull", ((int)now.bfull));
#endif
		;
		_m = 3; goto P999; /* 0 */
	case 37: // STATE 26 - C:\\Users\\user\\Desktop\\Code\\C\\CS_OS\\practical_3\\writer-and-readers.pml:67 - [bempty = (((out+1)%4)==in)] (0:0:1 - 1)
		IfNotBlocked
		reached[1][26] = 1;
		(trpt+1)->bup.oval = ((int)now.bempty);
		now.bempty = (((((int)now.out)+1)%4)==((int)now.in));
#ifdef VAR_RANGES
		logval("bempty", ((int)now.bempty));
#endif
		;
		_m = 3; goto P999; /* 0 */
	case 38: // STATE 27 - C:\\Users\\user\\Desktop\\Code\\C\\CS_OS\\practical_3\\writer-and-readers.pml:38 - [printf('@@@ %d BUFFER in:%d, out:%d, empty:%d, full:%d [|',_pid,in,out,bempty,bfull)] (0:35:1 - 1)
		IfNotBlocked
		reached[1][27] = 1;
		Printf("@@@ %d BUFFER in:%d, out:%d, empty:%d, full:%d [|", ((int)((P1 *)_this)->_pid), ((int)now.in), ((int)now.out), ((int)now.bempty), ((int)now.bfull));
		/* merge: six = 0(35, 28, 35) */
		reached[1][28] = 1;
		(trpt+1)->bup.oval = ((int)now.six);
		now.six = 0;
#ifdef VAR_RANGES
		logval("six", ((int)now.six));
#endif
		;
		/* merge: .(goto)(0, 36, 35) */
		reached[1][36] = 1;
		;
		_m = 3; goto P999; /* 2 */
	case 39: // STATE 29 - C:\\Users\\user\\Desktop\\Code\\C\\CS_OS\\practical_3\\writer-and-readers.pml:41 - [((six<4))] (35:0:1 - 1)
		IfNotBlocked
		reached[1][29] = 1;
		if (!((((int)now.six)<4)))
			continue;
		/* merge: printf(' %d |',buffer[six])(35, 30, 35) */
		reached[1][30] = 1;
		Printf(" %d |", ((int)now.buffer[ Index(((int)now.six), 4) ]));
		/* merge: six = (six+1)(35, 31, 35) */
		reached[1][31] = 1;
		(trpt+1)->bup.oval = ((int)now.six);
		now.six = (((int)now.six)+1);
#ifdef VAR_RANGES
		logval("six", ((int)now.six));
#endif
		;
		/* merge: .(goto)(0, 36, 35) */
		reached[1][36] = 1;
		;
		_m = 3; goto P999; /* 3 */
	case 40: // STATE 33 - C:\\Users\\user\\Desktop\\Code\\C\\CS_OS\\practical_3\\writer-and-readers.pml:42 - [printf(']\\n')] (0:37:0 - 1)
		IfNotBlocked
		reached[1][33] = 1;
		Printf("]\n");
		/* merge: goto :b5(37, 34, 37) */
		reached[1][34] = 1;
		;
		_m = 3; goto P999; /* 1 */
	case 41: // STATE 41 - C:\\Users\\user\\Desktop\\Code\\C\\CS_OS\\practical_3\\pthreads.pml:73 - [printf('@@@ %d SIGNAL cond[%d]=%d\\n',_pid,0,cvars[0].cvstate)] (0:0:0 - 1)
		IfNotBlocked
		reached[1][41] = 1;
		Printf("@@@ %d SIGNAL cond[%d]=%d\n", ((int)((P1 *)_this)->_pid), 0, now.cvars[0].cvstate);
		_m = 3; goto P999; /* 0 */
	case 42: // STATE 42 - C:\\Users\\user\\Desktop\\Code\\C\\CS_OS\\practical_3\\pthreads.pml:75 - [printf('@@@ %d SIGNALLED cond[%d]=%d\\n',_pid,0,cvars[0].cvstate)] (0:0:0 - 1)
		IfNotBlocked
		reached[1][42] = 1;
		Printf("@@@ %d SIGNALLED cond[%d]=%d\n", ((int)((P1 *)_this)->_pid), 0, now.cvars[0].cvstate);
		_m = 3; goto P999; /* 0 */
	case 43: // STATE 44 - C:\\Users\\user\\Desktop\\Code\\C\\CS_OS\\practical_3\\pthreads.pml:47 - [printf('@@@ %d UNLOCKING : state is %e\\n',_pid,mtx.mstate)] (0:0:0 - 1)
		IfNotBlocked
		reached[1][44] = 1;
		Printf("@@@ %d UNLOCKING : state is %e\n", ((int)((P1 *)_this)->_pid), now.mtx.mstate);
		_m = 3; goto P999; /* 0 */
	case 44: // STATE 45 - C:\\Users\\user\\Desktop\\Code\\C\\CS_OS\\practical_3\\pthreads.pml:49 - [assert((mtx.mid==_pid))] (0:49:2 - 1)
		IfNotBlocked
		reached[1][45] = 1;
		spin_assert((((int)now.mtx.mid)==((int)((P1 *)_this)->_pid)), "(mtx.mid==_pid)", II, tt, t);
		/* merge: mtx.mstate = unlocked(49, 46, 49) */
		reached[1][46] = 1;
		(trpt+1)->bup.ovals = grab_ints(2);
		(trpt+1)->bup.ovals[0] = now.mtx.mstate;
		now.mtx.mstate = 2;
#ifdef VAR_RANGES
		logval("mtx.mstate", now.mtx.mstate);
#endif
		;
		/* merge: mtx.mid = 0(49, 47, 49) */
		reached[1][47] = 1;
		(trpt+1)->bup.ovals[1] = ((int)now.mtx.mid);
		now.mtx.mid = 0;
#ifdef VAR_RANGES
		logval("mtx.mid", ((int)now.mtx.mid));
#endif
		;
		_m = 3; goto P999; /* 2 */
	case 45: // STATE 49 - C:\\Users\\user\\Desktop\\Code\\C\\CS_OS\\practical_3\\pthreads.pml:53 - [printf('@@@ %d UNLOCKED : state is %e\\n',_pid,mtx.mstate)] (0:0:0 - 1)
		IfNotBlocked
		reached[1][49] = 1;
		Printf("@@@ %d UNLOCKED : state is %e\n", ((int)((P1 *)_this)->_pid), now.mtx.mstate);
		_m = 3; goto P999; /* 0 */
	case 46: // STATE 55 - C:\\Users\\user\\Desktop\\Code\\C\\CS_OS\\practical_3\\writer-and-readers.pml:116 - [-end-] (0:0:0 - 1)
		IfNotBlocked
		reached[1][55] = 1;
		if (!delproc(1, II)) continue;
		_m = 3; goto P999; /* 0 */

		 /* PROC producer */
	case 47: // STATE 1 - C:\\Users\\user\\Desktop\\Code\\C\\CS_OS\\practical_3\\pthreads.pml:37 - [printf('@@@ %d LOCKING : state is %e\\n',_pid,mtx.mstate)] (0:0:0 - 1)
		IfNotBlocked
		reached[0][1] = 1;
		Printf("@@@ %d LOCKING : state is %e\n", ((int)((P0 *)_this)->_pid), now.mtx.mstate);
		_m = 3; goto P999; /* 0 */
	case 48: // STATE 2 - C:\\Users\\user\\Desktop\\Code\\C\\CS_OS\\practical_3\\pthreads.pml:39 - [((mtx.mstate==unlocked))] (6:0:2 - 1)
		IfNotBlocked
		reached[0][2] = 1;
		if (!((now.mtx.mstate==2)))
			continue;
		/* merge: mtx.mstate = locked(6, 3, 6) */
		reached[0][3] = 1;
		(trpt+1)->bup.ovals = grab_ints(2);
		(trpt+1)->bup.ovals[0] = now.mtx.mstate;
		now.mtx.mstate = 1;
#ifdef VAR_RANGES
		logval("mtx.mstate", now.mtx.mstate);
#endif
		;
		/* merge: mtx.mid = _pid(6, 4, 6) */
		reached[0][4] = 1;
		(trpt+1)->bup.ovals[1] = ((int)now.mtx.mid);
		now.mtx.mid = ((int)((P0 *)_this)->_pid);
#ifdef VAR_RANGES
		logval("mtx.mid", ((int)now.mtx.mid));
#endif
		;
		_m = 3; goto P999; /* 2 */
	case 49: // STATE 6 - C:\\Users\\user\\Desktop\\Code\\C\\CS_OS\\practical_3\\pthreads.pml:42 - [printf('@@@ %d LOCKED : state is %e\\n',_pid,mtx.mstate)] (0:0:0 - 1)
		IfNotBlocked
		reached[0][6] = 1;
		Printf("@@@ %d LOCKED : state is %e\n", ((int)((P0 *)_this)->_pid), now.mtx.mstate);
		_m = 3; goto P999; /* 0 */
	case 50: // STATE 8 - C:\\Users\\user\\Desktop\\Code\\C\\CS_OS\\practical_3\\writer-and-readers.pml:76 - [assert((mtx.mid==_pid))] (0:0:0 - 1)
		IfNotBlocked
		reached[0][8] = 1;
		spin_assert((((int)now.mtx.mid)==((int)((P0 *)_this)->_pid)), "(mtx.mid==_pid)", II, tt, t);
		_m = 3; goto P999; /* 0 */
	case 51: // STATE 9 - C:\\Users\\user\\Desktop\\Code\\C\\CS_OS\\practical_3\\writer-and-readers.pml:78 - [(!(bfull))] (0:0:0 - 1)
		IfNotBlocked
		reached[0][9] = 1;
		if (!( !(((int)now.bfull))))
			continue;
		_m = 3; goto P999; /* 0 */
	case 52: // STATE 12 - C:\\Users\\user\\Desktop\\Code\\C\\CS_OS\\practical_3\\pthreads.pml:58 - [printf('@@@ %d WAIT for cond[%d]=%d with mutex=%e\\n',_pid,0,cvars[0].cvstate,mtx.mstate)] (0:0:0 - 1)
		IfNotBlocked
		reached[0][12] = 1;
		Printf("@@@ %d WAIT for cond[%d]=%d with mutex=%e\n", ((int)((P0 *)_this)->_pid), 0, now.cvars[0].cvstate, now.mtx.mstate);
		_m = 3; goto P999; /* 0 */
	case 53: // STATE 13 - C:\\Users\\user\\Desktop\\Code\\C\\CS_OS\\practical_3\\pthreads.pml:62 - [assert((mtx.mstate==locked))] (0:0:0 - 1)
		IfNotBlocked
		reached[0][13] = 1;
		spin_assert((now.mtx.mstate==1), "(mtx.mstate==1)", II, tt, t);
		_m = 3; goto P999; /* 0 */
	case 54: // STATE 15 - C:\\Users\\user\\Desktop\\Code\\C\\CS_OS\\practical_3\\pthreads.pml:66 - [printf('@@@ %d DONE with cond[%d]=%d with mutex=%e\\n',_pid,0,cvars[0].cvstate,mtx.mstate)] (0:0:0 - 1)
		IfNotBlocked
		reached[0][15] = 1;
		Printf("@@@ %d DONE with cond[%d]=%d with mutex=%e\n", ((int)((P0 *)_this)->_pid), 0, now.cvars[0].cvstate, now.mtx.mstate);
		_m = 3; goto P999; /* 0 */
	case 55: // STATE 20 - C:\\Users\\user\\Desktop\\Code\\C\\CS_OS\\practical_3\\writer-and-readers.pml:50 - [assert(!(bfull))] (0:0:0 - 1)
		IfNotBlocked
		reached[0][20] = 1;
		spin_assert( !(((int)now.bfull)), " !(bfull)", II, tt, t);
		_m = 3; goto P999; /* 0 */
	case 56: // STATE 21 - C:\\Users\\user\\Desktop\\Code\\C\\CS_OS\\practical_3\\writer-and-readers.pml:51 - [buffer[in] = p] (0:0:1 - 1)
		IfNotBlocked
		reached[0][21] = 1;
		(trpt+1)->bup.oval = ((int)now.buffer[ Index(((int)now.in), 4) ]);
		now.buffer[ Index(now.in, 4) ] = ((P0 *)_this)->p;
#ifdef VAR_RANGES
		logval("buffer[in]", ((int)now.buffer[ Index(((int)now.in), 4) ]));
#endif
		;
		_m = 3; goto P999; /* 0 */
	case 57: // STATE 22 - C:\\Users\\user\\Desktop\\Code\\C\\CS_OS\\practical_3\\writer-and-readers.pml:52 - [printf('@@@ %d INSERT buf[%d] := %d\\n',_pid,in,p)] (0:0:0 - 1)
		IfNotBlocked
		reached[0][22] = 1;
		Printf("@@@ %d INSERT buf[%d] := %d\n", ((int)((P0 *)_this)->_pid), ((int)now.in), ((P0 *)_this)->p);
		_m = 3; goto P999; /* 0 */
	case 58: // STATE 23 - C:\\Users\\user\\Desktop\\Code\\C\\CS_OS\\practical_3\\writer-and-readers.pml:53 - [bempty = 0] (0:0:1 - 1)
		IfNotBlocked
		reached[0][23] = 1;
		(trpt+1)->bup.oval = ((int)now.bempty);
		now.bempty = 0;
#ifdef VAR_RANGES
		logval("bempty", ((int)now.bempty));
#endif
		;
		_m = 3; goto P999; /* 0 */
	case 59: // STATE 24 - C:\\Users\\user\\Desktop\\Code\\C\\CS_OS\\practical_3\\writer-and-readers.pml:54 - [bfull = (in==out)] (0:0:1 - 1)
		IfNotBlocked
		reached[0][24] = 1;
		(trpt+1)->bup.oval = ((int)now.bfull);
		now.bfull = (((int)now.in)==((int)now.out));
#ifdef VAR_RANGES
		logval("bfull", ((int)now.bfull));
#endif
		;
		_m = 3; goto P999; /* 0 */
	case 60: // STATE 25 - C:\\Users\\user\\Desktop\\Code\\C\\CS_OS\\practical_3\\writer-and-readers.pml:55 - [in = ((in+1)%4)] (0:0:1 - 1)
		IfNotBlocked
		reached[0][25] = 1;
		(trpt+1)->bup.oval = ((int)now.in);
		now.in = ((((int)now.in)+1)%4);
#ifdef VAR_RANGES
		logval("in", ((int)now.in));
#endif
		;
		_m = 3; goto P999; /* 0 */
	case 61: // STATE 26 - C:\\Users\\user\\Desktop\\Code\\C\\CS_OS\\practical_3\\writer-and-readers.pml:38 - [printf('@@@ %d BUFFER in:%d, out:%d, empty:%d, full:%d [|',_pid,in,out,bempty,bfull)] (0:34:1 - 1)
		IfNotBlocked
		reached[0][26] = 1;
		Printf("@@@ %d BUFFER in:%d, out:%d, empty:%d, full:%d [|", ((int)((P0 *)_this)->_pid), ((int)now.in), ((int)now.out), ((int)now.bempty), ((int)now.bfull));
		/* merge: six = 0(34, 27, 34) */
		reached[0][27] = 1;
		(trpt+1)->bup.oval = ((int)now.six);
		now.six = 0;
#ifdef VAR_RANGES
		logval("six", ((int)now.six));
#endif
		;
		/* merge: .(goto)(0, 35, 34) */
		reached[0][35] = 1;
		;
		_m = 3; goto P999; /* 2 */
	case 62: // STATE 28 - C:\\Users\\user\\Desktop\\Code\\C\\CS_OS\\practical_3\\writer-and-readers.pml:41 - [((six<4))] (34:0:1 - 1)
		IfNotBlocked
		reached[0][28] = 1;
		if (!((((int)now.six)<4)))
			continue;
		/* merge: printf(' %d |',buffer[six])(34, 29, 34) */
		reached[0][29] = 1;
		Printf(" %d |", ((int)now.buffer[ Index(((int)now.six), 4) ]));
		/* merge: six = (six+1)(34, 30, 34) */
		reached[0][30] = 1;
		(trpt+1)->bup.oval = ((int)now.six);
		now.six = (((int)now.six)+1);
#ifdef VAR_RANGES
		logval("six", ((int)now.six));
#endif
		;
		/* merge: .(goto)(0, 35, 34) */
		reached[0][35] = 1;
		;
		_m = 3; goto P999; /* 3 */
	case 63: // STATE 32 - C:\\Users\\user\\Desktop\\Code\\C\\CS_OS\\practical_3\\writer-and-readers.pml:42 - [printf(']\\n')] (0:36:0 - 1)
		IfNotBlocked
		reached[0][32] = 1;
		Printf("]\n");
		/* merge: goto :b2(36, 33, 36) */
		reached[0][33] = 1;
		;
		_m = 3; goto P999; /* 1 */
	case 64: // STATE 40 - C:\\Users\\user\\Desktop\\Code\\C\\CS_OS\\practical_3\\pthreads.pml:73 - [printf('@@@ %d SIGNAL cond[%d]=%d\\n',_pid,1,cvars[1].cvstate)] (0:0:0 - 1)
		IfNotBlocked
		reached[0][40] = 1;
		Printf("@@@ %d SIGNAL cond[%d]=%d\n", ((int)((P0 *)_this)->_pid), 1, now.cvars[1].cvstate);
		_m = 3; goto P999; /* 0 */
	case 65: // STATE 41 - C:\\Users\\user\\Desktop\\Code\\C\\CS_OS\\practical_3\\pthreads.pml:75 - [printf('@@@ %d SIGNALLED cond[%d]=%d\\n',_pid,1,cvars[1].cvstate)] (0:0:0 - 1)
		IfNotBlocked
		reached[0][41] = 1;
		Printf("@@@ %d SIGNALLED cond[%d]=%d\n", ((int)((P0 *)_this)->_pid), 1, now.cvars[1].cvstate);
		_m = 3; goto P999; /* 0 */
	case 66: // STATE 43 - C:\\Users\\user\\Desktop\\Code\\C\\CS_OS\\practical_3\\pthreads.pml:47 - [printf('@@@ %d UNLOCKING : state is %e\\n',_pid,mtx.mstate)] (0:0:0 - 1)
		IfNotBlocked
		reached[0][43] = 1;
		Printf("@@@ %d UNLOCKING : state is %e\n", ((int)((P0 *)_this)->_pid), now.mtx.mstate);
		_m = 3; goto P999; /* 0 */
	case 67: // STATE 44 - C:\\Users\\user\\Desktop\\Code\\C\\CS_OS\\practical_3\\pthreads.pml:49 - [assert((mtx.mid==_pid))] (0:48:2 - 1)
		IfNotBlocked
		reached[0][44] = 1;
		spin_assert((((int)now.mtx.mid)==((int)((P0 *)_this)->_pid)), "(mtx.mid==_pid)", II, tt, t);
		/* merge: mtx.mstate = unlocked(48, 45, 48) */
		reached[0][45] = 1;
		(trpt+1)->bup.ovals = grab_ints(2);
		(trpt+1)->bup.ovals[0] = now.mtx.mstate;
		now.mtx.mstate = 2;
#ifdef VAR_RANGES
		logval("mtx.mstate", now.mtx.mstate);
#endif
		;
		/* merge: mtx.mid = 0(48, 46, 48) */
		reached[0][46] = 1;
		(trpt+1)->bup.ovals[1] = ((int)now.mtx.mid);
		now.mtx.mid = 0;
#ifdef VAR_RANGES
		logval("mtx.mid", ((int)now.mtx.mid));
#endif
		;
		_m = 3; goto P999; /* 2 */
	case 68: // STATE 48 - C:\\Users\\user\\Desktop\\Code\\C\\CS_OS\\practical_3\\pthreads.pml:53 - [printf('@@@ %d UNLOCKED : state is %e\\n',_pid,mtx.mstate)] (0:0:0 - 1)
		IfNotBlocked
		reached[0][48] = 1;
		Printf("@@@ %d UNLOCKED : state is %e\n", ((int)((P0 *)_this)->_pid), now.mtx.mstate);
		_m = 3; goto P999; /* 0 */
	case 69: // STATE 51 - C:\\Users\\user\\Desktop\\Code\\C\\CS_OS\\practical_3\\writer-and-readers.pml:92 - [p = ((p%7)+1)] (0:0:1 - 1)
		IfNotBlocked
		reached[0][51] = 1;
		(trpt+1)->bup.oval = ((P0 *)_this)->p;
		((P0 *)_this)->p = ((((P0 *)_this)->p%7)+1);
#ifdef VAR_RANGES
		logval("producer:p", ((P0 *)_this)->p);
#endif
		;
		_m = 3; goto P999; /* 0 */
	case 70: // STATE 55 - C:\\Users\\user\\Desktop\\Code\\C\\CS_OS\\practical_3\\writer-and-readers.pml:95 - [-end-] (0:0:0 - 1)
		IfNotBlocked
		reached[0][55] = 1;
		if (!delproc(1, II)) continue;
		_m = 3; goto P999; /* 0 */
	case  _T5:	/* np_ */
		if (!((!(trpt->o_pm&4) && !(trpt->tau&128))))
			continue;
		/* else fall through */
	case  _T2:	/* true */
		_m = 3; goto P999;
#undef rand
	}

