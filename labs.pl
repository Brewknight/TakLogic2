:- lib(ic).
:- lib(branch_and_bound).

labs(N, Labs, Cost) :-
   length(Labs, N),           % ������� ���������� ����������
   Labs #:: [-1,1],           % ������� ������ ����������
   compute_auto_corrs_sq(Labs, Labs, AutoCorrsSq),
   Cost #= sum(AutoCorrsSq),  % ����� ������������� ��� ����������
   bb_min(search(Labs, 0, input_order, indomain, complete, []), Cost, _).

compute_auto_corrs_sq([], [], []).
compute_auto_corrs_sq(Labs1, [_|Labs3], [AutoCorr*AutoCorr|AutoCorrsSq]) :-
   append(Labs2, [_], Labs1),
   inner_prod(Labs2, Labs3, AutoCorr),  % ������������� k-�����
   compute_auto_corrs_sq(Labs2, Labs3, AutoCorrsSq).

inner_prod([], [], 0).
inner_prod([X|L1], [Y|L2], X*Y+InnerProd) :-
   inner_prod(L1, L2, InnerProd).
