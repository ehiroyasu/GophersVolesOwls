function out = OwlVole_III
out{1} = @init;
out{2} = @fun_eval;
out{3} = @jacobian;
out{4} = @jacobianp;
out{5} = @hessians;
out{6} = @hessiansp;
out{7} = @der3;
out{8} = [];
out{9} = [];

% --------------------------------------------------------------------------
function dydt = fun_eval(t,kmrgd,par_P,par_D,par_q)
dydt=[kmrgd(1)*(1-kmrgd(1))-par_P*kmrgd(1)^(par_q+1)/(kmrgd(1)^(par_q+1)+par_D^(par_q+1));];

% --------------------------------------------------------------------------
function [tspan,y0,options] = init
handles = feval(OwlVole_III);
y0=[0];
options = odeset('Jacobian',handles(3),'JacobianP',handles(4),'Hessians',handles(5),'HessiansP',handles(6));
tspan = [0 10];

% --------------------------------------------------------------------------
function jac = jacobian(t,kmrgd,par_P,par_D,par_q)
jac=[ (kmrgd(1)^par_q*kmrgd(1)^(par_q + 1)*par_P*(par_q + 1))/(kmrgd(1)^(par_q + 1) + par_D^(par_q + 1))^2 - (kmrgd(1)^par_q*par_P*(par_q + 1))/(kmrgd(1)^(par_q + 1) + par_D^(par_q + 1)) - 2*kmrgd(1) + 1 ];
% --------------------------------------------------------------------------
function jacp = jacobianp(t,kmrgd,par_P,par_D,par_q)
jacp=[ -kmrgd(1)^(par_q + 1)/(kmrgd(1)^(par_q + 1) + par_D^(par_q + 1)) , (kmrgd(1)^(par_q + 1)*par_D^par_q*par_P*(par_q + 1))/(kmrgd(1)^(par_q + 1) + par_D^(par_q + 1))^2 , (kmrgd(1)^(par_q + 1)*par_P*(kmrgd(1)^(par_q + 1)*log(kmrgd(1)) + par_D^(par_q + 1)*log(par_D)))/(kmrgd(1)^(par_q + 1) + par_D^(par_q + 1))^2 - (kmrgd(1)^(par_q + 1)*par_P*log(kmrgd(1)))/(kmrgd(1)^(par_q + 1) + par_D^(par_q + 1)) ];
% --------------------------------------------------------------------------
function hess = hessians(t,kmrgd,par_P,par_D,par_q)
hess1=[ (2*kmrgd(1)^(2*par_q)*par_P*(par_q + 1)^2)/(kmrgd(1)^(par_q + 1) + par_D^(par_q + 1))^2 - (2*kmrgd(1)^(2*par_q)*kmrgd(1)^(par_q + 1)*par_P*(par_q + 1)^2)/(kmrgd(1)^(par_q + 1) + par_D^(par_q + 1))^3 - (kmrgd(1)^(par_q - 1)*par_P*par_q*(par_q + 1))/(kmrgd(1)^(par_q + 1) + par_D^(par_q + 1)) + (kmrgd(1)^(par_q - 1)*kmrgd(1)^(par_q + 1)*par_P*par_q*(par_q + 1))/(kmrgd(1)^(par_q + 1) + par_D^(par_q + 1))^2 - 2 ];
hess(:,:,1) =hess1;
% --------------------------------------------------------------------------
function hessp = hessiansp(t,kmrgd,par_P,par_D,par_q)
hessp1=[ (kmrgd(1)^par_q*kmrgd(1)^(par_q + 1)*(par_q + 1))/(kmrgd(1)^(par_q + 1) + par_D^(par_q + 1))^2 - (kmrgd(1)^par_q*(par_q + 1))/(kmrgd(1)^(par_q + 1) + par_D^(par_q + 1)) ];
hessp2=[ (kmrgd(1)^par_q*par_D^par_q*par_P*(par_q + 1)^2)/(kmrgd(1)^(par_q + 1) + par_D^(par_q + 1))^2 - (2*kmrgd(1)^par_q*kmrgd(1)^(par_q + 1)*par_D^par_q*par_P*(par_q + 1)^2)/(kmrgd(1)^(par_q + 1) + par_D^(par_q + 1))^3 ];
hessp3=[ (kmrgd(1)^par_q*kmrgd(1)^(par_q + 1)*par_P)/(kmrgd(1)^(par_q + 1) + par_D^(par_q + 1))^2 - (kmrgd(1)^par_q*par_P)/(kmrgd(1)^(par_q + 1) + par_D^(par_q + 1)) - (kmrgd(1)^par_q*par_P*log(kmrgd(1))*(par_q + 1))/(kmrgd(1)^(par_q + 1) + par_D^(par_q + 1)) + (kmrgd(1)^par_q*par_P*(par_q + 1)*(kmrgd(1)^(par_q + 1)*log(kmrgd(1)) + par_D^(par_q + 1)*log(par_D)))/(kmrgd(1)^(par_q + 1) + par_D^(par_q + 1))^2 + (2*kmrgd(1)^par_q*kmrgd(1)^(par_q + 1)*par_P*log(kmrgd(1))*(par_q + 1))/(kmrgd(1)^(par_q + 1) + par_D^(par_q + 1))^2 - (2*kmrgd(1)^par_q*kmrgd(1)^(par_q + 1)*par_P*(par_q + 1)*(kmrgd(1)^(par_q + 1)*log(kmrgd(1)) + par_D^(par_q + 1)*log(par_D)))/(kmrgd(1)^(par_q + 1) + par_D^(par_q + 1))^3 ];
hessp(:,:,1) =hessp1;
hessp(:,:,2) =hessp2;
hessp(:,:,3) =hessp3;
%---------------------------------------------------------------------------
function tens3  = der3(t,kmrgd,par_P,par_D,par_q)
tens31=[ (6*kmrgd(1)^(3*par_q)*kmrgd(1)^(par_q + 1)*par_P*(par_q + 1)^3)/(kmrgd(1)^(par_q + 1) + par_D^(par_q + 1))^4 - (6*kmrgd(1)^(3*par_q)*par_P*(par_q + 1)^3)/(kmrgd(1)^(par_q + 1) + par_D^(par_q + 1))^3 + (4*kmrgd(1)^(2*par_q - 1)*par_P*par_q*(par_q + 1)^2)/(kmrgd(1)^(par_q + 1) + par_D^(par_q + 1))^2 + (2*kmrgd(1)^par_q*kmrgd(1)^(par_q - 1)*par_P*par_q*(par_q + 1)^2)/(kmrgd(1)^(par_q + 1) + par_D^(par_q + 1))^2 - (kmrgd(1)^(par_q - 2)*par_P*par_q*(par_q - 1)*(par_q + 1))/(kmrgd(1)^(par_q + 1) + par_D^(par_q + 1)) - (4*kmrgd(1)^(par_q + 1)*kmrgd(1)^(2*par_q - 1)*par_P*par_q*(par_q + 1)^2)/(kmrgd(1)^(par_q + 1) + par_D^(par_q + 1))^3 - (2*kmrgd(1)^par_q*kmrgd(1)^(par_q - 1)*kmrgd(1)^(par_q + 1)*par_P*par_q*(par_q + 1)^2)/(kmrgd(1)^(par_q + 1) + par_D^(par_q + 1))^3 + (kmrgd(1)^(par_q + 1)*kmrgd(1)^(par_q - 2)*par_P*par_q*(par_q - 1)*(par_q + 1))/(kmrgd(1)^(par_q + 1) + par_D^(par_q + 1))^2 ];
tens3(:,:,1,1) =tens31;
%---------------------------------------------------------------------------
function tens4  = der4(t,kmrgd,par_P,par_D,par_q)
%---------------------------------------------------------------------------
function tens5  = der5(t,kmrgd,par_P,par_D,par_q)
