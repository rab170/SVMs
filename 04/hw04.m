x = linspace(-10,10);
y = linspace(-5, 15);

f = @(x, alpha) (x.^2 - x) - alpha.*(x-3);
[X, Y] = meshgrid (x, y);
tz = f(X, Y);
z = f(x, y);

figure(1)
mesh(x, y, tz);
%surf(X, Y, f(X,Y))
xlabel('x')
zlabel('L(\alpha, x)')
ylabel('alpha')
grid minor
print -dpng surface.png;

view(0,0);
print -dpng L_x.png;

view(90,-10);
print -dpng L_alpha.png;
