import gifAnimation.*;

GifMaker gifExport;

float angle = 0.1;

PImage img;

PrintWriter output;

int numBalls = 3;
float spring = 0.05;
float gravity = 0.03;
float friction = -0.9;
Ball[] balls = new Ball[numBalls];

void setup() {
  size(640, 360);
  smooth();
  img = loadImage("basketball.png");
  output = createWriter("output.csv");
  output.println("frame,id,score,x,y,x2,y2");
  for (int i = 0; i < numBalls; i++) {
    //balls[i] = new Ball(img, 70, 70, 50, i, balls);
    balls[i] = new Ball(img, random(width), random(height), 50, i, balls);
  }
  noStroke();
  background(0);
  frameRate(6);
  gifExport = new GifMaker(this, "OneBall.gif");
  gifExport.setRepeat(0);
  gifExport.setTransparent(255);
  gifExport.setDelay(1000/24);
  //fill(255, 204);
}

void draw() {
  background(0);
  for (Ball ball : balls) {
    //ball.collide();
    ball.move();
    float dx = ball.x + 50;
    float dy = ball.y + 50;
    output.println(frameCount + ",sports ball,1," + ball.x + "," + ball.y + "," + dx + "," + dy);
    ball.display();
  }
  gifExport.addFrame();
  if (frameCount == 120){
    gifExport.finish();
    output.flush();
    output.close();
  }
}

class Ball {
  
  float x, y;
  float diameter;
  float vx = randomGaussian();
  float vy = randomGaussian();
  //float vx = 5 + random(2);
  //float vy = 5 + random(2);
  int id;
  PImage myimg;
  Ball[] others;
 
  Ball(PImage img, float xin, float yin, float din, int idin, Ball[] oin) {
    x = xin;
    y = yin;
    myimg = img;
    diameter = din;
    id = idin;
    others = oin;
  } 
  
  void collide() {
    for (int i = id + 1; i < numBalls; i++) {
      float dx = others[i].x - x;
      float dy = others[i].y - y;
      float distance = sqrt(dx*dx + dy*dy);
      float minDist = others[i].diameter/2 + diameter/2;
      if (distance < minDist) { 
        float angle = atan2(dy, dx);
        float targetX = x + cos(angle) * minDist;
        float targetY = y + sin(angle) * minDist;
        float ax = (targetX - others[i].x) * spring;
        float ay = (targetY - others[i].y) * spring;
        vx -= ax;
        vy -= ay;
        others[i].vx += ax;
        others[i].vy += ay;
      }
    }   
  }
  
  void move() {
    //vy += gravity;
    x += vx;
    y += vy;
    if (x + diameter/2 > width) {
      x = width - diameter/2;
      vx *= friction; 
    }
    else if (x - diameter/2 < 0) {
      x = diameter/2;
      vx *= friction;
    }
    if (y + diameter/2 > height) {
      y = height - diameter/2;
      vy *= friction; 
    } 
    else if (y - diameter/2 < 0) {
      y = diameter/2;
      vy *= friction;
    }
  }
  
  void display() {
    image(myimg, x, y);
    //ellipse(x, y, diameter, diameter);
  }
}
