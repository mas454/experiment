import java.awt.*;
import java.awt.event.*;
import java.util.*;
import javax.swing.*;
import javax.swing.border.*;
public class SlotMachine extends JFrame implements ActionListener {
public static void main(String[] args) {
JFrame frame  = new SlotMachine();
frame.setBounds(0, 0, 640, 480);
frame.setVisible(true);
frame.setDefaultCloseOperation(JFrame.EXIT_ON_CLOSE);
}
private Random random ;
private javax.swing.Timer timer ;
private JPanel labelPanel,buttonPanel ;
private boolean[] isStopLabels ;
private JLabel[] labels ;
private JButton[] buttons ;
private JButton startButton ;
public void actionPerformaed(ActionEvent e) {
if(e.getSource() == timer) {
actionTimer(e);
}else if(e.getSource() == startButton){
actionStartButton(e);
} else {
actionButton(e);
}

}
private void actionTimer(ActionEvent e) {
for(int i = 0; i < labels.length; i++) {
if(isStopLabels[i]) {
continue;
}

labels[i].setText("" + random.nextInt(10));
}
}
private void actionStartButton(ActionEvent e) {
for(int i = 0; i < isStopLabels.length; i++) {
isStopLabels[i] = false;
}
for(int i = 0; i < buttons.length; i++) {
buttons[i].setEnabled(true);
}
random = new Random(System.currentTimeMillis());
timer.start();
startButton.setEnabled(false);
}
private void actionButton(ActionEvent e) {
for(int i = 0; i < buttons.length; i++) {
if(e.getSource() == buttons[i]) {
buttons[i].setEnabled(false);
isStopLabels[i] = true;
}

}
boolean isAllStop  = true;
for(int i = 0; i < isStopLabels.length; i++) {
if(isStopLabels[i] == false) {
isAllStop = false;
break;
}

}
if(isAllStop) {
timer.stop();
startButton.setEnabled(true);
}

}
public SlotMachine() {
super("スロットマシーン");
labelPanel = new JPanel();
buttonPanel = new JPanel();
startButton = new JButton("スタート");
labelPanel.setLayout(new GridLayout(1, 3));
buttonPanel.setLayout(new GridLayout(1, 3));
startButton.addActionListener(this);
isStopLabels = new boolean[3];
labels = new JLabel[3];
buttons = new JButton[3];
for(int i = 0; i < buttons.length; i++) {
labels[i] = new JLabel("0");
labels[i].setFont(new Font(null, Font.PLAIN, 40));
labels[i].setBorder(new BevelBorder(BevelBorder.RAISED));
labels[i].setHorizontalAlignment(SwingConstants.CENTER);
labelPanel.add(labels[i]);
}
for(int i = 0; i < buttons.length; i++) {
buttons[i] = new JButton("停止" + i);
buttons[i].addActionListener(this);
buttons[i].setEnabled(false);
buttonPanel.add(buttons[i]);
}
getContentPane().add(startButton, BorderLayout.NORTH);
getContentPane().add(labelPanel, BorderLayout.CENTER);
getContentPane().add(buttonPanel, BorderLayout.SOUTH);
timer = new javax.swing.Timer(100, this);
}
}
