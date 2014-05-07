package puck.gui;

import java.awt.Dimension;
import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;
import java.io.File;

import javax.swing.BoxLayout;
import javax.swing.JButton;
import javax.swing.JFileChooser;
import javax.swing.JFrame;
import javax.swing.JLabel;
import javax.swing.JPanel;

import puck.Puck;

public class SettingsFrame extends JFrame{

	private File selectDirectory(String target){
		JFileChooser jfc = new JFileChooser();	
		jfc.setDialogTitle("Select "+target+" executable");
		jfc.setFileSelectionMode(JFileChooser.FILES_ONLY); 
		jfc.showDialog(null, null);
		return jfc.getSelectedFile();
		
	}
	
	private JLabel swiplPath;
	private JLabel dotPath;
	
	private Puck puck;
	SettingsFrame(Puck puck){
		this.puck = puck;
		
		setTitle("Settings");
        setDefaultCloseOperation(JFrame.DISPOSE_ON_CLOSE);
        setMinimumSize(new Dimension(300, 150));
        
		JButton swib = new JButton("Set swipl path");
		swib.addActionListener(new ActionListener() {
			
			@Override
			public void actionPerformed(ActionEvent arg0) {
				File f = selectDirectory("swipl");
			
				SettingsFrame.this.puck.getPrologHandler().setSwiplPath(f);
				
				swiplPath.setText(f.getAbsolutePath());
			}
		});
		
		String path = "";
		if(puck.getPrologHandler().getSwiplPath() != null)
			path = puck.getPrologHandler().getSwiplPath().getAbsolutePath();
		swiplPath = new JLabel(path);
		
		
		
		JButton dotb = new JButton("Set dot path");
		dotb.addActionListener(new ActionListener() {
			
			@Override
			public void actionPerformed(ActionEvent arg0) {
				File f = selectDirectory("dot");
			
				SettingsFrame.this.puck.getPrologHandler().setDotPath(f);
				
				dotPath.setText(f.getAbsolutePath());
			}
		});
		
		path = "";
		if(puck.getPrologHandler().getDotPath() != null)
			path = puck.getPrologHandler().getDotPath().getAbsolutePath();
		dotPath = new JLabel(path);
		
		
	
		JPanel p = new JPanel();
		p.setLayout(new BoxLayout(p, BoxLayout.Y_AXIS));
		this.add(p);
		JPanel tmpp = new JPanel();
		tmpp.add(swib);tmpp.add(swiplPath);
		p.add(tmpp);
		
		
		tmpp = new JPanel();
		tmpp.add(dotb);tmpp.add(dotPath);
		p.add(tmpp);
		
		
	}
	
	
}
