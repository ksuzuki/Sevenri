package slix.sevenri.gui;

import javax.swing.JPanel;
import java.awt.BorderLayout;
import javax.swing.JLabel;
import java.awt.Font;
import javax.swing.SwingConstants;
import java.awt.Color;
import java.awt.Dimension;
import javax.swing.JSplitPane;
import javax.swing.JScrollPane;
import javax.swing.JList;

public class MainPanel extends JPanel {
	public MainPanel() {
		setLayout(new BorderLayout(0, 0));
		
		lblSevenri = new JLabel("Sevenri-xx.xx.xx");
		lblSevenri.setPreferredSize(new Dimension(109, 48));
		lblSevenri.setForeground(new Color(204, 204, 102));
		lblSevenri.setBackground(Color.BLACK);
		lblSevenri.setOpaque(true);
		lblSevenri.setHorizontalAlignment(SwingConstants.CENTER);
		lblSevenri.setFont(new Font("Times", Font.BOLD, 16));
		add(lblSevenri, BorderLayout.NORTH);
		
		spDivider = new JSplitPane();
		spDivider.setContinuousLayout(true);
		add(spDivider, BorderLayout.CENTER);
		
		JScrollPane spListSn = new JScrollPane();
		spDivider.setLeftComponent(spListSn);
		
		lstSn = new JList();
		spListSn.setViewportView(lstSn);
		
		JScrollPane spListName = new JScrollPane();
		spDivider.setRightComponent(spListName);
		
		lstName = new JList();
		spListName.setViewportView(lstName);
		spDivider.setDividerLocation(122);
	}
	static final long serialVersionUID = 1L;
	private JLabel lblSevenri;
	private JSplitPane spDivider;
	private JList lstSn;
	private JList lstName;

	/**
	 * @param args
	 */
	public static void main(String[] args) {
		javax.swing.JFrame frame = new javax.swing.JFrame();
		frame.getContentPane().add(new MainPanel());
		frame.setSize(320, 200);
		frame.setVisible(true);
	}

	public JLabel getLblSevenri() {
		return lblSevenri;
	}
	public JSplitPane getSpDivider() {
		return spDivider;
	}
	public JList getLstSn() {
		return lstSn;
	}
	public JList getLstName() {
		return lstName;
	}
}
